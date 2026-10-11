"""Unit tests for the ingest module's parsing logic (no DB required)."""

from __future__ import annotations

import csv
from pathlib import Path

import pytest
from sqlalchemy import Delete, select

from statue_api import cli, ingest
from statue_api import db as db_module
from statue_api.config import get_settings
from statue_api.ingest import (
    SnapshotShrinkError,
    StatueRow,
    build_snapshot,
    check_shrink,
    normalize_entry,
    parse_csv,
    run_ingest,
    upsert_rows,
)
from statue_api.models import Statue


def _write_csv(path: Path, rows: list[dict[str, str]]) -> None:
    headers = "source,entry,year\n"
    body = "\n".join(f'{r["source"]},"{r["entry"]}",{r["year"]}' for r in rows)
    path.write_text(headers + body + "\n", encoding="utf-8")


def test_content_hash_is_stable():
    a = StatueRow(source="alabama", entry="Lee County", year=1866)
    b = StatueRow(source="alabama", entry="Lee County", year=1866)
    assert a.content_hash == b.content_hash
    assert len(a.content_hash) == 64


def test_content_hash_changes_with_any_field():
    base = StatueRow(source="alabama", entry="Lee County", year=1866)
    assert base.content_hash != StatueRow("alabama", "Lee County", 1867).content_hash
    assert base.content_hash != StatueRow("georgia", "Lee County", 1866).content_hash
    assert base.content_hash != StatueRow("alabama", "Lee Counties", 1866).content_hash


def test_parse_csv_happy_path(tmp_path):
    csv_path = tmp_path / "x.csv"
    _write_csv(
        csv_path,
        [
            {"source": "alabama", "entry": "Bullock County (1866)", "year": "1866"},
            {"source": "georgia", "entry": "Stone Mountain (1915)", "year": "1915"},
        ],
    )
    rows = list(parse_csv(csv_path))
    assert len(rows) == 2
    assert rows[0] == StatueRow("alabama", "Bullock County (1866)", 1866)


def test_parse_csv_rejects_bad_year(tmp_path):
    csv_path = tmp_path / "x.csv"
    _write_csv(
        csv_path,
        [
            {"source": "alabama", "entry": "Good Row", "year": "1866"},
            {"source": "alabama", "entry": "Bad Year", "year": "nineteen-oh-five"},
        ],
    )
    with pytest.raises(ValueError, match="Invalid year"):
        list(parse_csv(csv_path))


def test_parse_csv_rejects_empty_fields(tmp_path):
    csv_path = tmp_path / "x.csv"
    _write_csv(
        csv_path,
        [
            {"source": "", "entry": "No source", "year": "1900"},
            {"source": "alabama", "entry": "", "year": "1900"},
            {"source": "alabama", "entry": "Real Row", "year": "1900"},
        ],
    )
    with pytest.raises(ValueError, match="Empty field"):
        list(parse_csv(csv_path))


def test_parse_csv_rejects_wrong_header(tmp_path):
    csv_path = tmp_path / "x.csv"
    csv_path.write_text("a,b,c\n1,2,3\n", encoding="utf-8")
    with pytest.raises(ValueError, match="CSV header mismatch"):
        list(parse_csv(csv_path))


def test_parse_csv_strips_whitespace(tmp_path):
    csv_path = tmp_path / "x.csv"
    csv_path.write_text(
        'source,entry,year\n  alabama  ,"  My entry  ",1866\n',
        encoding="utf-8",
    )
    rows = list(parse_csv(csv_path))
    assert rows[0].source == "alabama"
    assert rows[0].entry == "My entry"


def test_citation_changes_preserve_id():
    assert StatueRow("georgia", "Monument (1909)[104]", 1909).content_hash == (
        StatueRow("georgia", "Monument (1909)[105]", 1909).content_hash
    )


def test_ingest_syncs_snapshot_and_removes_legacy_ids(db_session):
    db_session.add(Statue(id="legacy", source="georgia", entry="Old [104]", year=1909))
    db_session.commit()
    original = StatueRow("georgia", "Monument (1909)[104]", 1909)
    removed = StatueRow("alabama", "Removed (1910)", 1910)
    assert upsert_rows(db_session, [original, removed], batch_size=1) == 2
    revised = StatueRow("georgia", "Monument (1909)[105]", 1909)
    # Dropping a whole (tiny) source trips the shrink guard; force the deletion.
    assert upsert_rows(db_session, [revised], force=True) == 1
    rows = list(db_session.scalars(select(Statue)))
    assert len(rows) == 1
    assert rows[0].id == original.content_hash
    assert rows[0].entry == revised.entry
    assert upsert_rows(db_session, [revised]) == 1
    assert len(list(db_session.scalars(select(Statue)))) == 1


def test_empty_snapshot_preserves_database(db_session):
    row = StatueRow("georgia", "Monument", 1909)
    upsert_rows(db_session, [row])
    with pytest.raises(ValueError, match="empty snapshot"):
        upsert_rows(db_session, [])
    assert db_session.get(Statue, row.content_hash) is not None


def test_invalid_csv_preserves_database(db_session, tmp_path):
    row = StatueRow("georgia", "Monument", 1909)
    upsert_rows(db_session, [row])
    csv_path = tmp_path / "invalid.csv"
    csv_path.write_text("source,entry,year\nalabama,New,1910\ngeorgia,Bad,oops\n")
    with pytest.raises(ValueError, match="Invalid year"):
        upsert_rows(db_session, parse_csv(csv_path), batch_size=1)
    assert [r.id for r in db_session.scalars(select(Statue))] == [row.content_hash]


def test_duplicate_citation_variants_collapse(db_session):
    rows = [StatueRow("georgia", "Monument[1]", 1909), StatueRow("georgia", "Monument[2]", 1909)]
    assert upsert_rows(db_session, rows) == 1
    assert len(list(db_session.scalars(select(Statue)))) == 1


def test_build_snapshot_counts_collisions():
    rows = [
        StatueRow("georgia", "Monument[1]", 1909),
        StatueRow("georgia", "Monument[2]", 1909),
        StatueRow("georgia", "Monument[citation needed]", 1909),
        StatueRow("georgia", "Other", 1909),
    ]
    snapshot, collisions = build_snapshot(rows)
    assert len(snapshot) == 2
    assert collisions == 2
    # Last row for an ID wins, matching the previous behavior.
    assert snapshot[rows[0].content_hash].entry == "Monument[citation needed]"


@pytest.mark.parametrize(
    ("raw", "normalized"),
    [
        ("Monument (1909)[104]", "Monument (1909)"),
        ("Monument (1909)[1][2]", "Monument (1909)"),
        ("Monument (1909)[1, 2]", "Monument (1909)"),
        ("Monument (1909)[3\u20135]", "Monument (1909)"),
        ("Monument (1909).[a]", "Monument (1909)."),
        ("Monument (1909).[D]", "Monument (1909)."),
        ("Monument (1909)[note 1]", "Monument (1909)"),
        ("Monument (1909)[Note 12]", "Monument (1909)"),
        ("Monument (1909)[nb 1]", "Monument (1909)"),
        ("Monument (1909)[1]:23", "Monument (1909)"),
        ("Monument (1909)[24]:\u200a70 more", "Monument (1909) more"),
        ("Monument (1909)[32]: 33\u201335.", "Monument (1909)."),
        ("Monument (1909)[citation needed]", "Monument (1909)"),
        ("As of 2020[update], there", "As of 2020, there"),
        ("Memorial (1936).[57][dubious \u2013 discuss]", "Memorial (1936)."),
    ],
)
def test_normalize_entry_strips_citation_markers(raw, normalized):
    assert normalize_entry(raw) == normalized


@pytest.mark.parametrize(
    "entry",
    [
        # Real quote edit in the Terry's Texas Rangers row.
        'a monument "to memorialize those [who] fought for the Confederacy"',
        "the inside contains .... [T]he time capsule",
        "fought for conscience sake [sic] and died",
        '"[The monument] describes the United States"',
    ],
)
def test_normalize_entry_keeps_editorial_brackets(entry):
    assert normalize_entry(entry) == entry


def test_spaced_letter_label_is_not_merged_with_bare_name():
    labelled = StatueRow("other", "Company [b] monument (1910)", 1910)
    bare = StatueRow("other", "Company monument (1910)", 1910)
    assert labelled.content_hash != bare.content_hash
    attached = StatueRow("other", "Company monument (1910).[b]", 1910)
    assert (
        attached.content_hash == StatueRow("other", "Company monument (1910).", 1910).content_hash
    )


def test_committed_csv_has_no_id_collisions():
    csv_path = Path(__file__).resolve().parents[2] / "data" / "confederate_statue_dates.csv"
    rows = list(parse_csv(csv_path))
    snapshot, collisions = build_snapshot(rows)
    assert collisions == 0
    assert len(snapshot) == len(rows)


@pytest.mark.parametrize("year", ["1700", "2999"])
def test_parse_csv_rejects_out_of_range_year(tmp_path, year):
    csv_path = tmp_path / "x.csv"
    _write_csv(csv_path, [{"source": "alabama", "entry": "Row", "year": year}])
    with pytest.raises(ValueError, match="out of range"):
        list(parse_csv(csv_path))


def _seed(db_session, counts: dict[str, int]) -> list[StatueRow]:
    rows = [
        StatueRow(source, f"{source} monument {i}", 1900)
        for source, n in counts.items()
        for i in range(n)
    ]
    upsert_rows(db_session, rows)
    return rows


def _stored_count(db_session) -> int:
    return len(list(db_session.scalars(select(Statue))))


@pytest.fixture
def ingest_session(db_session, monkeypatch):
    """Point ``run_ingest`` at the test database with the shrink guard enabled."""
    monkeypatch.setattr(ingest, "SessionLocal", db_module.SessionLocal)
    monkeypatch.setenv("STATUE_API_ALLOW_SHRINK", "false")
    get_settings.cache_clear()
    yield db_session
    get_settings.cache_clear()


def test_truncated_snapshot_is_refused(db_session):
    rows = _seed(db_session, {"alabama": 10, "georgia": 10})
    truncated = rows[:15]  # loses 25% of the total
    with pytest.raises(SnapshotShrinkError, match="total rows would drop from 20 to 15"):
        upsert_rows(db_session, truncated)
    assert _stored_count(db_session) == 20


def test_source_wipeout_is_refused_even_if_total_is_small_drop(db_session):
    rows = _seed(db_session, {"alabama": 2, "georgia": 18})
    without_alabama = [r for r in rows if r.source != "alabama"]
    with pytest.raises(SnapshotShrinkError, match="source 'alabama' would drop from 2 to 0"):
        upsert_rows(db_session, without_alabama)
    assert _stored_count(db_session) == 20


def test_moderate_shrink_is_allowed(db_session):
    rows = _seed(db_session, {"alabama": 10, "georgia": 10})
    assert upsert_rows(db_session, rows[1:19]) == 18  # 10% total, 10% per source
    assert _stored_count(db_session) == 18


def test_force_overrides_shrink_guard(db_session):
    rows = _seed(db_session, {"alabama": 10, "georgia": 10})
    assert upsert_rows(db_session, rows[:2], force=True) == 2
    assert _stored_count(db_session) == 2


def test_check_shrink_thresholds():
    assert check_shrink({}, {"a": 1}) == []
    assert check_shrink({"a": 10}, {"a": 8}) == []
    assert len(check_shrink({"a": 10}, {"a": 7})) == 1
    assert check_shrink({"a": 10}, {"a": 7}, max_total_shrink=0.5) == []
    problems = check_shrink({"a": 50, "b": 50}, {"a": 50, "b": 20})
    assert any("'b'" in p for p in problems)


def test_run_ingest_honors_allow_shrink_setting(ingest_session, tmp_path, monkeypatch):
    db_session = ingest_session
    _seed(db_session, {"alabama": 10})
    csv_path = tmp_path / "small.csv"
    _write_csv(csv_path, [{"source": "alabama", "entry": "Only row", "year": "1900"}])
    with pytest.raises(SnapshotShrinkError):
        run_ingest(csv_path)
    monkeypatch.setenv("STATUE_API_ALLOW_SHRINK", "true")
    get_settings.cache_clear()
    assert run_ingest(csv_path) == 1
    assert _stored_count(db_session) == 1


def test_cli_ingest_refuses_then_forces(ingest_session, tmp_path, capsys):
    db_session = ingest_session
    _seed(db_session, {"alabama": 10})
    csv_path = tmp_path / "small.csv"
    _write_csv(csv_path, [{"source": "alabama", "entry": "Only row", "year": "1900"}])
    assert cli.main(["ingest", "--csv", str(csv_path)]) == 1
    assert "Ingest refused" in capsys.readouterr().err
    assert _stored_count(db_session) == 10
    assert cli.main(["ingest", "--csv", str(csv_path), "--force"]) == 0
    assert _stored_count(db_session) == 1


@pytest.mark.parametrize("header", ["source,entry,year,year", "year,entry,source"])
def test_parse_csv_rejects_noncanonical_header(tmp_path, header):
    path = tmp_path / "headers.csv"
    path.write_text(header + "\n")
    with pytest.raises(ValueError, match="CSV header mismatch"):
        list(parse_csv(path))


def test_unclosed_quote_preserves_database(db_session, tmp_path):
    row = StatueRow("georgia", "Existing monument", 1909)
    upsert_rows(db_session, [row])
    path = tmp_path / "quotes.csv"
    path.write_text('source,entry,year\nalabama,New,"1910')
    with pytest.raises(csv.Error):
        upsert_rows(db_session, parse_csv(path))
    assert [r.id for r in db_session.scalars(select(Statue))] == [row.content_hash]


def test_write_failure_rolls_back_snapshot(db_session, monkeypatch):
    original = StatueRow("georgia", "Existing monument", 1909)
    upsert_rows(db_session, [original])
    execute = db_session.execute

    def fail_during_delete(stmt, *args, **kwargs):
        if isinstance(stmt, Delete):
            raise RuntimeError("simulated write failure")
        return execute(stmt, *args, **kwargs)

    with monkeypatch.context() as patch:
        patch.setattr(db_session, "execute", fail_during_delete)
        with pytest.raises(RuntimeError, match="simulated write failure"):
            upsert_rows(db_session, [StatueRow("alabama", "New monument", 1910)], force=True)
    assert [r.id for r in db_session.scalars(select(Statue))] == [original.content_hash]
