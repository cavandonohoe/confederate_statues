"""Unit tests for the ingest module's parsing logic (no DB required)."""

from __future__ import annotations

from pathlib import Path

import pytest
from sqlalchemy import select

from statue_api.ingest import StatueRow, parse_csv, upsert_rows
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
    assert upsert_rows(db_session, [revised]) == 1
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
