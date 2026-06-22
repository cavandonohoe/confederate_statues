"""Unit tests for the ingest module's parsing logic (no DB required)."""

from __future__ import annotations

from pathlib import Path

import pytest

from statue_api.ingest import StatueRow, parse_csv


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


def test_parse_csv_skips_bad_year(tmp_path):
    csv_path = tmp_path / "x.csv"
    _write_csv(
        csv_path,
        [
            {"source": "alabama", "entry": "Good Row", "year": "1866"},
            {"source": "alabama", "entry": "Bad Year", "year": "nineteen-oh-five"},
        ],
    )
    rows = list(parse_csv(csv_path))
    assert len(rows) == 1
    assert rows[0].entry == "Good Row"


def test_parse_csv_skips_empty_fields(tmp_path):
    csv_path = tmp_path / "x.csv"
    _write_csv(
        csv_path,
        [
            {"source": "", "entry": "No source", "year": "1900"},
            {"source": "alabama", "entry": "", "year": "1900"},
            {"source": "alabama", "entry": "Real Row", "year": "1900"},
        ],
    )
    rows = list(parse_csv(csv_path))
    assert len(rows) == 1
    assert rows[0].entry == "Real Row"


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
