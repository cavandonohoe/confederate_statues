"""CSV → Postgres ingest job.

Reads the upstream ``data/confederate_statue_dates.csv`` produced by the R
pipeline, derives a stable content-hash id, and does an idempotent upsert
into the ``statues`` table. Safe to run on a schedule (no churn when the
CSV hasn't changed; targeted updates when rows change).
"""

from __future__ import annotations

import csv
import hashlib
from collections.abc import Iterable
from dataclasses import dataclass
from pathlib import Path

from sqlalchemy.dialects.postgresql import insert as pg_insert
from sqlalchemy.orm import Session

from statue_api.db import SessionLocal
from statue_api.logging_config import get_logger
from statue_api.models import Statue

log = get_logger("statue_api.ingest")


@dataclass(frozen=True, slots=True)
class StatueRow:
    source: str
    entry: str
    year: int

    @property
    def content_hash(self) -> str:
        material = f"{self.source}\x1f{self.entry}\x1f{self.year}"
        return hashlib.sha256(material.encode("utf-8")).hexdigest()


def parse_csv(path: Path) -> Iterable[StatueRow]:
    """Yield validated rows from the upstream CSV."""
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        expected = {"source", "entry", "year"}
        if reader.fieldnames is None or set(reader.fieldnames) != expected:
            raise ValueError(
                f"CSV header mismatch. Expected {sorted(expected)}, "
                f"got {sorted(reader.fieldnames or [])}"
            )
        for raw in reader:
            try:
                year_int = int(raw["year"])
            except (TypeError, ValueError):
                log.warning("skip_row_bad_year", year=raw.get("year"))
                continue
            source = (raw["source"] or "").strip()
            entry = (raw["entry"] or "").strip()
            if not source or not entry:
                log.warning("skip_row_empty_field", source=source, entry_len=len(entry))
                continue
            yield StatueRow(source=source, entry=entry, year=year_int)


def upsert_rows(db: Session, rows: Iterable[StatueRow], batch_size: int = 500) -> int:
    """Upsert rows in batches. Returns the number of rows processed."""
    batch: list[dict[str, object]] = []
    processed = 0

    def _flush() -> None:
        nonlocal batch
        if not batch:
            return
        stmt = pg_insert(Statue).values(batch)
        stmt = stmt.on_conflict_do_update(
            index_elements=[Statue.id],
            set_={
                "source": stmt.excluded.source,
                "entry": stmt.excluded.entry,
                "year": stmt.excluded.year,
            },
        )
        db.execute(stmt)
        batch = []

    for r in rows:
        batch.append(
            {
                "id": r.content_hash,
                "source": r.source,
                "entry": r.entry,
                "year": r.year,
            }
        )
        processed += 1
        if len(batch) >= batch_size:
            _flush()
    _flush()
    db.commit()
    return processed


def run_ingest(csv_path: Path) -> int:
    """End-to-end ingest: parse CSV → upsert → return row count."""
    if not csv_path.exists():
        raise FileNotFoundError(csv_path)
    log.info("ingest_start", path=str(csv_path))
    with SessionLocal() as db:
        n = upsert_rows(db, parse_csv(csv_path))
    log.info("ingest_done", rows=n)
    return n
