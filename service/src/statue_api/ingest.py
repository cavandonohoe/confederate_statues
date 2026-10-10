"""CSV → Postgres ingest job.

Reads the upstream ``data/confederate_statue_dates.csv`` produced by the R
pipeline, derives a citation-insensitive content-hash id, and synchronizes the snapshot
into the ``statues`` table. Rows absent from the new snapshot are removed in the same transaction.
Empty or malformed snapshots are rejected before any database writes.
"""

from __future__ import annotations

import csv
import hashlib
import re
from collections.abc import Iterable
from dataclasses import dataclass
from pathlib import Path

from sqlalchemy import delete
from sqlalchemy.dialects.postgresql import insert as pg_insert
from sqlalchemy.dialects.sqlite import insert as sqlite_insert
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
        entry = re.sub(r"\[(?:\d+(?:[ ,\u2013-]\d+)*|[a-z])\]", "", self.entry)
        entry = " ".join(entry.split())
        material = f"{self.source}\x1f{entry}\x1f{self.year}"
        return hashlib.sha256(material.encode("utf-8")).hexdigest()


def parse_csv(path: Path) -> Iterable[StatueRow]:
    """Yield validated rows from the upstream CSV."""
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh, strict=True)
        expected = ["source", "entry", "year"]
        if reader.fieldnames != expected:
            raise ValueError(f"CSV header mismatch. Expected {expected}, got {reader.fieldnames}")
        for line_number, raw in enumerate(reader, start=2):
            if None in raw or any(value is None for value in raw.values()):
                raise ValueError(f"Malformed CSV row at line {line_number}")
            try:
                year_int = int(raw["year"])
            except (TypeError, ValueError):
                raise ValueError(f"Invalid year at CSV line {line_number}") from None
            source = (raw["source"] or "").strip()
            entry = (raw["entry"] or "").strip()
            if not source or not entry:
                raise ValueError(f"Empty field at CSV line {line_number}")
            yield StatueRow(source=source, entry=entry, year=year_int)


def upsert_rows(db: Session, rows: Iterable[StatueRow], batch_size: int = 500) -> int:
    """Atomically synchronize a complete, non-empty snapshot.

    Materialize before writing so parse errors cannot leave partial changes.
    Citation-only edits retain their ID; changed/deleted entries replace the
    previous snapshot rather than accumulating obsolete records.
    """
    if batch_size < 1:
        raise ValueError("batch_size must be positive")
    snapshot = {row.content_hash: row for row in rows}
    if not snapshot:
        raise ValueError("Refusing to ingest an empty snapshot")
    dialect = db.get_bind().dialect.name
    if dialect not in {"postgresql", "sqlite"}:
        raise ValueError(f"Unsupported database dialect: {dialect}")
    insert = pg_insert if dialect == "postgresql" else sqlite_insert
    values = [
        {"id": sid, "source": row.source, "entry": row.entry, "year": row.year}
        for sid, row in snapshot.items()
    ]
    try:
        for offset in range(0, len(values), batch_size):
            stmt = insert(Statue).values(values[offset : offset + batch_size])
            stmt = stmt.on_conflict_do_update(
                index_elements=[Statue.id],
                set_={
                    "source": stmt.excluded.source,
                    "entry": stmt.excluded.entry,
                    "year": stmt.excluded.year,
                },
            )
            db.execute(stmt)
        db.execute(delete(Statue).where(Statue.id.not_in(list(snapshot))))
        db.commit()
    except Exception:
        db.rollback()
        raise
    return len(snapshot)


def run_ingest(csv_path: Path) -> int:
    """End-to-end ingest: parse CSV → upsert → return row count."""
    if not csv_path.exists():
        raise FileNotFoundError(csv_path)
    log.info("ingest_start", path=str(csv_path))
    with SessionLocal() as db:
        n = upsert_rows(db, parse_csv(csv_path))
    log.info("ingest_done", rows=n)
    return n
