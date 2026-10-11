"""CSV → Postgres ingest job.

Reads the upstream ``data/confederate_statue_dates.csv`` produced by the R
pipeline, derives a citation-insensitive content-hash id, and synchronizes the snapshot
into the ``statues`` table. Rows absent from the new snapshot are removed in the same transaction.
Empty or malformed snapshots are rejected before any database writes, and so are
snapshots that would shrink the stored dataset beyond the shrink-guard thresholds
(unless explicitly forced).
"""

from __future__ import annotations

import csv
import hashlib
import re
from collections import Counter
from collections.abc import Iterable, Mapping
from dataclasses import dataclass
from datetime import date
from pathlib import Path

from sqlalchemy import delete, func, select
from sqlalchemy.dialects.postgresql import insert as pg_insert
from sqlalchemy.dialects.sqlite import insert as sqlite_insert
from sqlalchemy.orm import Session

from statue_api.config import get_settings
from statue_api.db import SessionLocal
from statue_api.logging_config import get_logger
from statue_api.models import Statue

log = get_logger("statue_api.ingest")


# Valid installation-year window. Mirrors STATUE_YEAR_MIN / statue_year_max()
# in R/clean.R so the service rejects the same rows the R validator does.
YEAR_MIN = 1840


def year_max() -> int:
    return date.today().year


# Default shrink-guard thresholds. A complete snapshot that drops more than
# MAX_TOTAL_SHRINK of the stored rows, or more than MAX_SOURCE_SHRINK of any
# single source's rows, is almost always a truncated or broken parse.
MAX_TOTAL_SHRINK = 0.2
MAX_SOURCE_SHRINK = 0.5

_RANGE_SEP = r"\s*[,\u2013\u2014-]\s*"
_PAGE_SUFFIX = rf"(?::\s*\d+(?:{_RANGE_SEP}\d+)*)?"
# Wikipedia inline maintenance templates. Matched exactly (case-insensitive)
# so editorial brackets inside quotations, e.g. "those [who] fought", survive.
_MAINTENANCE_TAGS = (
    "according to whom?",
    "better source needed",
    "by whom?",
    "citation needed",
    "clarification needed",
    "dead link",
    "dubious \u2013 discuss",
    "dubious - discuss",
    "failed verification",
    "full citation needed",
    "needs update",
    "original research?",
    "page needed",
    "permanent dead link",
    "update",
    "verification needed",
    "when?",
    "which?",
    "who?",
)
_CITATION_RE = re.compile(
    "|".join(
        [
            # Numbered references, lists, and ranges: [1], [1, 2], [3-5],
            # optionally followed by a page suffix such as [1]:23.
            rf"\[\d+(?:{_RANGE_SEP}\d+)*\]{_PAGE_SUFFIX}",
            # Named footnote groups: [note 1], [nb 1], [n 2].
            rf"\[(?:note|nb|n)\s*\d+\]{_PAGE_SUFFIX}",
            # Single-letter notes [a] / [A], only when attached to the
            # preceding text and not continuing a word. Keeps editorial
            # brackets such as "[T]he" and spaced labels such as "Company [b]".
            r"(?<=\S)\[[A-Za-z]\](?![A-Za-z0-9])",
            # Maintenance tags: [citation needed], [update], ...
            r"\[(?:" + "|".join(re.escape(tag) for tag in _MAINTENANCE_TAGS) + r")\]",
        ]
    ),
    flags=re.IGNORECASE,
)


def normalize_entry(entry: str) -> str:
    """Strip citation and maintenance markers that should not affect identity."""
    return " ".join(_CITATION_RE.sub("", entry).split())


@dataclass(frozen=True, slots=True)
class StatueRow:
    source: str
    entry: str
    year: int

    @property
    def content_hash(self) -> str:
        material = f"{self.source}\x1f{normalize_entry(self.entry)}\x1f{self.year}"
        return hashlib.sha256(material.encode("utf-8")).hexdigest()


class SnapshotShrinkError(ValueError):
    """Raised when a snapshot would delete too much of the stored dataset."""


def build_snapshot(rows: Iterable[StatueRow]) -> tuple[dict[str, StatueRow], int]:
    """Key rows by content hash and count rows that collapsed onto an earlier ID.

    The last row for an ID wins (matching the previous dict-comprehension
    behavior); every collision is logged so silent merges are visible.
    """
    snapshot: dict[str, StatueRow] = {}
    collisions = 0
    for row in rows:
        sid = row.content_hash
        if sid in snapshot:
            collisions += 1
            log.warning(
                "ingest_duplicate_id",
                id=sid,
                source=row.source,
                replaced_entry=snapshot[sid].entry,
                entry=row.entry,
            )
        snapshot[sid] = row
    return snapshot, collisions


def check_shrink(
    current: Mapping[str, int],
    incoming: Mapping[str, int],
    max_total_shrink: float = MAX_TOTAL_SHRINK,
    max_source_shrink: float = MAX_SOURCE_SHRINK,
) -> list[str]:
    """Return human-readable reasons the incoming snapshot shrinks too much.

    ``current`` and ``incoming`` map source -> row count. An empty list means
    the snapshot is acceptable.
    """
    problems: list[str] = []
    current_total = sum(current.values())
    incoming_total = sum(incoming.values())
    if current_total and incoming_total < current_total * (1 - max_total_shrink):
        problems.append(
            f"total rows would drop from {current_total} to {incoming_total} "
            f"(more than {max_total_shrink:.0%})"
        )
    for source, n_current in sorted(current.items()):
        n_incoming = incoming.get(source, 0)
        if n_current and n_incoming < n_current * (1 - max_source_shrink):
            problems.append(
                f"source {source!r} would drop from {n_current} to {n_incoming} "
                f"(more than {max_source_shrink:.0%})"
            )
    return problems


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
            if not YEAR_MIN <= year_int <= year_max():
                raise ValueError(
                    f"Year {year_int} out of range [{YEAR_MIN}, {year_max()}] "
                    f"at CSV line {line_number}"
                )
            source = (raw["source"] or "").strip()
            entry = (raw["entry"] or "").strip()
            if not source or not entry:
                raise ValueError(f"Empty field at CSV line {line_number}")
            yield StatueRow(source=source, entry=entry, year=year_int)


def upsert_rows(
    db: Session,
    rows: Iterable[StatueRow],
    batch_size: int = 500,
    *,
    force: bool = False,
    max_total_shrink: float = MAX_TOTAL_SHRINK,
    max_source_shrink: float = MAX_SOURCE_SHRINK,
) -> int:
    """Atomically synchronize a complete, non-empty snapshot.

    Materialize before writing so parse errors cannot leave partial changes.
    Citation-only edits retain their ID; changed/deleted entries replace the
    previous snapshot rather than accumulating obsolete records.

    Because absent rows are deleted, a truncated but well-formed snapshot
    would wipe most of the dataset. Unless ``force`` is set, the snapshot is
    refused (before any writes) when it shrinks the stored total by more than
    ``max_total_shrink`` or any stored source by more than
    ``max_source_shrink``.
    """
    if batch_size < 1:
        raise ValueError("batch_size must be positive")
    snapshot, collisions = build_snapshot(rows)
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
        current = {
            source: int(n)
            for source, n in db.execute(
                select(Statue.source, func.count()).group_by(Statue.source)
            ).all()
        }
        incoming = Counter(row.source for row in snapshot.values())
        problems = check_shrink(current, incoming, max_total_shrink, max_source_shrink)
        if problems and not force:
            raise SnapshotShrinkError(
                "Refusing to ingest a snapshot that shrinks the dataset: "
                + "; ".join(problems)
                + ". Re-run with --force (or STATUE_API_ALLOW_SHRINK=true) if intended."
            )
        if problems:
            log.warning("ingest_shrink_forced", problems=problems)
        if collisions:
            log.warning("ingest_duplicate_ids", collisions=collisions)
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


def run_ingest(csv_path: Path, *, force: bool | None = None) -> int:
    """End-to-end ingest: parse CSV → upsert → return row count.

    ``force`` overrides the shrink guard; when ``None`` it falls back to the
    ``STATUE_API_ALLOW_SHRINK`` setting.
    """
    if not csv_path.exists():
        raise FileNotFoundError(csv_path)
    if force is None:
        force = get_settings().allow_shrink
    log.info("ingest_start", path=str(csv_path), force=force)
    with SessionLocal() as db:
        n = upsert_rows(db, parse_csv(csv_path), force=force)
    log.info("ingest_done", rows=n)
    return n
