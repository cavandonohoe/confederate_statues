"""SQLAlchemy ORM models for the statues dataset."""

from __future__ import annotations

from datetime import datetime

from sqlalchemy import DateTime, Index, Integer, String, Text, func
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column


class Base(DeclarativeBase):
    """Declarative base for all ORM models."""


class Statue(Base):
    """A single Confederate-named entity (statue, place name, school, etc).

    Schema mirrors the upstream CSV produced by the R scraping pipeline:

    - ``source``: state slug (e.g. ``alabama``); origin of the row.
    - ``entry``: free-text description that includes the entity name and
      surrounding context exactly as scraped from Wikipedia.
    - ``year``: dedication / naming year, filtered upstream to 1840..present.

    A SHA-256 hash of ``(source, entry, year)`` is stored as the primary key
    so the ingest job can do idempotent upserts without surrogate-key churn.
    """

    __tablename__ = "statues"

    id: Mapped[str] = mapped_column(String(64), primary_key=True)
    source: Mapped[str] = mapped_column(String(64), nullable=False, index=True)
    entry: Mapped[str] = mapped_column(Text, nullable=False)
    year: Mapped[int] = mapped_column(Integer, nullable=False, index=True)

    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        server_default=func.now(),
        nullable=False,
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        server_default=func.now(),
        onupdate=func.now(),
        nullable=False,
    )

    __table_args__ = (Index("ix_statues_source_year", "source", "year"),)

    def __repr__(self) -> str:
        return f"Statue(id={self.id!r}, source={self.source!r}, year={self.year})"
