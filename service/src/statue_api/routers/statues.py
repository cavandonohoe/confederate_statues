"""Statues CRUD-style read endpoints."""

from __future__ import annotations

from fastapi import APIRouter, HTTPException, Query, status
from sqlalchemy import func, select

from statue_api.db import DbSession
from statue_api.models import Statue
from statue_api.schemas import PageMeta, StatueOut, StatuePage

router = APIRouter(prefix="/statues", tags=["statues"])


@router.get("", response_model=StatuePage)
def list_statues(
    db: DbSession,
    source: str | None = Query(default=None, description="Filter by state slug."),
    year_min: int | None = Query(default=None, ge=1840),
    year_max: int | None = Query(default=None, le=2100),
    q: str | None = Query(
        default=None,
        description="Case-insensitive substring match on entry text.",
        min_length=2,
        max_length=200,
    ),
    limit: int = Query(default=50, ge=1, le=500),
    offset: int = Query(default=0, ge=0),
) -> StatuePage:
    """Return a paginated, filterable list of statue entries.

    Filters compose with AND semantics. Pagination is offset-based; the
    response includes a ``meta.total`` for the unpaginated count so clients
    can render proper pagers without an extra request.
    """
    stmt = select(Statue)
    count_stmt = select(func.count()).select_from(Statue)

    if source is not None:
        stmt = stmt.where(Statue.source == source)
        count_stmt = count_stmt.where(Statue.source == source)
    if year_min is not None:
        stmt = stmt.where(Statue.year >= year_min)
        count_stmt = count_stmt.where(Statue.year >= year_min)
    if year_max is not None:
        stmt = stmt.where(Statue.year <= year_max)
        count_stmt = count_stmt.where(Statue.year <= year_max)
    if q is not None:
        pattern = f"%{q}%"
        stmt = stmt.where(Statue.entry.ilike(pattern))
        count_stmt = count_stmt.where(Statue.entry.ilike(pattern))

    total = db.execute(count_stmt).scalar_one()
    stmt = stmt.order_by(Statue.year, Statue.source, Statue.id).limit(limit).offset(offset)
    rows = db.execute(stmt).scalars().all()

    return StatuePage(
        items=[StatueOut.model_validate(r) for r in rows],
        meta=PageMeta(total=total, limit=limit, offset=offset),
    )


@router.get("/{statue_id}", response_model=StatueOut)
def get_statue(statue_id: str, db: DbSession) -> StatueOut:
    """Return a single statue by its content-hash id."""
    obj = db.get(Statue, statue_id)
    if obj is None:
        raise HTTPException(status.HTTP_404_NOT_FOUND, "Statue not found")
    return StatueOut.model_validate(obj)
