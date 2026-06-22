"""Aggregate statistics endpoints."""

from __future__ import annotations

from fastapi import APIRouter
from sqlalchemy import func, select

from statue_api.db import DbSession
from statue_api.models import Statue
from statue_api.schemas import DecadeBucket, DecadeStats, StateBucket, StateStats

router = APIRouter(prefix="/stats", tags=["stats"])


@router.get("/by-decade", response_model=DecadeStats)
def by_decade(db: DbSession) -> DecadeStats:
    """Return entry counts bucketed by decade.

    The decade is computed in SQL as ``(year / 10) * 10`` so the database
    does the grouping and only the small bucketed result crosses the wire.
    """
    decade_col = (func.floor(Statue.year / 10) * 10).label("decade")
    stmt = select(decade_col, func.count().label("count")).group_by(decade_col).order_by(decade_col)
    rows = db.execute(stmt).all()
    return DecadeStats(items=[DecadeBucket(decade_start=int(d), count=int(c)) for d, c in rows])


@router.get("/by-state", response_model=StateStats)
def by_state(db: DbSession) -> StateStats:
    """Return entry counts grouped by source state slug."""
    stmt = (
        select(Statue.source, func.count().label("count"))
        .group_by(Statue.source)
        .order_by(func.count().desc())
    )
    rows = db.execute(stmt).all()
    return StateStats(items=[StateBucket(source=s, count=int(c)) for s, c in rows])
