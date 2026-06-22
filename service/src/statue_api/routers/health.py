"""Health and readiness endpoints."""

from __future__ import annotations

from fastapi import APIRouter
from sqlalchemy import text

from statue_api import __version__
from statue_api.db import DbSession
from statue_api.schemas import HealthOut

router = APIRouter(tags=["meta"])


@router.get("/health", response_model=HealthOut)
def health(db: DbSession) -> HealthOut:
    """Liveness + DB connectivity check.

    Returns ``database="ok"`` if a trivial ``SELECT 1`` succeeds, otherwise
    ``database="error"`` while still responding 200 to keep the endpoint
    cheap for load balancers. Use ``/readyz`` for stricter probes if needed.
    """
    db_status = "ok"
    try:
        db.execute(text("SELECT 1"))
    except Exception:
        db_status = "error"
    return HealthOut(status="ok", database=db_status, version=__version__)
