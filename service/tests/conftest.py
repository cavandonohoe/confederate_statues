"""Shared pytest fixtures.

The fast unit-test path uses an in-process SQLite database so the suite
can run without external services. The Postgres-specific upsert behavior
is covered by a separate marker that the CI workflow runs against a
``postgres`` service container.
"""

from __future__ import annotations

import os
from collections.abc import Generator
from pathlib import Path

import pytest
from fastapi.testclient import TestClient
from sqlalchemy import create_engine
from sqlalchemy.orm import Session, sessionmaker
from sqlalchemy.pool import StaticPool

os.environ.setdefault("STATUE_API_DATABASE_URL", "sqlite:///:memory:")
os.environ.setdefault("STATUE_API_LOG_JSON", "false")
os.environ.setdefault("STATUE_API_LOG_LEVEL", "WARNING")

from statue_api import db as db_module
from statue_api.main import create_app
from statue_api.models import Base, Statue


@pytest.fixture(scope="session")
def engine():  # type: ignore[no-untyped-def]
    eng = create_engine(
        "sqlite:///:memory:",
        future=True,
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
    )
    Base.metadata.create_all(eng)
    return eng


@pytest.fixture
def db_session(engine, monkeypatch) -> Generator[Session, None, None]:  # type: ignore[no-untyped-def]
    """Per-test session backed by the in-memory SQLite engine.

    We monkeypatch the app's ``SessionLocal`` and ``engine`` so dependency
    injection inside FastAPI picks up the same database the fixture builds.
    A ``StaticPool`` ensures the same in-memory connection is reused across
    the test, the app, and any background activity within a single test.
    """
    test_session_local = sessionmaker(bind=engine, autoflush=False, autocommit=False, future=True)
    monkeypatch.setattr(db_module, "engine", engine)
    monkeypatch.setattr(db_module, "SessionLocal", test_session_local)

    session = test_session_local()
    try:
        yield session
    finally:
        session.rollback()
        session.query(Statue).delete()
        session.commit()
        session.close()


@pytest.fixture
def client(db_session) -> TestClient:  # type: ignore[no-untyped-def]
    app = create_app()
    return TestClient(app)


@pytest.fixture
def sample_rows() -> list[dict[str, object]]:
    return [
        {"source": "alabama", "entry": "Bullock County (1866)", "year": 1866},
        {"source": "alabama", "entry": "Lee County (1866)", "year": 1866},
        {"source": "georgia", "entry": "Stone Mountain memorial (1915)", "year": 1915},
        {"source": "georgia", "entry": "Confederate Avenue (1907)", "year": 1907},
        {"source": "virginia", "entry": "Monument Avenue (1890)", "year": 1890},
    ]


@pytest.fixture
def seeded_session(db_session, sample_rows):  # type: ignore[no-untyped-def]
    import hashlib

    for r in sample_rows:
        material = f"{r['source']}\x1f{r['entry']}\x1f{r['year']}"
        sid = hashlib.sha256(material.encode("utf-8")).hexdigest()
        db_session.add(Statue(id=sid, source=r["source"], entry=r["entry"], year=r["year"]))
    db_session.commit()
    return db_session


@pytest.fixture
def fixtures_dir() -> Path:
    return Path(__file__).parent / "fixtures"
