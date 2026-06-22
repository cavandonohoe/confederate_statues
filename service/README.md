# statue-api

A production-style REST API over the Confederate statues dataset
scraped and cleaned by the R pipeline in the parent repository.

This service is intentionally small but designed the way I'd design
something an on-call team would actually have to run:

- FastAPI app factory with structured JSON logging, request-id
  propagation, Prometheus metrics, and a `/health` probe
- SQLAlchemy 2.0 ORM with Alembic migrations
- Idempotent CSV → Postgres ingest job keyed by a content hash so
  re-runs are no-ops when the upstream data hasn't changed
- pytest suite with in-memory SQLite for unit tests and a separate
  Postgres marker for upsert integration tests
- Multi-stage Dockerfile, `docker-compose.yml` for local dev, and a
  `fly.toml` for the production deploy
- CI on every PR: `ruff` (lint + format check), `mypy --strict`,
  `pytest --cov`

## Architecture

```
┌────────────────────────┐   monthly cron via GitHub Actions
│   R pipeline (R/, ...) │ ────────────────────────────────────┐
│  scrape → tidy → CSV   │                                     │
└────────────────────────┘                                     │
            │                                                  ▼
            │  data/confederate_statue_dates.csv     ┌──────────────────────┐
            └──────────────────────────────────────► │ statue-api ingest    │
                                                     │ (idempotent upsert)  │
                                                     └──────────┬───────────┘
                                                                │
                                                                ▼
                                                       ┌─────────────────┐
                                                       │ Postgres        │
                                                       │ (statues table) │
                                                       └────────┬────────┘
                                                                │
                                                                ▼
                                                       ┌─────────────────┐
                                                       │ FastAPI service │
                                                       │  /statues       │
                                                       │  /stats/*       │
                                                       │  /health        │
                                                       │  /metrics       │
                                                       └─────────────────┘
```

## Quickstart

### Local (docker compose)

```bash
cd service
docker compose up --build
# in another shell:
docker compose exec api statue-api ingest --csv ../data/confederate_statue_dates.csv
open http://localhost:8000/docs
```

### Local (bare metal)

Requires Python 3.12+ and a Postgres you can connect to.

```bash
cd service
python -m venv .venv && source .venv/bin/activate
pip install -e ".[dev]"
export STATUE_API_DATABASE_URL=postgresql+psycopg://postgres:postgres@localhost:5432/statues
alembic upgrade head
statue-api ingest --csv ../data/confederate_statue_dates.csv
statue-api serve --reload
```

## API

| Endpoint              | Description                                              |
| --------------------- | -------------------------------------------------------- |
| `GET /statues`        | Paginated list with `source`, `year_min/max`, `q` filters |
| `GET /statues/{id}`   | Single entry by content-hash id                          |
| `GET /stats/by-decade`| Count of entries bucketed by decade                      |
| `GET /stats/by-state` | Count of entries grouped by source state                 |
| `GET /health`         | Liveness + DB connectivity                               |
| `GET /metrics`        | Prometheus text exposition                               |
| `GET /docs`           | Swagger UI                                               |

All endpoints return JSON; every request gets an `x-request-id`
header propagated from the client or generated server-side, and that
id is bound into every log line emitted while the request is in
flight.

## Configuration

All settings are environment variables prefixed with `STATUE_API_`.
See `src/statue_api/config.py` for the full list. The most important
ones:

| Variable                       | Default                                                                 |
| ------------------------------ | ----------------------------------------------------------------------- |
| `STATUE_API_DATABASE_URL`      | `postgresql+psycopg://postgres:postgres@localhost:5432/statues`         |
| `STATUE_API_LOG_LEVEL`         | `INFO`                                                                  |
| `STATUE_API_LOG_JSON`          | `true`                                                                  |
| `STATUE_API_CORS_ORIGINS`      | `["*"]`                                                                 |

## Development

```bash
# Lint + format check
ruff check . && ruff format --check .

# Type check
mypy src

# Unit tests (in-memory SQLite)
pytest

# Integration tests (require a real Postgres)
docker compose up -d postgres
STATUE_API_DATABASE_URL=postgresql+psycopg://postgres:postgres@localhost:5432/statues \
  pytest -m postgres
```

## Deploy

Production runs on Fly.io. To deploy:

```bash
fly launch --copy-config --no-deploy   # first time only
fly secrets set STATUE_API_DATABASE_URL=postgres://...
fly deploy
```

`fly.toml` declares two processes: the web server and an `ingest`
release command that runs `alembic upgrade head` followed by the CSV
ingest on every deploy.
