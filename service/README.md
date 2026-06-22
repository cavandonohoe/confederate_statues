# statue-api

A small REST API over the Confederate statues dataset scraped and
cleaned by the R pipeline in the parent repository. It turns the
static CSV into something any tool can query: a browser, `curl`, a
Python notebook, a JavaScript dashboard, or R itself.

The service is intentionally small but designed the way I'd design
something an on-call team would actually have to run:

- FastAPI app factory with structured JSON logging, request-id
  propagation, Prometheus metrics, and a `/health` probe
- SQLAlchemy 2.0 ORM with Alembic migrations
- Idempotent CSV → Postgres ingest job keyed by a content hash so
  re-runs are no-ops when the upstream data hasn't changed
- pytest suite with in-memory SQLite for unit tests and a Postgres
  service container for integration tests
- Multi-stage Dockerfile, `docker-compose.yml` for local dev, and a
  `fly.toml` for the production deploy
- CI on every PR: `ruff` (lint + format check), `mypy --strict`,
  `pytest --cov`, Docker image build

## Table of contents

- [Quickstart](#quickstart): get the service running locally
- [Using the API](#using-the-api): hit it from `curl`, Python, R, JS
- [Endpoint reference](#endpoint-reference): every URL, every param
- [Architecture](#architecture)
- [Configuration](#configuration)
- [Development](#development)
- [Deploy](#deploy)

## Quickstart

Pick one path: Docker (one command, everything containerized) or
bare-metal Python (faster iteration if you already have Postgres).

### With Docker Compose

```bash
cd service
docker compose up --build               # starts postgres + api
docker compose exec api statue-api ingest --csv /data/confederate_statue_dates.csv
open http://localhost:8000/docs         # interactive API explorer
```

### Bare metal (Python 3.12+, Postgres)

```bash
cd service
python -m venv .venv && source .venv/bin/activate
pip install -e ".[dev]"
export STATUE_API_DATABASE_URL=postgresql+psycopg://postgres:postgres@localhost:5432/statues
alembic upgrade head
statue-api ingest --csv ../data/confederate_statue_dates.csv
statue-api serve --reload
```

### Verify it's up

```bash
curl http://localhost:8000/health
# {"status":"ok","database":"ok","version":"0.1.0"}
```

If `database` is `error`, the service started but couldn't reach
Postgres — check `STATUE_API_DATABASE_URL`.

## Using the API

The friendliest entry point is the auto-generated interactive
documentation, which lives at `/docs`:

- **Swagger UI:** [http://localhost:8000/docs](http://localhost:8000/docs)
- **ReDoc:** [http://localhost:8000/redoc](http://localhost:8000/redoc)
- **Raw OpenAPI 3.1 schema:** [http://localhost:8000/openapi.json](http://localhost:8000/openapi.json)

You can click through every endpoint in Swagger, fill in parameters,
and execute live requests against your local instance without writing
any code.

Below are copy-pasteable examples in four languages. They assume a
local server on port 8000; swap in your deployed URL otherwise.

### `curl`

```bash
# Aggregate counts per decade (great for histograms)
curl -s http://localhost:8000/stats/by-decade

# Filtered list: Georgia entries dedicated between 1900 and 1910
curl -s 'http://localhost:8000/statues?source=georgia&year_min=1900&year_max=1910&limit=10'

# Full-text search across the entry text
curl -s 'http://localhost:8000/statues?q=Robert+E.+Lee&limit=5'

# Single entry by id
curl -s http://localhost:8000/statues/<content-hash-id>

# Pretty-print with jq
curl -s http://localhost:8000/stats/by-state | jq '.items[] | "\(.source): \(.count)"'
```

### Python

```python
import httpx

BASE = "http://localhost:8000"

# Aggregate counts per decade
decades = httpx.get(f"{BASE}/stats/by-decade").json()
for row in decades["items"]:
    print(f"{row['decade_start']}s: {row['count']}")

# Filtered, paginated list
resp = httpx.get(
    f"{BASE}/statues",
    params={
        "source": "north_carolina",
        "year_min": 1900,
        "year_max": 1920,
        "limit": 50,
    },
)
resp.raise_for_status()
data = resp.json()
print(f"Found {data['meta']['total']} matching rows; showing {len(data['items'])}.")
for item in data["items"]:
    print(item["year"], item["entry"][:80])
```

If you'd rather use the requests library or pandas, the JSON shape is
the same:

```python
import pandas as pd
df = pd.DataFrame(httpx.get(f"{BASE}/statues?limit=500").json()["items"])
```

### R

```r
library(httr2)

base_url <- "http://localhost:8000"

decades <- request(base_url) |>
  req_url_path("/stats/by-decade") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE)

# decades$items is now a data frame with decade_start and count columns
print(decades$items)

# Filtered query
statues <- request(base_url) |>
  req_url_path("/statues") |>
  req_url_query(
    source = "alabama",
    year_min = 1900,
    year_max = 1920,
    limit = 50
  ) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE)

# statues$items is a data frame ready for tidyverse / ggplot2
head(statues$items)
```

### JavaScript / TypeScript (browser or Node)

```js
const BASE = "http://localhost:8000";

// Aggregate per decade
const decades = await fetch(`${BASE}/stats/by-decade`).then(r => r.json());
console.table(decades.items);

// Filtered list with paging
const params = new URLSearchParams({
  source: "georgia",
  year_min: "1900",
  year_max: "1920",
  limit: "50",
});
const page = await fetch(`${BASE}/statues?${params}`).then(r => r.json());
console.log(`Total: ${page.meta.total}, showing: ${page.items.length}`);
```

### Tracing a request across logs

Every request returns an `x-request-id` header (generated server-side
if you don't send one). If you supply your own, every log line emitted
while handling that request will include it, which is invaluable when
debugging a specific call:

```bash
curl -i -H 'x-request-id: my-correlation-id-42' http://localhost:8000/health
# HTTP/1.1 200 OK
# x-request-id: my-correlation-id-42
# ...
```

## Endpoint reference

All endpoints return `application/json` and propagate a
`Content-Type: application/json` response header. All filters compose
with AND semantics. Times are UTC ISO-8601.

### `GET /statues`

Paginated, filterable list of entries.

**Query parameters**

| Name       | Type    | Default | Description                                            |
| ---------- | ------- | ------- | ------------------------------------------------------ |
| `source`   | string  | —       | Filter to a single source state slug (`alabama`, `georgia`, `mississippi`, `north_carolina`, `south_carolina`, `other`). |
| `year_min` | integer | —       | Lower bound (inclusive). Must be ≥ 1840.               |
| `year_max` | integer | —       | Upper bound (inclusive). Must be ≤ 2100.               |
| `q`        | string  | —       | Case-insensitive substring match on `entry`. 2-200 chars. |
| `limit`    | integer | 50      | Page size. 1-500.                                      |
| `offset`   | integer | 0       | Rows to skip. ≥ 0.                                     |

**Response**

```json
{
  "items": [
    {
      "id": "0a1b2c...",
      "source": "alabama",
      "entry": "Bullock County (1866) named for...",
      "year": 1866,
      "created_at": "2026-06-22T20:14:01Z",
      "updated_at": "2026-06-22T20:14:01Z"
    }
  ],
  "meta": { "total": 857, "limit": 50, "offset": 0 }
}
```

`meta.total` is the unpaginated count of rows matching the filters,
so a client can render a pager without an extra round-trip.

### `GET /statues/{statue_id}`

Single entry by id. The id is a SHA-256 hex digest of
`(source, entry, year)` — stable across re-ingests of the same row.

**Responses**

- `200`: same shape as a single item in the list response
- `404`: id not found

### `GET /stats/by-decade`

Counts of entries bucketed by decade. The bucket key is the first
year of the decade (e.g. `1900` covers 1900-1909).

```json
{
  "items": [
    { "decade_start": 1840, "count": 1 },
    { "decade_start": 1860, "count": 21 },
    { "decade_start": 1870, "count": 39 }
  ]
}
```

Items are ordered chronologically.

### `GET /stats/by-state`

Counts grouped by source state slug, ordered by count descending.

```json
{
  "items": [
    { "source": "other", "count": 443 },
    { "source": "north_carolina", "count": 125 },
    { "source": "georgia", "count": 96 }
  ]
}
```

### `GET /health`

Liveness + DB connectivity. Always returns `200` so load balancers
treat the service as up; check the `database` field for actual
reachability.

```json
{ "status": "ok", "database": "ok", "version": "0.1.0" }
```

### `GET /metrics`

Prometheus text exposition. Exposes:

- `http_requests_total{method, path, status}` — request counter
- `http_request_duration_seconds{method, path}` — latency histogram
- `http_requests_in_flight` — current in-flight gauge

Path labels use the FastAPI route template (e.g. `/statues/{statue_id}`)
to keep cardinality bounded.

### `GET /docs`, `GET /redoc`, `GET /openapi.json`

Swagger UI, ReDoc, and the raw OpenAPI 3.1 schema, respectively. Use
the latter to generate clients in any language via
[openapi-generator](https://openapi-generator.tech/) or similar.

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

## Configuration

All settings are environment variables prefixed with `STATUE_API_`.
See `src/statue_api/config.py` for the full list. The most important
ones:

| Variable                  | Default                                                          |
| ------------------------- | ---------------------------------------------------------------- |
| `STATUE_API_DATABASE_URL` | `postgresql+psycopg://postgres:postgres@localhost:5432/statues`  |
| `STATUE_API_LOG_LEVEL`    | `INFO`                                                           |
| `STATUE_API_LOG_JSON`     | `true`                                                           |
| `STATUE_API_CORS_ORIGINS` | `["*"]`                                                          |

For local development a `.env` file at `service/.env` is auto-loaded
(see `.env.example`).

## Development

```bash
# Lint + format check
ruff check . && ruff format --check .

# Type check
mypy src

# Unit tests (in-memory SQLite, no external services)
pytest

# Run against a real Postgres
docker compose up -d postgres
STATUE_API_DATABASE_URL=postgresql+psycopg://postgres:postgres@localhost:5432/statues \
  pytest
```

Generating a new Alembic migration after model changes:

```bash
alembic revision --autogenerate -m "describe the change"
alembic upgrade head
```

## Deploy

Production target is Fly.io. To deploy:

```bash
fly launch --copy-config --no-deploy   # first time only
fly secrets set STATUE_API_DATABASE_URL=postgres://...
fly deploy
```

`fly.toml` declares a `release_command` that runs `alembic upgrade head`
before the new code starts serving, so the schema is always consistent
with the running version. The CSV ingest is a separate one-shot command
you run via `fly ssh console -C 'statue-api ingest --csv ...'` or via
a scheduled GitHub Action that targets the deployed instance.
