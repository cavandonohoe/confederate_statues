"""Prometheus metrics: request counter, request latency, in-flight gauge.

Exposed via the ``/metrics`` endpoint mounted from ``main.py``.
"""

from __future__ import annotations

import time
from collections.abc import Awaitable, Callable

from fastapi import Request, Response
from prometheus_client import (
    CONTENT_TYPE_LATEST,
    CollectorRegistry,
    Counter,
    Gauge,
    Histogram,
    generate_latest,
)
from starlette.middleware.base import BaseHTTPMiddleware

registry = CollectorRegistry()

REQUEST_COUNT = Counter(
    "http_requests_total",
    "Total HTTP requests.",
    ["method", "path", "status"],
    registry=registry,
)
REQUEST_LATENCY = Histogram(
    "http_request_duration_seconds",
    "HTTP request latency in seconds.",
    ["method", "path"],
    registry=registry,
)
IN_FLIGHT = Gauge(
    "http_requests_in_flight",
    "In-flight HTTP requests.",
    registry=registry,
)


class PrometheusMiddleware(BaseHTTPMiddleware):
    """Record request count, latency, and in-flight gauge for every request.

    Uses the route template (e.g. ``/statues/{statue_id}``) rather than the
    raw path so cardinality stays bounded.
    """

    async def dispatch(
        self,
        request: Request,
        call_next: Callable[[Request], Awaitable[Response]],
    ) -> Response:
        IN_FLIGHT.inc()
        start = time.perf_counter()
        try:
            response = await call_next(request)
        except Exception:
            IN_FLIGHT.dec()
            REQUEST_COUNT.labels(request.method, request.url.path, "500").inc()
            raise

        route = request.scope.get("route")
        path_label = route.path if route is not None else request.url.path

        elapsed = time.perf_counter() - start
        REQUEST_LATENCY.labels(request.method, path_label).observe(elapsed)
        REQUEST_COUNT.labels(request.method, path_label, str(response.status_code)).inc()
        IN_FLIGHT.dec()
        return response


def metrics_response() -> Response:
    """Return the Prometheus text exposition for the ``/metrics`` endpoint."""
    return Response(content=generate_latest(registry), media_type=CONTENT_TYPE_LATEST)
