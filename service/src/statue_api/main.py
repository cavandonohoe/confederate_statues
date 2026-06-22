"""FastAPI application factory and ASGI entrypoint."""

from __future__ import annotations

import time
import uuid
from collections.abc import AsyncIterator, Awaitable, Callable
from contextlib import asynccontextmanager

import structlog
from fastapi import FastAPI, Request, Response
from fastapi.middleware.cors import CORSMiddleware

from statue_api import __version__
from statue_api.config import get_settings
from statue_api.logging_config import configure_logging, get_logger
from statue_api.metrics import PrometheusMiddleware, metrics_response
from statue_api.routers import health, stats, statues


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncIterator[None]:
    configure_logging()
    get_logger("statue_api").info("startup", version=__version__)
    yield
    get_logger("statue_api").info("shutdown")


def create_app() -> FastAPI:
    """Build a configured FastAPI app. Factory pattern for easy testing."""
    settings = get_settings()
    app = FastAPI(
        title="Statue Tracker API",
        description=(
            "REST API over the Confederate statues dataset scraped and "
            "cleaned by the sibling R pipeline in this repository."
        ),
        version=__version__,
        lifespan=lifespan,
    )

    app.add_middleware(
        CORSMiddleware,
        allow_origins=settings.cors_origins,
        allow_credentials=False,
        allow_methods=["GET"],
        allow_headers=["*"],
    )
    app.add_middleware(PrometheusMiddleware)

    @app.middleware("http")
    async def access_log(
        request: Request,
        call_next: Callable[[Request], Awaitable[Response]],
    ) -> Response:
        request_id = request.headers.get("x-request-id", uuid.uuid4().hex)
        structlog.contextvars.clear_contextvars()
        structlog.contextvars.bind_contextvars(
            request_id=request_id,
            method=request.method,
            path=request.url.path,
        )
        start = time.perf_counter()
        try:
            response = await call_next(request)
        except Exception:
            get_logger("statue_api").exception("request_failed")
            raise
        elapsed_ms = (time.perf_counter() - start) * 1000
        get_logger("statue_api").info(
            "request",
            status=response.status_code,
            duration_ms=round(elapsed_ms, 2),
        )
        response.headers["x-request-id"] = request_id
        return response

    app.include_router(health.router)
    app.include_router(statues.router)
    app.include_router(stats.router)

    @app.get("/metrics", include_in_schema=False)
    def metrics() -> Response:
        return metrics_response()

    @app.get("/", include_in_schema=False)
    def root() -> dict[str, str]:
        return {
            "service": "statue-api",
            "version": __version__,
            "docs": "/docs",
        }

    return app


app = create_app()
