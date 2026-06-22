"""Runtime configuration loaded from environment variables.

All settings are validated via pydantic-settings. ``Settings()`` reads
from environment variables prefixed with ``STATUE_API_``; the ``Config``
class below also wires in a ``.env`` file for local development.
"""

from __future__ import annotations

from functools import lru_cache

from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    model_config = SettingsConfigDict(
        env_prefix="STATUE_API_",
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore",
    )

    database_url: str = Field(
        default="postgresql+psycopg://postgres:postgres@localhost:5432/statues",
        description="SQLAlchemy database URL.",
    )
    log_level: str = Field(default="INFO", description="Python logging level.")
    log_json: bool = Field(
        default=True,
        description="Emit logs as JSON. Set false for human-readable local dev.",
    )
    cors_origins: list[str] = Field(
        default_factory=lambda: ["*"],
        description="CORS allow-origins list.",
    )
    page_size_default: int = Field(default=50, ge=1, le=500)
    page_size_max: int = Field(default=500, ge=1, le=1000)


@lru_cache
def get_settings() -> Settings:
    """Return process-wide settings.

    Cached so importing modules don't re-parse environment variables on
    every call. Tests can override by calling ``get_settings.cache_clear()``.
    """
    return Settings()
