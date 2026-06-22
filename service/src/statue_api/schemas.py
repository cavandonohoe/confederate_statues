"""Pydantic schemas for API request/response validation."""

from __future__ import annotations

from datetime import datetime

from pydantic import BaseModel, ConfigDict, Field


class StatueOut(BaseModel):
    """A single statue/entity as returned by the API."""

    model_config = ConfigDict(from_attributes=True)

    id: str
    source: str = Field(description="State slug the entry was scraped from.")
    entry: str = Field(description="Raw descriptive text including the entity name.")
    year: int = Field(description="Year of dedication or naming.", ge=1840)
    created_at: datetime
    updated_at: datetime


class PageMeta(BaseModel):
    """Cursor metadata for paginated list responses."""

    total: int = Field(description="Total rows matching the query.")
    limit: int
    offset: int


class StatuePage(BaseModel):
    """Paginated list of statues."""

    items: list[StatueOut]
    meta: PageMeta


class DecadeBucket(BaseModel):
    """Count of entries falling in a single decade."""

    decade_start: int = Field(description="First year of the decade (e.g. 1900).")
    count: int


class StateBucket(BaseModel):
    """Count of entries for a single source state."""

    source: str
    count: int


class DecadeStats(BaseModel):
    items: list[DecadeBucket]


class StateStats(BaseModel):
    items: list[StateBucket]


class HealthOut(BaseModel):
    status: str
    database: str
    version: str
