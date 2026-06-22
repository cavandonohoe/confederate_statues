"""Tests for the Pydantic settings layer."""

from __future__ import annotations

import os

import pytest

from statue_api.config import Settings, get_settings


def test_settings_defaults(monkeypatch):
    for key in list(os.environ):
        if key.startswith("STATUE_API_"):
            monkeypatch.delenv(key, raising=False)
    get_settings.cache_clear()
    s = Settings(_env_file=None)
    assert s.database_url
    assert s.log_level == "INFO"
    assert s.page_size_default <= s.page_size_max


def test_settings_reads_env(monkeypatch):
    monkeypatch.setenv("STATUE_API_LOG_LEVEL", "DEBUG")
    monkeypatch.setenv("STATUE_API_PAGE_SIZE_DEFAULT", "25")
    s = Settings()
    assert s.log_level == "DEBUG"
    assert s.page_size_default == 25


def test_settings_validates_page_size():
    with pytest.raises(ValueError):
        Settings(page_size_default=0)
    with pytest.raises(ValueError):
        Settings(page_size_max=10_000)
