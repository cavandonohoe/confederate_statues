"""Tests for the /stats endpoints."""

from __future__ import annotations


def test_by_decade_empty(client):
    r = client.get("/stats/by-decade")
    assert r.status_code == 200
    assert r.json() == {"items": []}


def test_by_decade(client, seeded_session):
    r = client.get("/stats/by-decade")
    assert r.status_code == 200
    items = r.json()["items"]
    by_decade = {row["decade_start"]: row["count"] for row in items}
    assert by_decade == {1860: 2, 1890: 1, 1900: 1, 1910: 1}
    decades = [row["decade_start"] for row in items]
    assert decades == sorted(decades)


def test_by_state(client, seeded_session):
    r = client.get("/stats/by-state")
    assert r.status_code == 200
    items = r.json()["items"]
    counts = {row["source"]: row["count"] for row in items}
    assert counts == {"alabama": 2, "georgia": 2, "virginia": 1}
    assert items[0]["count"] >= items[-1]["count"]
