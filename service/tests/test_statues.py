"""Endpoint-level tests for the statues router."""

from __future__ import annotations


def test_list_statues_empty(client):
    r = client.get("/statues")
    assert r.status_code == 200
    body = r.json()
    assert body["meta"]["total"] == 0
    assert body["items"] == []


def test_list_statues_returns_all(client, seeded_session):
    r = client.get("/statues")
    assert r.status_code == 200
    body = r.json()
    assert body["meta"]["total"] == 5
    assert len(body["items"]) == 5
    assert all({"id", "source", "entry", "year"} <= set(item) for item in body["items"])


def test_list_statues_filters_by_source(client, seeded_session):
    r = client.get("/statues", params={"source": "alabama"})
    assert r.status_code == 200
    body = r.json()
    assert body["meta"]["total"] == 2
    assert {item["source"] for item in body["items"]} == {"alabama"}


def test_list_statues_filters_by_year_range(client, seeded_session):
    r = client.get("/statues", params={"year_min": 1890, "year_max": 1910})
    assert r.status_code == 200
    body = r.json()
    years = [item["year"] for item in body["items"]]
    assert all(1890 <= y <= 1910 for y in years)
    assert body["meta"]["total"] == 2


def test_list_statues_search(client, seeded_session):
    r = client.get("/statues", params={"q": "Lee"})
    assert r.status_code == 200
    body = r.json()
    assert body["meta"]["total"] == 1
    assert "Lee" in body["items"][0]["entry"]


def test_list_statues_pagination(client, seeded_session):
    r = client.get("/statues", params={"limit": 2, "offset": 0})
    body = r.json()
    assert len(body["items"]) == 2
    assert body["meta"]["total"] == 5
    assert body["meta"]["limit"] == 2
    assert body["meta"]["offset"] == 0


def test_list_statues_validates_query_bounds(client):
    r = client.get("/statues", params={"limit": 0})
    assert r.status_code == 422
    r = client.get("/statues", params={"limit": 9999})
    assert r.status_code == 422
    r = client.get("/statues", params={"year_min": 1700})
    assert r.status_code == 422


def test_get_statue_by_id(client, seeded_session):
    page = client.get("/statues").json()
    first = page["items"][0]
    r = client.get(f"/statues/{first['id']}")
    assert r.status_code == 200
    assert r.json()["id"] == first["id"]


def test_get_statue_not_found(client, seeded_session):
    r = client.get("/statues/" + "0" * 64)
    assert r.status_code == 404
