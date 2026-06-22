"""Tests for the health and meta endpoints."""

from __future__ import annotations


def test_health_ok(client):
    r = client.get("/health")
    assert r.status_code == 200
    body = r.json()
    assert body["status"] == "ok"
    assert body["database"] == "ok"
    assert body["version"]


def test_root_returns_service_info(client):
    r = client.get("/")
    assert r.status_code == 200
    body = r.json()
    assert body["service"] == "statue-api"
    assert body["docs"] == "/docs"


def test_metrics_endpoint_exposes_prometheus(client):
    client.get("/health")
    r = client.get("/metrics")
    assert r.status_code == 200
    assert "text/plain" in r.headers["content-type"]
    assert "http_requests_total" in r.text


def test_request_id_header_round_trip(client):
    r = client.get("/health", headers={"x-request-id": "test-correlation-id"})
    assert r.headers["x-request-id"] == "test-correlation-id"


def test_request_id_generated_when_absent(client):
    r = client.get("/health")
    assert r.headers["x-request-id"]
    assert len(r.headers["x-request-id"]) >= 16
