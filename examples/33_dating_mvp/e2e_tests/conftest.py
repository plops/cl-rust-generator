"""E2E-Fixtures: Test-DB + Axum-Server als Hintergrundprozess.

Der Server läuft auf Port 3101 mit separater Test-Datenbank, damit die
Dev-DB (Port 3000) unangetastet bleibt. Vor jedem Test werden die Tabellen
geleert (Isolation), der Server selbst läuft einmal pro Session.
"""

import os
import subprocess
import time
import urllib.request
from pathlib import Path

import psycopg
import pytest

PROJECT_ROOT = Path(__file__).resolve().parent.parent
BASE_URL = os.environ.get("E2E_BASE_URL", "http://127.0.0.1:3101")
# Fixer Test-Key (32 Bytes 0..31, base64) — nur für die Test-DB.
TEST_ENCRYPTION_KEY = "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8="


def _database_url() -> str:
    if "E2E_DATABASE_URL" in os.environ:
        return os.environ["E2E_DATABASE_URL"]
    base = os.environ.get(
        "DATABASE_URL", "postgres://dating:dating_secret_dev@localhost:5432/dating_mvp"
    )
    # DB-Namen gegen Test-DB tauschen (query-String bleibt erhalten).
    head, _, _tail = base.rpartition("/")
    db, _, query = _tail.partition("?")
    assert db, f"unexpected DATABASE_URL: {base}"
    url = f"{head}/dating_mvp_test"
    return f"{url}?{query}" if query else url


def _ensure_database(url: str) -> None:
    """Legt die Test-DB an (falls fehlend) — via Maintenance-DB `postgres`."""
    head, _, tail = url.rpartition("/")
    maint = f"{head}/postgres"
    if "?" in tail:
        _, _, query = tail.partition("?")
        maint = f"{maint}?{query}"
    try:
        with psycopg.connect(maint, autocommit=True) as conn:
            with conn.cursor() as cur:
                cur.execute("CREATE DATABASE dating_mvp_test")
    except psycopg.errors.DuplicateDatabase:
        pass  # existiert bereits


def _truncate(url: str) -> None:
    with psycopg.connect(url, autocommit=True) as conn:
        with conn.cursor() as cur:
            cur.execute("TRUNCATE users CASCADE")


def _wait_for_health(url: str, timeout: float = 90.0) -> None:
    deadline = time.time() + timeout
    last = None
    while time.time() < deadline:
        try:
            with urllib.request.urlopen(f"{url}/health", timeout=3) as resp:
                if resp.status == 200:
                    return
        except Exception as exc:  # noqa: BLE001 — Startphase, weiter pollen
            last = exc
        time.sleep(1.0)
    raise RuntimeError(f"server at {url} not ready: {last}")


@pytest.fixture(scope="session")
def server():
    """Startet den Axum-Server einmal pro Session und stoppt ihn danach."""
    db_url = _database_url()
    _ensure_database(db_url)
    env = {
        **os.environ,
        "DATABASE_URL": db_url,
        "DATA_ENCRYPTION_KEY": os.environ.get("E2E_ENCRYPTION_KEY", TEST_ENCRYPTION_KEY),
        "PORT": "3101",
        "SESSION_SECRET": "e2e-test-secret",
        "RUST_LOG": "warn",
        "AI_MODE": "mock",
        "MATCH_JOB_INTERVAL_HOURS": "24",
        "RATE_LIMIT_PER_SECOND": "1000",
        "RATE_LIMIT_BURST": "1000",
    }
    proc = subprocess.Popen(
        ["cargo", "run", "--quiet"],
        cwd=PROJECT_ROOT,
        env=env,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    try:
        _wait_for_health(BASE_URL)
        _truncate(db_url)
        yield BASE_URL
    finally:
        proc.terminate()
        try:
            proc.wait(timeout=15)
        except subprocess.TimeoutExpired:
            proc.kill()


@pytest.fixture()
def clean_db(server):
    """Leere DB vor jedem Test (Server-URL als Rückgabe)."""
    _truncate(_database_url())
    return server
