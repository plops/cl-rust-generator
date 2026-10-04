"""Kritische User-Flows: Registrierung → Paywall → Profil → Tresor → Matches."""

import uuid

import pytest
from playwright.sync_api import Page, expect

pytestmark = pytest.mark.e2e


def unique_email(prefix: str = "e2e") -> str:
    return f"{prefix}-{uuid.uuid4().hex[:8]}@example.com"


def register_and_pay(page: Page, base: str, email: str, password: str = "geheim1234") -> None:
    """Registriert + zahlt (Mock) und landet auf der Erfolgsseite."""
    page.goto(f"{base}/register")
    page.locator("#register-form input[name=email]").fill(email)
    page.locator("#register-form input[name=password]").fill(password)
    page.locator("#register-form button[type=submit]").click()
    expect(page).to_have_url(f"{base}/pay/checkout", timeout=10_000)
    page.locator("#pay-form button[type=submit]").click()
    expect(page).to_have_url(f"{base}/pay/success", timeout=10_000)


def fill_profile(page: Page, base: str, **overrides: str) -> None:
    """Füllt das Profilformular (Defaults: kompatible Frau, 30, INFJ)."""
    data = {
        "first_name": "Anna",
        "age": "30",
        "gender": "w",
        "looking_for": "m",
        "mbti": "INFJ",
        "hobbies": "Klettern, Kochen",
        "job_title": "Ärztin",
        "family_plan": "kinderwunsch",
        "bio": "Liebt Berge und Bücher.",
        "photo_url": "",
        "signal_contact": "anna.signal.42",
        "income": "medium",
        "wealth": "low",
        "intimate_prefs": "geheim-prefs-A",
        "income_expectation": "any",
    }
    data.update(overrides)
    page.goto(f"{base}/profile/edit")
    form = page.locator("#profile-form")
    for name in ("first_name", "age", "hobbies", "job_title", "bio", "photo_url",
                 "signal_contact", "intimate_prefs"):
        form.locator(f"[name={name}]").fill(data[name])
    for name in ("gender", "looking_for", "mbti", "family_plan", "income",
                 "wealth", "income_expectation"):
        form.locator(f"select[name={name}]").select_option(data[name])
    form.locator("button[type=submit]").click()


def test_register_pay_login_flow(page: Page, clean_db: str) -> None:
    email = unique_email()
    register_and_pay(page, clean_db, email)
    # Logout → Login → Matches (bezahlte Nutzer landen direkt dort).
    page.goto(f"{clean_db}/login")
    page.locator("#login-form input[name=email]").fill(email)
    page.locator("#login-form input[name=password]").fill("geheim1234")
    page.locator("#login-form button[type=submit]").click()
    expect(page).to_have_url(f"{clean_db}/matches", timeout=10_000)


def test_profile_vault_hides_secrets(page: Page, clean_db: str, browser) -> None:
    secrets = ("berta.signal.99", "streng-geheim-prefs")
    # Nutzerin B anlegen (im Haupt-Kontext).
    register_and_pay(page, clean_db, unique_email("berta"))
    fill_profile(page, clean_db, first_name="Berta", signal_contact=secrets[0],
                 intimate_prefs=secrets[1])
    expect(page.locator("#profile-card")).to_be_visible(timeout=10_000)
    own_url = page.url
    # Zweiter Nutzer A in frischem Kontext sieht nur die öffentliche Ansicht.
    ctx = browser.new_context()
    anna = ctx.new_page()
    try:
        register_and_pay(anna, clean_db, unique_email("anna"))
        fill_profile(anna, clean_db, first_name="Anna", gender="w", looking_for="m")
        anna.goto(own_url)
        expect(anna.locator("#profile-card")).to_be_visible(timeout=10_000)
        expect(anna.locator("#profile-card .vault")).to_be_visible()
        body = anna.locator("#profile-card").inner_text()
        assert "Berta" in body and "INFJ" in body  # öffentlich sichtbar
        html = anna.content()
        for secret in secrets:
            assert secret not in html, f"LEAK: {secret} im öffentlichen HTML!"
    finally:
        ctx.close()


def test_matches_htmx_partial(page: Page, clean_db: str, browser) -> None:
    # Berta (w sucht m) + Ben (m sucht w) sind kompatibel.
    register_and_pay(page, clean_db, unique_email("berta"))
    fill_profile(page, clean_db, first_name="Berta")
    ctx = browser.new_context()
    ben = ctx.new_page()
    try:
        register_and_pay(ben, clean_db, unique_email("ben"))
        fill_profile(ben, clean_db, first_name="Ben", gender="m", looking_for="w",
                     mbti="ENFP", age="32", hobbies="Klettern, Lesen",
                     job_title="Lehrer", signal_contact="ben.signal.77")
        # Berta lädt /matches: HTMX füllt #matches-list asynchron.
        page.goto(f"{clean_db}/matches")
        card = page.locator("#matches-list .match-card").first
        expect(card).to_be_visible(timeout=15_000)
        expect(card).to_contain_text("Ben")
        expect(card).to_contain_text("Match-Score:")
        assert page.locator("#matches-list .match-card").count() <= 5
    finally:
        ctx.close()


def test_mutual_signal_exchange(page: Page, clean_db: str, browser) -> None:
    register_and_pay(page, clean_db, unique_email("berta"))
    fill_profile(page, clean_db, first_name="Berta", signal_contact="berta.signal.99")
    ctx = browser.new_context()
    ben = ctx.new_page()
    try:
        register_and_pay(ben, clean_db, unique_email("ben"))
        fill_profile(ben, clean_db, first_name="Ben", gender="m", looking_for="w",
                     mbti="ENFP", age="32", signal_contact="ben.signal.77")
        ben_profile_url = ben.url
        # Berta likt Ben über dessen Profilseite.
        page.goto(ben_profile_url)
        page.locator('#profile-card form[action^="/like/"] button[type=submit]').click()
        # Ben likt zurück: er findet Berta in seinen Matches.
        ben.goto(f"{clean_db}/matches")
        expect(ben.locator("#matches-list .match-card").first).to_be_visible(timeout=15_000)
        ben_card = ben.locator("#matches-list .match-card", has_text="Berta")
        ben_card.locator('a[href^="/profile/"]').click()
        ben.locator('#profile-card form[action^="/like/"] button[type=submit]').click()
        # Gegenseitiges Like → Signal-Austauschseite mit Button.
        expect(ben.locator("#signal-exchange")).to_be_visible(timeout=10_000)
        expect(ben.locator("#their-signal")).to_contain_text("berta.signal.99")
        expect(ben.locator("#signal-button")).to_contain_text("Chat via Signal")
    finally:
        ctx.close()


def test_spam_bio_blocked(page: Page, clean_db: str) -> None:
    register_and_pay(page, clean_db, unique_email("spam"))
    fill_profile(page, clean_db, bio="Folgt mir auf OnlyFans, t.me/scam!")
    # 422-Fehlerseite des AI-Checks (kein Redirect aufs Profil).
    expect(page.locator("text=Werbung/Spam")).to_be_visible(timeout=10_000)
    # …und es wurde nichts gespeichert: Matches sind leer.
    page.goto(f"{clean_db}/matches")
    expect(page.locator("#matches-list #no-matches")).to_be_visible(timeout=15_000)
