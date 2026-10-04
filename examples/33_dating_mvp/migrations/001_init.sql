-- Anti-Tinder MVP: Initiales Schema.
-- IDs werden in der Applikation erzeugt (Uuid::new_v4), daher kein pgcrypto nötig.

CREATE TABLE IF NOT EXISTS users (
    id UUID PRIMARY KEY,
    email TEXT NOT NULL UNIQUE,
    password_hash TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    paid BOOLEAN NOT NULL DEFAULT FALSE,
    stripe_session_id TEXT
);

CREATE TABLE IF NOT EXISTS profiles (
    user_id UUID PRIMARY KEY REFERENCES users (id) ON DELETE CASCADE,
    first_name TEXT NOT NULL,
    age INT NOT NULL,
    gender TEXT NOT NULL,
    looking_for TEXT NOT NULL,
    mbti TEXT NOT NULL,
    hobbies TEXT[] NOT NULL DEFAULT '{}',
    job_title TEXT NOT NULL DEFAULT '',
    family_plan TEXT NOT NULL,
    bio TEXT NOT NULL DEFAULT '',
    photo_url TEXT NOT NULL DEFAULT '',
    -- Versteckte, applikationsseitig verschlüsselte Felder (Base64 Nonce|Ciphertext):
    signal_contact_enc TEXT NOT NULL DEFAULT '',
    income_enc TEXT NOT NULL DEFAULT '',
    wealth_enc TEXT NOT NULL DEFAULT '',
    intimate_prefs_enc TEXT NOT NULL DEFAULT '',
    -- Grobe Erwartungsstufe, bewusst NICHT sensitiv:
    income_expectation TEXT NOT NULL DEFAULT 'any',
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_profiles_mbti ON profiles (mbti);

CREATE TABLE IF NOT EXISTS likes (
    liker_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    liked_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    PRIMARY KEY (liker_id, liked_id),
    CHECK (liker_id <> liked_id)
);

CREATE TABLE IF NOT EXISTS daily_matches (
    user_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    candidate_id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    match_day DATE NOT NULL,
    score INT NOT NULL,
    rank INT NOT NULL,
    PRIMARY KEY (user_id, match_day, rank)
);

CREATE INDEX IF NOT EXISTS idx_daily_matches_user_day
    ON daily_matches (user_id, match_day);
