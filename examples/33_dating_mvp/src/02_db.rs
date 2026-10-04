//! 02_db: Postgres-Pool, Migrationen und AEAD-Verschlüsselungshelfer.
//!
//! Sensible Felder (Einkommen, Vermögen, intime Präferenzen, Signal-Kontakt)
//! werden applikationsseitig mit ChaCha20-Poly1305 verschlüsselt. Pro Wert wird
//! eine frische 96-Bit-Nonce gewürfelt; gespeichert wird
//! `Base64(nonce || ciphertext)`.

use base64::{Engine as _, engine::general_purpose::STANDARD as B64};
use chacha20poly1305::{
    ChaCha20Poly1305, Key, Nonce,
    aead::{Aead, KeyInit},
};
use sqlx::PgPool;
use sqlx::postgres::PgPoolOptions;
use thiserror::Error;

const NONCE_LEN: usize = 12;

#[derive(Clone)]
pub struct CryptoKey(pub [u8; 32]);

impl CryptoKey {
    pub fn new(bytes: [u8; 32]) -> Self {
        Self(bytes)
    }
}

#[derive(Debug, Error)]
pub enum CryptoError {
    #[error("encryption failed")]
    Encrypt,
    #[error("decryption failed (wrong key or tampered data)")]
    Decrypt,
    #[error("invalid payload encoding")]
    Encoding,
}

fn cipher(key: &[u8; 32]) -> Result<ChaCha20Poly1305, CryptoError> {
    let array = Key::try_from(key.as_slice()).map_err(|_| CryptoError::Encrypt)?;
    Ok(ChaCha20Poly1305::new(&array))
}

/// Verschlüsselt `plaintext` und gibt `Base64(nonce || ciphertext)` zurück.
pub fn encrypt_field(key: &CryptoKey, plaintext: &str) -> Result<String, CryptoError> {
    let nonce_bytes: [u8; NONCE_LEN] = rand::random();
    let nonce = Nonce::try_from(nonce_bytes.as_slice()).map_err(|_| CryptoError::Encrypt)?;
    let ciphertext = cipher(&key.0)?
        .encrypt(&nonce, plaintext.as_bytes())
        .map_err(|_| CryptoError::Encrypt)?;
    let mut payload = Vec::with_capacity(NONCE_LEN + ciphertext.len());
    payload.extend_from_slice(&nonce_bytes);
    payload.extend_from_slice(&ciphertext);
    Ok(B64.encode(payload))
}

/// Entschlüsselt ein `Base64(nonce || ciphertext)`-Payload.
pub fn decrypt_field(key: &CryptoKey, payload_b64: &str) -> Result<String, CryptoError> {
    let payload = B64
        .decode(payload_b64.trim())
        .map_err(|_| CryptoError::Encoding)?;
    if payload.len() < NONCE_LEN + 1 {
        return Err(CryptoError::Encoding);
    }
    let (nonce_bytes, ciphertext) = payload.split_at(NONCE_LEN);
    let nonce = Nonce::try_from(nonce_bytes).map_err(|_| CryptoError::Encoding)?;
    let bytes = cipher(&key.0)
        .map_err(|_| CryptoError::Decrypt)?
        .decrypt(&nonce, ciphertext)
        .map_err(|_| CryptoError::Decrypt)?;
    String::from_utf8(bytes).map_err(|_| CryptoError::Decrypt)
}

pub async fn create_pool(database_url: &str) -> Result<PgPool, sqlx::Error> {
    PgPoolOptions::new()
        .max_connections(5)
        .connect(database_url)
        .await
}

pub async fn run_migrations(pool: &PgPool) -> Result<(), sqlx::migrate::MigrateError> {
    sqlx::migrate!("./migrations").run(pool).await
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_key() -> CryptoKey {
        CryptoKey([42u8; 32])
    }

    #[test]
    fn encrypt_decrypt_roundtrip() {
        let key = test_key();
        for text in ["", "hallo", "70000-90000", "🚀🔒 unicode-preferences"] {
            let enc = encrypt_field(&key, text).expect("encrypts");
            assert_ne!(enc, text, "ciphertext must differ from plaintext");
            let dec = decrypt_field(&key, &enc).expect("decrypts");
            assert_eq!(dec, text);
        }
    }

    #[test]
    fn same_plaintext_yields_different_ciphertexts() {
        let key = test_key();
        let a = encrypt_field(&key, "low").unwrap();
        let b = encrypt_field(&key, "low").unwrap();
        assert_ne!(a, b, "fresh nonce per value required");
    }

    #[test]
    fn tampered_payload_fails() {
        let key = test_key();
        let mut raw = B64.decode(encrypt_field(&key, "secret").unwrap()).unwrap();
        let last = raw.len() - 1;
        raw[last] ^= 0x01;
        let err = decrypt_field(&key, &B64.encode(raw)).unwrap_err();
        assert!(matches!(err, CryptoError::Decrypt));
    }

    #[test]
    fn wrong_key_fails() {
        let enc = encrypt_field(&test_key(), "secret").unwrap();
        let err = decrypt_field(&CryptoKey([9u8; 32]), &enc).unwrap_err();
        assert!(matches!(err, CryptoError::Decrypt));
    }

    #[test]
    fn garbage_payload_errors_as_encoding() {
        let key = test_key();
        assert!(matches!(
            decrypt_field(&key, "!!!").unwrap_err(),
            CryptoError::Encoding
        ));
        assert!(matches!(
            decrypt_field(&key, &B64.encode([1u8; 4])).unwrap_err(),
            CryptoError::Encoding
        ));
    }

    /// DB-Test: läuft nur mit `TEST_DATABASE_URL`, sonst Skip (E2E deckt DB ab).
    #[tokio::test]
    async fn migrations_run_on_test_db() {
        let Some(url) = std::env::var("TEST_DATABASE_URL").ok() else {
            eprintln!("skipping migrations test: TEST_DATABASE_URL not set");
            return;
        };
        let pool = create_pool(&url).await.expect("connect test db");
        run_migrations(&pool).await.expect("migrations run");
        let tables: Vec<String> =
            sqlx::query_scalar("SELECT tablename FROM pg_tables WHERE schemaname = 'public'")
                .fetch_all(&pool)
                .await
                .expect("list tables");
        for expected in ["users", "profiles", "likes", "daily_matches"] {
            assert!(
                tables.iter().any(|t| t == expected),
                "table {expected} exists"
            );
        }
    }
}
