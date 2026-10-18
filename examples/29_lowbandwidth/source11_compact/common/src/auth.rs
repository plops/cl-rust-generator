//! `auth` — Token-Nachweis per Challenge/Response (HMAC-SHA256).
//!
//! Das Token liegt als Datei (0600) auf dem Server und im Client-Profil;
//! es erscheint nie auf der Leitung. Verglichen wird konstantzeitig.

use hmac::{Hmac, Mac};
use sha2::Sha256;

use crate::types::AUTH_DOMAIN;

/// Berechnet den Antwort-MAC: HMAC-SHA256(Token, `nonce || "lbw11"`).
#[must_use]
pub fn auth_mac(token: &[u8], nonce: &[u8; 16]) -> [u8; 32] {
    let mut mac = Hmac::<Sha256>::new_from_slice(token).expect("HMAC nimmt jeden Key");
    mac.update(&nonce[..]);
    mac.update(AUTH_DOMAIN);
    mac.finalize().into_bytes().into()
}

/// Prüft einen Client-MAC konstantzeitig (`verify_slice`).
#[must_use]
pub fn verify_auth(token: &[u8], nonce: &[u8; 16], mac: &[u8; 32]) -> bool {
    let mut v = Hmac::<Sha256>::new_from_slice(token).expect("HMAC nimmt jeden Key");
    v.update(&nonce[..]);
    v.update(AUTH_DOMAIN);
    v.verify_slice(mac).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn correct_mac_verifies_wrong_does_not() {
        let nonce = *b"0123456789abcdef";
        let mac = auth_mac(b"geheim", &nonce);
        assert!(verify_auth(b"geheim", &nonce, &mac));
        assert!(!verify_auth(b"falsch", &nonce, &mac));
        let mut bad = mac;
        bad[0] ^= 1;
        assert!(!verify_auth(b"geheim", &nonce, &bad));
    }

    #[test]
    fn nonce_changes_mac() {
        let a = auth_mac(b"t", b"0123456789abcdef");
        let b = auth_mac(b"t", b"fedcba9876543210");
        assert_ne!(a, b);
    }
}
