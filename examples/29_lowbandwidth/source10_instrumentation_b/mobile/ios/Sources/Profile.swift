// Profile.swift — Verbindungsprofil (Jump + Ziel, Ed25519-Keys), Speicherung.
//
// Hosts/Ports/User in UserDefaults, private Keys im Schlüsselbund (Keychain,
// nur dieses Gerät), Hostkey-Pins (TOFU) in UserDefaults je Hop.

import Foundation
import Security

struct Hop: Codable, Equatable {
    var host = ""
    var port = 22
    var user = ""
    var spec: String { "\(user)@\(host):\(port)" }
}

struct Profile: Codable, Equatable {
    var useJump = true
    var jump = Hop()
    var target = Hop()
    /// Ziel-sshd aus Sicht des Jumps (z. B. 127.0.0.1:2332 bei autossh -R).
    var targetSshdHost = "127.0.0.1"
    var targetSshdPort = 22
    var lbwPort = 7878

    var hops: [Hop] { useJump ? [jump, target] : [target] }

    static func load() -> Profile {
        guard let d = UserDefaults.standard.data(forKey: "profile"),
              let p = try? JSONDecoder().decode(Profile.self, from: d) else { return Profile() }
        return p
    }

    func save() {
        if let d = try? JSONEncoder().encode(self) { UserDefaults.standard.set(d, forKey: "profile") }
    }

    /// TOFU-Pin-Schlüssel je Hop (Host + Port + User).
    static func pinKey(_ h: Hop) -> String { "pin:\(h.spec)" }

    /// EngineCfg-JSON (mobile/core/src/01_engine.rs). Pins positional, "" = lernen.
    func engineJson(keys: [String]) -> String {
        let pins = hops.map { UserDefaults.standard.string(forKey: Profile.pinKey($0)) ?? "" }
        let cfg: [String: Any] = [
            "hops": hops.map(\.spec),
            "identities": keys,
            "pins": pins,
            "target_sshd_host": useJump ? targetSshdHost : target.host,
            "target_sshd_port": useJump ? targetSshdPort : target.port,
            "lbw_port": lbwPort,
        ]
        let d = (try? JSONSerialization.data(withJSONObject: cfg)) ?? Data()
        return String(decoding: d, as: UTF8.self)
    }

    /// Gelernte Pins speichern (nur leere Plätze — ein Wechsel bricht die Kette ab).
    func learn(pins: [String]) {
        for (h, p) in zip(hops, pins) where !p.isEmpty {
            let k = Profile.pinKey(h)
            if UserDefaults.standard.string(forKey: k) == nil { UserDefaults.standard.set(p, forKey: k) }
        }
    }

    func forgetPins() { hops.forEach { UserDefaults.standard.removeObject(forKey: Profile.pinKey($0)) } }
}

/// Private Keys (OpenSSH-Text) im Schlüsselbund.
enum Keychain {
    private static func query(_ account: String) -> [String: Any] {
        [kSecClass as String: kSecClassGenericPassword,
         kSecAttrService as String: "de.lbw.client10b",
         kSecAttrAccount as String: account]
    }

    static func set(_ value: String, for account: String) {
        SecItemDelete(query(account) as CFDictionary)
        guard !value.isEmpty else { return }
        var q = query(account)
        q[kSecValueData as String] = Data(value.utf8)
        q[kSecAttrAccessible as String] = kSecAttrAccessibleWhenUnlockedThisDeviceOnly
        SecItemAdd(q as CFDictionary, nil)
    }

    static func get(_ account: String) -> String {
        var q = query(account)
        q[kSecReturnData as String] = true
        var out: AnyObject?
        guard SecItemCopyMatching(q as CFDictionary, &out) == errSecSuccess, let d = out as? Data else { return "" }
        return String(decoding: d, as: UTF8.self)
    }
}
