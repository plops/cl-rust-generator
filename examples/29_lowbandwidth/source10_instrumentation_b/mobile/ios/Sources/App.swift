// App.swift — Einstieg: Verbindungsformular → entfernter Bildschirm.

import SwiftUI

@main
struct LbwApp: App {
    var body: some Scene {
        WindowGroup { RootView() }
    }
}

struct RootView: View {
    @State private var profile = Profile.load()
    @State private var jumpKey = Keychain.get("jump")
    @State private var targetKey = Keychain.get("target")
    @State private var core: LbwCore?
    @State private var status = ""
    @State private var keyboard = false
    private let timer = Timer.publish(every: 1, on: .main, in: .common).autoconnect()

    var body: some View {
        if let core {
            VStack(spacing: 0) {
                ScreenView(core: core, keyboard: $keyboard) { profile.learn(pins: core.pins) }
                HStack {
                    Text(status).font(.caption2).lineLimit(1)
                    Spacer()
                    ForEach(["Esc", "Tab", "Enter"], id: \.self) { k in
                        Button(k) { core.key(k) }.font(.caption)
                    }
                    Button { keyboard.toggle() } label: { Image(systemName: "keyboard") }
                    Button("Trennen") { self.core = nil; keyboard = false }
                }
                .padding(6)
            }
            .ignoresSafeArea(.container, edges: .top)
            .onReceive(timer) { _ in status = core.status }
        } else {
            form
        }
    }

    private var form: some View {
        NavigationStack {
            Form {
                Section("Jump-Host") {
                    Toggle("Über Jump-Host", isOn: $profile.useJump)
                    if profile.useJump { hopFields($profile.jump); keyField($jumpKey) }
                }
                Section("Ziel (lbw-server)") {
                    hopFields($profile.target)
                    keyField($targetKey)
                    if profile.useJump {
                        TextField("Ziel-sshd aus Jump-Sicht", text: $profile.targetSshdHost)
                        TextField("Port", value: $profile.targetSshdPort, format: .number.grouping(.never))
                    }
                    TextField("LBW-Port", value: $profile.lbwPort, format: .number.grouping(.never))
                }
                Section {
                    Button("Verbinden", action: connect)
                    Button("Hostkey-Pins vergessen", role: .destructive) { profile.forgetPins() }
                    if !status.isEmpty { Text(status).font(.footnote).foregroundStyle(.red) }
                }
            }
            .navigationTitle("lbw-client")
            .textInputAutocapitalization(.never)
            .autocorrectionDisabled()
        }
    }

    private func hopFields(_ h: Binding<Hop>) -> some View {
        Group {
            TextField("Host/IP", text: h.host)
            TextField("Port", value: h.port, format: .number.grouping(.never))
            TextField("User", text: h.user)
        }
    }

    private func keyField(_ k: Binding<String>) -> some View {
        TextField("Privater Key (OpenSSH, z. B. Ed25519)", text: k, axis: .vertical)
            .font(.system(.caption2, design: .monospaced))
            .lineLimit(3...6)
    }

    private func connect() {
        profile.save()
        Keychain.set(jumpKey, for: "jump")
        Keychain.set(targetKey, for: "target")
        let keys = profile.useJump ? [jumpKey, targetKey] : [targetKey]
        if let c = LbwCore(json: profile.engineJson(keys: keys)) {
            status = ""
            core = c
        } else {
            status = "Konfiguration ungültig (Host/Port/Key prüfen)"
        }
    }
}
