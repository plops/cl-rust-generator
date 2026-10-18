# Deployment: sshd-Pflicht + CGNAT per Reverse-Tunnel (T6)

`lbw-server` bindet nur `127.0.0.1:7878` und verlangt zwei Schichten:
SSH (Transport) + LBW-Token (Challenge/Auth gegen lokale Fremdprozesse).

## Server-Host (hier: hpz6o)

1. OpenSSH-Server mit Ed25519-Hostkey, Key-only, Forwarding an:
   `AllowTcpForwarding yes`, `PasswordAuthentication no`.
2. LBW-Token erzeugen und als Datei (0600) ablegen, z. B.
   `/etc/lbw/token`:
   `head -c 32 /dev/urandom | base64 > /etc/lbw/token; chmod 600 …`
3. Server starten: `lbw-server --token-file /etc/lbw/token`.
4. Token einmalig in die Client-Profile übernehmen (nie per Mail/Chat).

## CGNAT-Variante (hpz6o hinter Carrier-Grade-NAT, Jump: tinyeu)

`hpz6o` hält selbst einen Reverse-Tunnel zu `tinyeu`, der seinen sshd
dort als `127.0.0.1:2332` bereitstellt. Beispiel als systemd-Unit
(`autossh` hält die Verbindung samt Healthcheck am Leben):

```ini
# /etc/systemd/system/lbw-reverse.service
[Unit]
Description=lbw Reverse-Tunnel hpz6o -> tinyeu
After=network-online.target
Wants=network-online.target

[Service]
User=kiel
ExecStart=/usr/bin/autossh -M 0 -N \
  -o ServerAliveInterval=15 -o ServerAliveCountMax=3 \
  -o ExitOnForwardFailure=yes \
  -i /home/kiel/.ssh/tinyeu_key.pem \
  -R 2332:127.0.0.1:22 kiel@51.107.16.14
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

Clients verbinden dann exakt wie in Plan §2.1: Session zu `tinyeu` →
`direct-tcpip` auf `127.0.0.1:2332` (= sshd von `hpz6o`) → genestete
Session zu `hpz6o` (`~/.ssh/id_lenovo_to_hpz6o`) → Forward auf
`127.0.0.1:7878`. Der Jump sieht nur SSH-Chiffrat (Ziel-Hostkey wird
Ende-zu-Ende geprüft).
