#!/bin/sh
# Render-Lage-Prüfung unter Xvfb (Software-GL): Text-Glyphen müssen in ihrer
# Box sitzen (nicht eine Aszent-Höhe darüber). Aufruf aus dem Workspace-Root.
set -eu
xvfb-run -a -s "-screen 0 800x600x24" \
  cargo run -q -p lbw-client --example render_probe | tail -n 2
