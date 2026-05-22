#!/usr/bin/env bash
# Rasterize the report's SVG figures to PNG (for markdown viewers that don't
# render referenced SVGs). Requires Google Chrome. Run from anywhere.
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/reports" && pwd)"
CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
if [ ! -x "$CHROME" ]; then
	echo "Google Chrome not found at $CHROME; skipping PNG render." >&2
	exit 0
fi
for svg in "$DIR"/*.svg; do
	base="$(basename "$svg" .svg)"
	"$CHROME" --headless --disable-gpu --no-sandbox \
		--screenshot="$DIR/$base.png" --window-size=720,420 \
		--default-background-color=FFFFFFFF "file://$svg" 2>/dev/null
	echo "rendered $base.png"
done
