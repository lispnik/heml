#!/bin/sh
# scripts/demo/encode.sh NAME -- a Cocoa demo's frames, build/demo/NAME-frames/,
# into build/demo/heml-NAME.mp4, at the rate they came (NAME-rate.txt), with the demo's captions
# (build/demo/NAME-captions/list.txt: FROM TO FILE a line) in a band below
# the editor.
set -e
cd "$(dirname "$0")/../.."
D=build/demo
NAME=$1
BG=0x1e1e1e
FONT=/System/Library/Fonts/Helvetica.ttc
BAND=180

filter="pad=iw:ih+$BAND:0:0:color=$BG"
if [ -f "$D/$NAME-captions/list.txt" ]; then
    while read -r from to file; do
        filter="$filter,drawtext=fontfile=$FONT:textfile=$file:fontcolor=white:fontsize=56:x=(w-text_w)/2:y=h-$BAND/2-text_h/2:enable='between(n,$from,$to)'"
    done < "$D/$NAME-captions/list.txt"
fi
printf '%s' "$filter" > "$D/$NAME-filter.txt"
RATE=$(cat "$D/$NAME-rate.txt" 2>/dev/null || echo 10)
ffmpeg -v error -y -framerate "$RATE" -i "$D/$NAME-frames/%05d.png" \
    -/filter:v "$D/$NAME-filter.txt" \
    -r 30 -c:v libx264 -pix_fmt yuv420p "$D/heml-$NAME.mp4"
echo "built $D/heml-$NAME.mp4"
