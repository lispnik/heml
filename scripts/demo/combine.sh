#!/bin/sh
# scripts/demo/combine.sh -- `make demo-full': every demo video, each after
# a title card, in build/demo/heml.mp4.  A part whose video was not made is
# left out.
set -e
cd "$(dirname "$0")/../.."
D=build/demo
W=1600
H=1080
BG=0x1e1e1e
FONT=/System/Library/Fonts/Helvetica.ttc

# card FILE SECONDS TITLE SUBTITLE
card() {
  ffmpeg -v error -y -f lavfi -i "color=c=$BG:s=${W}x${H}:d=$2:r=30" \
    -vf "drawtext=fontfile=$FONT:text='$3':fontcolor=white:fontsize=72:x=(w-text_w)/2:y=(h-text_h)/2-40,drawtext=fontfile=$FONT:text='$4':fontcolor=0xaaaaaa:fontsize=34:x=(w-text_w)/2:y=(h-text_h)/2+60" \
    -c:v libx264 -pix_fmt yuv420p "$1"
}

# fit IN OUT: scaled into the frame, on the editor's background.
fit() {
  ffmpeg -v error -y -i "$1" \
    -vf "scale=$W:$H:force_original_aspect_ratio=decrease,pad=$W:$H:(ow-iw)/2:(oh-ih)/2:color=$BG,fps=30,format=yuv420p" \
    -c:v libx264 -an "$2"
}

parts=""
# part NAME TITLE SUBTITLE: build/demo/heml-NAME.mp4, after its card.
part() {
  if [ -f "$D/heml-$1.mp4" ]; then
    card "$D/card-$1.mp4" 2.5 "$2" "$3"
    fit "$D/heml-$1.mp4" "$D/part-$1.mp4"
    parts="$parts card-$1.mp4 part-$1.mp4"
  else
    echo "no $D/heml-$1.mp4: left out"
  fi
}

card $D/card-title.mp4 3 "Heml" "An Emacs-style editor in Common Lisp"
parts="card-title.mp4"
part tty "In a terminal" "SBCL and ECL"
part cocoa "Native on macOS" "Cocoa, through the objc bridge"
part tree-sitter "Highlighting" "By major mode, with tree-sitter"
part languages "Language servers" "Hints, hover, completion, errors, definitions, renaming"
part projects "Projects" "Finding files, searching, folding, Dired and Bufed"
part run "Running and testing" "Rust, Go and TypeScript, into the compilation buffer"
part debug "Debugging" "The Debug Adapter Protocol, with lldb-dap"
part git "Git" "The fringe, the status, staging, committing, log and blame"
printf "file '%s'\n" $parts > $D/parts.txt
ffmpeg -v error -y -f concat -safe 0 -i $D/parts.txt -c:v libx264 -pix_fmt yuv420p $D/heml.mp4
echo "built $D/heml.mp4"
