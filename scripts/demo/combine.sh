#!/bin/sh
# scripts/demo/combine.sh -- `make demo-full': the terminal and the Cocoa
# videos, each after a title card, in build/demo/xoamax.mp4.
set -e
cd "$(dirname "$0")/../.."
D=build/demo
W=1600
H=960
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

card $D/card-title.mp4 3 "Xoamax" "An Emacs-style editor in Common Lisp"
card $D/card-tty.mp4 2.5 "In a terminal" "SBCL and ECL"
fit $D/xoamax-tty.mp4 $D/part-tty.mp4
card $D/card-cocoa.mp4 2.5 "Native on macOS" "Cocoa, through the objc bridge"
fit $D/xoamax-cocoa.mp4 $D/part-cocoa.mp4
printf "file '%s'\n" card-title.mp4 card-tty.mp4 part-tty.mp4 card-cocoa.mp4 part-cocoa.mp4 > $D/parts.txt
ffmpeg -v error -y -f concat -safe 0 -i $D/parts.txt -c:v libx264 -pix_fmt yuv420p $D/xoamax.mp4
echo "built $D/xoamax.mp4"
