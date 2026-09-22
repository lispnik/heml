#!/bin/sh
# Wrap build/Xoamax.app in build/Xoamax-<version>.dmg: the app and a link to
# /Applications, compressed.  Signed with MACOS_SIGNING_IDENTITY when set,
# and then notarized and stapled when scripts/notarize.sh finds credentials.
#
# The app is notarized and stapled first, on its own, so that the copy a user
# drags out of the image carries its ticket; then the image, so that the
# image itself opens without a warning.
set -e
cd "$(dirname "$0")/.."
APP="build/Xoamax.app"
[ -d "$APP" ] || { echo "no $APP: build it first (make app)" >&2; exit 1; }
VERSION=$(plutil -extract CFBundleShortVersionString raw "$APP/Contents/Info.plist")
DMG="build/Xoamax-$VERSION.dmg"
if [ -n "$MACOS_SIGNING_IDENTITY" ] && [ "$MACOS_SIGNING_IDENTITY" != "-" ]; then
  scripts/notarize.sh "$APP"
fi
STAGE=$(mktemp -d)
cp -R "$APP" "$STAGE/"
ln -s /Applications "$STAGE/Applications"
rm -f "$DMG"
hdiutil create -volname "Xoamax" -srcfolder "$STAGE" -ov -format UDZO -quiet "$DMG"
rm -rf "$STAGE"
if [ -n "$MACOS_SIGNING_IDENTITY" ] && [ "$MACOS_SIGNING_IDENTITY" != "-" ]; then
  codesign --force --sign "$MACOS_SIGNING_IDENTITY" --timestamp "$DMG"
  scripts/notarize.sh "$DMG"
fi
echo "built $DMG"
