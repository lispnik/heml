#!/bin/sh
# Notarize one item (an .app or a .dmg) with Apple and staple the ticket to it.
#
# Credentials come from the environment, never from the tree:
#   NOTARY_KEYCHAIN_PROFILE   a profile made by `xcrun notarytool store-credentials`
# or an App Store Connect API key, which is what CI has:
#   NOTARY_KEY_ID  NOTARY_KEY_ISSUER  NOTARY_KEY_FILE (the .p8)
# With neither set this does nothing and says so, so the ad-hoc build path
# stays a plain `make dmg`.
set -e
ITEM="$1"
[ -e "$ITEM" ] || { echo "notarize: no such item: $ITEM" >&2; exit 1; }

if [ -n "$NOTARY_KEYCHAIN_PROFILE" ]; then
  AUTH="--keychain-profile $NOTARY_KEYCHAIN_PROFILE"
elif [ -n "$NOTARY_KEY_ID" ] && [ -n "$NOTARY_KEY_ISSUER" ] && [ -n "$NOTARY_KEY_FILE" ]; then
  AUTH="--key $NOTARY_KEY_FILE --key-id $NOTARY_KEY_ID --issuer $NOTARY_KEY_ISSUER"
else
  echo "notarize: no credentials in the environment; $ITEM left un-notarized"
  exit 0
fi

# An app goes up as a zip; a disk image goes up as itself.
case "$ITEM" in
  *.app)
    ZIP=$(mktemp -d)/$(basename "$ITEM" .app).zip
    ditto -c -k --keepParent "$ITEM" "$ZIP"
    UPLOAD="$ZIP" ;;
  *) UPLOAD="$ITEM" ;;
esac

# --wait blocks until Apple answers, usually a minute or two.  A rejection
# prints a submission id; `xcrun notarytool log <id>` says why.
xcrun notarytool submit "$UPLOAD" $AUTH --wait
xcrun stapler staple "$ITEM"
xcrun stapler validate "$ITEM"
[ -n "$ZIP" ] && rm -rf "$(dirname "$ZIP")"
echo "notarized and stapled $ITEM"
