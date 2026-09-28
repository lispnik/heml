# Releasing

```sh
make app                          # ad hoc: runs here, cannot be notarised
make app SIGN_IDENTITY=...        # signed with a Developer ID
make release SIGN_IDENTITY=...    # notarise the app and the disk image, staple both
```

`make release` is `notarize` then `notarize-dmg`, and both matter. Notarising
the **app** is what the notary service inspects; stapling the **disk image** is
what makes the file a user actually downloads recognisable to Gatekeeper. Doing
only the first leaves the download itself unrecognised.

The disk image is `dist/Heml-<version>-<arch>.dmg`, where the version is the
`:version` in `heml-app.asd`.

## Locally

The one hard prerequisite is an SBCL built `--without-sb-core-compression`.
Homebrew's links `libzstd`, and a bundle built with it notarises and then fails
to launch on a Mac without Homebrew. `make notarize` checks for that and stops.

```sh
# Once.  Asks for an app-specific password from appleid.apple.com, not your
# Apple ID password.
xcrun notarytool store-credentials heml \
  --apple-id you@example.com --team-id TEAMID

# Per release:
make release SIGN_IDENTITY="Developer ID Application: Your Name (TEAMID)"
gh release create v0.1.0 dist/*.dmg
```

A Mac builds only for its own architecture, since there is no universal SBCL
core.

## In CI

Pushing a tag `v*` runs `.github/workflows/release.yml`. It builds an SBCL
without core compression (and caches it), builds, signs, notarises and staples
on both an arm64 and an Intel runner, and publishes a release with both disk
images. Running the workflow by hand is a dry run unless you untick it: it
builds ad hoc and notarises nothing.

Four secrets:

| secret | what |
|---|---|
| `MACOS_CERTIFICATE` | `base64 -i cert.p12` |
| `MACOS_CERTIFICATE_PASSWORD` | the password you gave the `.p12` |
| `APPLE_ID` | your Apple ID email |
| `APPLE_APP_PASSWORD` | an app-specific password |

The signing identity and the team id are read from the certificate.

Export the `.p12` from Keychain Access: **login** keychain, My Certificates,
right-click the Developer ID Application entry, Export.

```sh
gh secret set MACOS_CERTIFICATE < <(base64 -i cert.p12)
gh secret set MACOS_CERTIFICATE_PASSWORD
gh secret set APPLE_ID
gh secret set APPLE_APP_PASSWORD
```
