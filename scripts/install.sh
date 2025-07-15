#!/usr/bin/env sh
set -euo pipefail

# --- Platform/Arch Detection ---
OS=$(uname | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

# --- Platform and Architecture Mapping ---
case "$OS" in
  linux)   PLATFORM=linux ;;
  darwin)  PLATFORM=macos ;;
  *) echo "Oga, your OS no dey supported. Abeg use Linux or Mac." >&2; exit 1 ;;
esac

case "$ARCH" in
  x86_64|amd64) ARCH=x86_64 ;;
  aarch64|arm64) ARCH=aarch64 ;;
  *) echo "Oga, your machine arch no dey supported. Abeg use x86_64 or aarch64." >&2; exit 1 ;;
esac

TARGET="$ARCH-$PLATFORM"
BIN=naijaup

# --- Fetch Latest Version ---
REPO="xosnrdev/naijascript"
LATEST_TAG=$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" | grep '"tag_name"' | cut -d '"' -f4)
if [ -z "$LATEST_TAG" ]; then
  echo "Wahala! I no fit find latest version for GitHub. Check your network." >&2; exit 1
fi

ASSET_URL="https://github.com/$REPO/releases/download/$LATEST_TAG/${BIN}-v${LATEST_TAG}-$TARGET.tar.gz"
SHA_URL="https://github.com/$REPO/releases/download/$LATEST_TAG/${BIN}-v${LATEST_TAG}-$TARGET.sha256"

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
cd "$TMPDIR"

# --- Download Binary and Checksum ---
echo "I dey download $BIN..."
curl -fsSLO "$ASSET_URL"
curl -fsSLO "$SHA_URL"


# --- Verify Checksum of Archive ---
echo "I dey check say archive correct..."
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum -c "${BIN}-v${LATEST_TAG}-$TARGET.sha256" || { echo "Omo! Checksum no match. No try run am o." >&2; exit 1; }
else
  ACTUAL=$(shasum -a 256 "${BIN}-v${LATEST_TAG}-$TARGET.tar.gz" | awk '{print $1}')
  EXPECTED=$(cut -d' ' -f1 "${BIN}-v${LATEST_TAG}-$TARGET.sha256")
  [ "$ACTUAL" = "$EXPECTED" ] || { echo "Omo! Checksum no match. No try run am o." >&2; exit 1; }
fi

# --- Extract Archive ---
echo "I dey extract $BIN..."
tar -xzf "${BIN}-v${LATEST_TAG}-$TARGET.tar.gz"
chmod +x naijaup

# --- Install ---
INSTALL_DIR="$HOME/.local/bin"
mkdir -p "$INSTALL_DIR"
mv naijaup "$INSTALL_DIR/naijaup"

# --- Self-Test ---
echo "I don put naijaup for $INSTALL_DIR. Make I check am..."
if "$INSTALL_DIR/naijaup" --version >/dev/null 2>&1; then
  echo "Naijaup don land gidigba!"
else
  echo "E get as e be. Naijaup no run. Check your PATH or try again." >&2; exit 1
fi

case ":$PATH:" in
  *:"$INSTALL_DIR":*) :;;
  *)
    echo "Oga, your PATH no get $INSTALL_DIR. I go add am for you."
    # Try to detect the user's shell profile
    PROFILE=""
    if [ -n "${ZSH_VERSION-}" ] || [ "$(basename "$SHELL")" = "zsh" ]; then
      PROFILE="$HOME/.zshrc"
    elif [ -n "${BASH_VERSION-}" ] || [ "$(basename "$SHELL")" = "bash" ]; then
      PROFILE="$HOME/.bashrc"
    elif [ -f "$HOME/.profile" ]; then
      PROFILE="$HOME/.profile"
    else
      PROFILE="$HOME/.profile"
    fi
    # Only add if not already present
    if ! grep -q "$INSTALL_DIR" "$PROFILE" 2>/dev/null; then
      echo "export PATH=\"$INSTALL_DIR:\$PATH\"" >> "$PROFILE"
      echo "I don add $INSTALL_DIR to your PATH for next terminal session ($PROFILE)."
      echo "If you wan use am now, run: export PATH=\"$INSTALL_DIR:\$PATH\""
    else
      echo "$INSTALL_DIR already dey your PATH in $PROFILE."
    fi
    ;;
esac

echo "If you wan install NaijaScript interpreter, run: naijaup install latest"
echo "Oya, you fit enjoy NaijaScript now!"
