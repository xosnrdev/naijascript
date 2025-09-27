#!/usr/bin/env sh
set -eu

# --- Platform/Arch Detection ---
OS=$(uname | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

# --- Platform and Architecture Mapping ---
case "$OS" in
  linux)
    case "$ARCH" in
      x86_64|amd64) TARGET="x86_64-unknown-linux-gnu" ;;
      aarch64|arm64) TARGET="aarch64-unknown-linux-gnu" ;;
      *) echo "Dis machine architecture no get support. We expect x86_64 or aarch64." >&2; exit 1 ;;
    esac
    ;;
  darwin)
    case "$ARCH" in
      x86_64|amd64) TARGET="x86_64-apple-darwin" ;;
      aarch64|arm64) TARGET="aarch64-apple-darwin" ;;
      *) echo "Dis machine architecture no get support. We expect x86_64 or aarch64." >&2; exit 1 ;;
    esac
    ;;
  *) echo "Dis operating system no get support. Use Linux or Mac." >&2; exit 1 ;;
esac


# --- Fetch Latest Version ---
echo "Fetching latest version..."
REPO="xosnrdev/naijaup"
LATEST_TAG=$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" | grep '"tag_name"' | cut -d '"' -f4)
if [ -z "$LATEST_TAG" ]; then
  echo "I no fit fetch latest version. Check your network and try again." >&2; exit 1
fi

BIN=naijaup
ASSET_URL="https://github.com/$REPO/releases/download/$LATEST_TAG/${BIN}-${LATEST_TAG}-$TARGET.tar.xz"
SHA_URL="https://github.com/$REPO/releases/download/$LATEST_TAG/${BIN}-${LATEST_TAG}-$TARGET.sha256"

TMPDIR=$(mktemp -d)
trap 'echo "Cleaning up..."; rm -rf "$TMPDIR"' EXIT
cd "$TMPDIR"

# --- Download Binary and Checksum ---
echo "Downloading binary and checksum..."
curl -fsSLO "$ASSET_URL"
curl -fsSLO "$SHA_URL"


# --- Verify Checksum of Archive ---
echo "Verifying checksum..."
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum -c "${BIN}-${LATEST_TAG}-$TARGET.sha256" || { echo "Checksum no match. Aborting installation." >&2; exit 1; }
else
  ACTUAL=$(shasum -a 256 "${BIN}-${LATEST_TAG}-$TARGET.tar.xz" | awk '{print $1}')
  EXPECTED=$(cut -d' ' -f1 "${BIN}-${LATEST_TAG}-$TARGET.sha256")
  [ "$ACTUAL" = "$EXPECTED" ] || { echo "Checksum no match. Aborting installation." >&2; exit 1; }
fi

# --- Extract Archive ---
echo "Extracting binary..."
tar -xvf "${BIN}-${LATEST_TAG}-$TARGET.tar.xz"
chmod +x naijaup

# --- Install ---
echo "Installing binary..."
INSTALL_DIR="$HOME/.local/bin"
mkdir -p "$INSTALL_DIR"
mv naijaup "$INSTALL_DIR/naijaup"

# --- Self-Test ---
echo "Testing installation..."
if "$INSTALL_DIR/naijaup" --version >/dev/null 2>&1; then
  echo "Installation successful."
else
  echo "Self-check fail. Confirm PATH setup or run the installer again." >&2; exit 1
fi

case ":$PATH:" in
  *:"$INSTALL_DIR":*) :;;
  *)
    echo "PATH no contain $INSTALL_DIR. I go add am to your shell profile."
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
      echo "I don add $INSTALL_DIR to PATH inside $PROFILE."
      echo "Run dis command to load am now:"
      echo "  source \"$PROFILE\""
      echo "If you no wan restart shell, you fit run:"
      echo "  export PATH=\"$INSTALL_DIR:\$PATH\""
    else
      echo "$INSTALL_DIR already dey your PATH for inside $PROFILE."
    fi
    ;;
esac

echo "Installation don complete."
echo "Run 'naijaup install latest' to pull the newest Interpreter."
