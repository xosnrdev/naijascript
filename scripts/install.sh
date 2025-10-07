#!/bin/sh

set -eu

info() {
	printf '\033[1;34minfo:\033[0m %s\n' "$*"
}

success() {
	printf '\033[1;32mok:\033[0m %s\n' "$*"
}

warn() {
	printf '\033[1;33mwarn:\033[0m %s\n' "$*" >&2
}

error() {
	printf '\033[1;31merror:\033[0m %s\n' "$*" >&2
	exit 1
}

BIN="naija"
INSTALL_DIR=".naijascript"
BIN_ROOT="${HOME}/${INSTALL_DIR}/bin"
BIN_PATH="${BIN_ROOT}/${BIN}"
CONFIG_FILE="${HOME}/${INSTALL_DIR}/config.toml"
REPO="xosnrdev/naijascript"

info "Checking environment..."
if [ -x "${BIN_PATH}" ]; then
	warn "NaijaScript interpreter already exists at: ${BIN_PATH}"
	exit 0
fi

info "Detecting OS..."
OS=$(uname | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

case "${OS}" in
linux)
	case "${ARCH}" in
	x86_64 | amd64) TARGET="x86_64-unknown-linux-gnu" ;;
	aarch64 | arm64) TARGET="aarch64-unknown-linux-gnu" ;;
	*) error "Your architecture ('${ARCH}') is not supported. This script requires x86_64 or aarch64." ;;
	esac
	;;
darwin)
	case "${ARCH}" in
	x86_64 | amd64) TARGET="x86_64-apple-darwin" ;;
	aarch64 | arm64) TARGET="aarch64-apple-darwin" ;;
	*) error "Your architecture ('${ARCH}') is not supported. This script requires x86_64 or aarch64." ;;
	esac
	;;
*) error "Your operating system ('${OS}') is not supported. This script runs on Linux and macOS." ;;
esac

info "Fetching the latest version..."
LATEST_TAG=$(curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" | grep '"tag_name"' | cut -d '"' -f4)
if [ -z "${LATEST_TAG}" ]; then
	error "Failed to fetch latest version. Please check your network or try again later."
fi

ASSET_URL="https://github.com/${REPO}/releases/download/${LATEST_TAG}/${BIN}-${LATEST_TAG}-${TARGET}.tar.xz"
SHA_URL="https://github.com/${REPO}/releases/download/${LATEST_TAG}/${BIN}-${LATEST_TAG}-${TARGET}.sha256"

TMPDIR=$(mktemp -d)
cd "${TMPDIR}"

info "Downloading files for version ${LATEST_TAG}..."
curl -fsSLO "${ASSET_URL}"
curl -fsSLO "${SHA_URL}"

info "Verifying checksum..."
if command -v sha256sum >/dev/null 2>&1; then
	sha256sum -c --quiet "${BIN}-${LATEST_TAG}-${TARGET}.sha256" || error "Checksum verification failed. The file may be corrupt."
else
	ACTUAL=$(shasum -a 256 -q "${BIN}-${LATEST_TAG}-${TARGET}.tar.xz" | awk '{print $1}')
	EXPECTED=$(cut -d ' ' -f1 "${BIN}-${LATEST_TAG}-${TARGET}.sha256")
	[ "${ACTUAL}" = "${EXPECTED}" ] || error "Checksum verification failed. The file may be corrupt."
fi

info "Installing binary..."
tar -xf "${BIN}-${LATEST_TAG}-${TARGET}.tar.xz"
mkdir -p "${BIN_ROOT}"

TMP_BIN=$(mktemp)
install -m 755 "${BIN}" "${TMP_BIN}"

info "Testing installation..."
if ! "${TMP_BIN}" --version >/dev/null 2>&1; then
	rm -f "${TMP_BIN}"
	error "The installed binary failed to run. It may be incompatible with your system."
fi

mv "${TMP_BIN}" "${BIN_PATH}"

CONFIG_VERSION=${LATEST_TAG#v}
info "Setting default version to '${CONFIG_VERSION}'..."
mkdir -p "$(dirname "${CONFIG_FILE}")"
printf 'default = "%s"\n' "${CONFIG_VERSION}" >"${CONFIG_FILE}"

info "Configuring shell environment..."
if ! echo ":${PATH}:" | grep -q ":${BIN_ROOT}:"; then
	PROFILE=""
	if [ -n "${ZSH_VERSION-}" ] || [ "$(basename "${SHELL}")" = "zsh" ]; then
		PROFILE="${HOME}/.zshrc"
	elif [ -n "${BASH_VERSION-}" ] || [ "$(basename "${SHELL}")" = "bash" ]; then
		PROFILE="${HOME}/.bashrc"
	else
		PROFILE="${HOME}/.profile"
	fi

	if ! grep -q "export PATH=\"${BIN_ROOT}:\$PATH\"" "${PROFILE}" 2>/dev/null; then
		printf "\nexport PATH=\"%s:\$PATH\"\n\n" "${BIN_ROOT}" >>"${PROFILE}"
		info "Added '${BIN_ROOT}' to PATH in '${PROFILE}'."
		info "Restart your terminal or run 'source ${PROFILE}' to apply the changes."
	fi
fi

printf "\n"
success "Installation complete."

cleanup() {
	info "Cleaning up temporary files..."
	rm -rf "${TMPDIR}"
}

trap cleanup EXIT
