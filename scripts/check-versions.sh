#!/usr/bin/env bash
#
# Check for newer versions of packages in px/packages/
# Requires: curl, jq
# Optional: GITHUB_TOKEN env var for higher API rate limits

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_DIR="$SCRIPT_DIR/../px/packages"

outdated=0
checked=0
errors=0

if [ -z "${GITHUB_TOKEN:-}" ] && command -v gh &>/dev/null; then
    GITHUB_TOKEN=$(gh auth token 2>/dev/null || true)
fi

gh_api() {
    local url="$1"
    if [ -n "${GITHUB_TOKEN:-}" ]; then
        curl -sfL -H "Authorization: token $GITHUB_TOKEN" "$url"
    else
        curl -sfL "$url"
    fi
}

# Strip common tag prefixes
strip_tag() {
    local tag="$1"
    tag="${tag#release/}"
    tag="${tag#bun-v}"
    tag="${tag#rust-v}"
    tag="${tag#V}"
    tag="${tag#v}"
    tag="${tag#M}"
    tag="${tag#.}"
    # Strip package-name prefix (e.g., "qxkb-0.5.2" -> "0.5.2")
    if [[ "$tag" =~ ^[a-zA-Z_]+-([0-9].*)$ ]]; then
        tag="${BASH_REMATCH[1]}"
    fi
    echo "$tag"
}

# Fetch latest GitHub release tag; falls back to latest tag if no release
gh_latest() {
    local repo="$1"
    local tag
    # Try releases first
    tag=$(gh_api "https://api.github.com/repos/$repo/releases/latest" \
        | jq -r '.tag_name // empty' 2>/dev/null)
    # Fall back to latest tag
    if [ -z "$tag" ]; then
        tag=$(gh_api "https://api.github.com/repos/$repo/tags?per_page=1" \
            | jq -r '.[0].name // empty' 2>/dev/null)
    fi
    [ -z "$tag" ] && return 1
    strip_tag "$tag"
}

# Fetch latest tag by version sort (for repos with unreliable /releases/latest)
gh_latest_semver() {
    local repo="$1"
    gh_api "https://api.github.com/repos/$repo/tags?per_page=50" \
        | jq -r '.[].name // empty' 2>/dev/null \
        | while read -r t; do strip_tag "$t"; done \
        | grep -E '^[0-9]+\.' \
        | sort -V | tail -1
}

# Fetch latest crate version
crate_latest() {
    local crate="$1"
    curl -sf "https://crates.io/api/v1/crates/$crate" \
        | jq -r '.crate.newest_version // empty' 2>/dev/null || return 1
}

# Fetch latest npm package version
npm_latest() {
    local pkg="$1"
    curl -sf "https://registry.npmjs.org/$pkg/latest" \
        | jq -r '.version // empty' 2>/dev/null || return 1
}

# Fetch latest Forgejo (Codeberg) release tag; falls back to latest tag if no release
forgejo_latest() {
    local repo="$1"
    local host="${2:-codeberg.org}"
    local tag
    tag=$(curl -sfL "https://$host/api/v1/repos/$repo/releases/latest" \
        | jq -r '.tag_name // empty' 2>/dev/null) || tag=""
    if [ -z "$tag" ]; then
        tag=$(curl -sfL "https://$host/api/v1/repos/$repo/tags?limit=1" \
            | jq -r '.[0].name // empty' 2>/dev/null) || tag=""
    fi
    [ -z "$tag" ] && return 1
    strip_tag "$tag"
}

# Fetch latest GitLab release tag; falls back to latest tag if no release
gitlab_latest() {
    local project_id="$1"
    local host="${2:-gitlab.com}"
    local tag
    tag=$(curl -sfL "https://$host/api/v4/projects/$project_id/releases" \
        | jq -r '.[0].tag_name // empty' 2>/dev/null) || tag=""
    # Fall back to latest tag
    if [ -z "$tag" ]; then
        tag=$(curl -sfL "https://$host/api/v4/projects/$project_id/repository/tags?per_page=1" \
            | jq -r '.[0].name // empty' 2>/dev/null) || tag=""
    fi
    [ -z "$tag" ] && return 1
    strip_tag "$tag"
}

# Extract package version from .scm file (match name exactly via the (name "...") field)
pkg_version() {
    local pkg="$1"
    local ver
    ver=$(grep -A5 "(name \"$pkg\")" "$PKG_DIR"/*.scm 2>/dev/null \
        | grep -oP '\(version "?\K[^")]+' | head -1) || true
    # Handle let-bound version: (let ((version "X.Y.Z")) ... (version version))
    if [ "$ver" = "version" ]; then
        ver=$(grep -B10 "(name \"$pkg\")" "$PKG_DIR"/*.scm 2>/dev/null \
            | grep -oP '\(version "\K[^"]+' | head -1) || true
    fi
    # Handle variable-bound version: (version %my-var) with (define %my-var "X.Y.Z")
    if [[ "$ver" =~ ^% ]]; then
        ver=$(grep -hE "^\(define $ver " "$PKG_DIR"/*.scm 2>/dev/null \
            | grep -oP '\(define \S+ "\K[^"]+' | head -1) || true
    fi
    # Fall back: inherited packages (no explicit (name "...") field)
    if [ -z "$ver" ]; then
        ver=$(grep -A10 "^(define-public $pkg\$" "$PKG_DIR"/*.scm 2>/dev/null \
            | grep -oP '\(version "\K[^"]+' | head -1) || true
    fi
    echo "$ver"
}

compare() {
    local name="$1" current="$2" latest="$3"
    checked=$((checked + 1))
    if [ -z "$latest" ]; then
        printf "  ${YELLOW}%-30s %s -> ???${NC}\n" "$name" "$current"
        errors=$((errors + 1))
    elif [ "$current" = "$latest" ]; then
        printf "  ${GREEN}%-30s %s${NC}\n" "$name" "$current"
    else
        printf "  ${RED}%-30s %s -> %s${NC}\n" "$name" "$current" "$latest"
        outdated=$((outdated + 1))
    fi
}

gh_release() {
    local repo="$1" pkg="$2"
    local current latest
    current=$(pkg_version "$pkg")
    [ -z "$current" ] && return
    latest=$(gh_latest "$repo" 2>/dev/null || echo "")
    compare "$pkg" "$current" "$latest"
}

gh_release_tag() {
    local repo="$1" pkg="$2"
    local current tag
    current=$(pkg_version "$pkg")
    [ -z "$current" ] && return
    tag=$(gh_api "https://api.github.com/repos/$repo/releases/latest" \
        | jq -r '.tag_name // empty' 2>/dev/null) || tag=""
    compare "$pkg" "$current" "$tag"
}

crate_release() {
    local crate="$1" pkg="${2:-$1}"
    local current latest
    current=$(pkg_version "$pkg")
    [ -z "$current" ] && return
    latest=$(crate_latest "$crate" 2>/dev/null || echo "")
    compare "$pkg" "$current" "$latest"
}

npm_release() {
    local npm_pkg="$1" pkg="${2:-$1}"
    local current latest
    current=$(pkg_version "$pkg")
    [ -z "$current" ] && return
    latest=$(npm_latest "$npm_pkg" 2>/dev/null || echo "")
    compare "$pkg" "$current" "$latest"
}

gh_release_semver() {
    local repo="$1" pkg="$2"
    local current latest
    current=$(pkg_version "$pkg")
    [ -z "$current" ] && return
    latest=$(gh_latest_semver "$repo" 2>/dev/null || echo "")
    compare "$pkg" "$current" "$latest"
}

gitlab_release() {
    local project_id="$1" pkg="$2" host="${3:-gitlab.com}"
    local current latest
    current=$(pkg_version "$pkg")
    [ -z "$current" ] && return
    latest=$(gitlab_latest "$project_id" "$host" 2>/dev/null || echo "")
    compare "$pkg" "$current" "$latest"
}

forgejo_release() {
    local repo="$1" pkg="$2" host="${3:-codeberg.org}"
    local current latest
    current=$(pkg_version "$pkg")
    [ -z "$current" ] && return
    latest=$(forgejo_latest "$repo" "$host" 2>/dev/null || echo "")
    compare "$pkg" "$current" "$latest"
}

echo "Checking package versions..."
echo ""

# --- Binary Downloads (GitHub) ---
echo "Binary Downloads (GitHub):"
gh_release "cli/cli" "gh"
gh_release "gitbutlerapp/gitbutler" "gitbutler"
gh_release "ollama/ollama" "ollama"
gh_release "pnpm/pnpm" "pnpm"
gh_release "yarnpkg/yarn" "yarn"
gh_release "oven-sh/bun" "bun"
gh_release "zed-industries/zed" "zed"
gh_release "ActivityWatch/activitywatch" "activitywatch"
gh_release "CyberTimon/RapidRAW" "rapidraw"
gh_release "AppFlowy-IO/AppFlowy" "appflowy"
gh_release "rustdesk/rustdesk" "rustdesk"
gh_release "Mastermindzh/tidal-hifi" "tidal-hifi"
gh_release "dbeaver/dbeaver" "dbeaver"
gh_release "openai/codex" "codex"
gh_release "wakatime/wakatime-cli" "wakatime-cli"
gh_release "bluetuith-org/bluetuith" "bluetuith"
gh_release "slackhq/nebula" "nebula"
gh_release "v2fly/v2ray-core" "v2ray"
gh_release "XTLS/Xray-core" "xray-core"
gh_release "Alex313031/thorium" "thorium-browser"
gh_release "iotaledger/iota" "iota"
gh_release "vicinaehq/vicinae" "vicinae"
gh_release "googleworkspace/cli" "google-workspace-cli"
gh_release "aaddrick/claude-desktop-debian" "claude-desktop"
gh_release "mullvad/mullvadvpn-app" "mullvad-vpn-desktop"
echo ""

# --- Source Builds (GitHub) ---
echo "Source Builds (GitHub):"
gh_release "tailscale/tailscale" "tailscale"
gh_release "ivpn/desktop-app" "ivpn"
gh_release "squidowl/halloy" "halloy"
gh_release "albertlauncher/albert" "albert-launcher"
gh_release "raphamorim/rio" "rio"
gh_release "maximbaz/wluma" "wluma"
gh_release "orhun/binsider" "binsider"
gh_release "Canop/broot" "broot"
gh_release "terrastruct/d2" "d2"
gh_release "google/osv-scanner" "osv-scanner"
gh_release "franzos/tku" "tku"
gh_release "microsoft/edit" "edit"
gh_release "flowsurface-rs/flowsurface" "flowsurface"
gh_release "JakeStanger/ironbar" "ironbar"
gh_release "hecrj/icebreaker" "icebreaker"
gh_release "tsujan/FeatherPad" "featherpad"
gh_release "libvips/vipsdisp" "vipsdisp"
gh_release "wcbonner/GoveeBTTempLogger" "goveebttemplogger"
gh_release "sn99/nothing-linux" "nothing-linux"
gh_release "zed-industries/package-version-server" "package-version-server"
gh_release "wakatime/zed-wakatime" "wakatime-ls"
gh_release "Martchus/syncthingtray" "syncthingtray"
gh_release "pvanek/qlipper" "qlipper"
gh_release "Martchus/cpp-utilities" "cpputilities"
gh_release "Martchus/qtforkawesome" "qtforkawesome"
gh_release "Martchus/qtutilities" "qtutilities"
gh_release "nayuki/QR-Code-generator" "qr-code-generator"
gh_release "SRombauts/SQLiteCpp" "sqlitecpp"
gh_release "libcpr/cpr" "cpr"
gh_release_semver "mrtazz/restclient-cpp" "restclient-cpp"
gh_release "capnproto/pycapnp" "python-pycapnp"
gh_release "TamtamHero/fw-fanctrl" "fw-fanctrl"
gh_release "tpm2-software/tpm2-tss" "tpm2-tss"
gh_release "tpm2-software/tpm2-tss-engine" "tpm2-tss-engine"
gh_release "tpm2-software/tpm2-tools" "tpm2-tools"
gh_release "tpm2-software/tpm2-abrmd" "tpm2-abrmd"
gh_release "tpm2-software/tpm2-pkcs11" "tpm2-pkcs11"
gh_release "kaii-lb/overskride" "overskride"
gh_release "strawberrymusicplayer/strawberry" "strawberry"
gh_release "flxzt/rnote" "rnote"
gh_release "SeaDve/Kooha" "kooha"
gh_release "thesofproject/sof-bin" "sof-bin"
gh_release "Tarsnap/tarsnap" "tarsnap"
gh_release "franzos/podman-healthcheckd" "podman-healthcheckd"
gh_release "litehtml/litehtml" "litehtml"
gh_release "socketio/socket.io-client-cpp" "cpp-socketio-client"
gh_release "snwh/paper-icon-theme" "paper-icon-theme"
gh_release "thegala/qxkb" "qxkb"
gh_release "ForkAwesome/Fork-Awesome" "fork-awesome"
gh_release "getsentry/sentry-native" "sentry-native"
gh_release "reclosedev/requests-cache" "python-requests-cache"
gh_release "BlockIo/block_io-python" "python-block-io"
gh_release "Rikorose/DeepFilterNet" "deepfilternet-ladspa"
gh_release "franzos/jota" "jota"
gh_release "franzos/arbtt-capture-wl" "arbtt-capture-wl"
gh_release "franzos/vatic" "vatic"
gh_release "franzos/vpnmux" "vpnmux"
gh_release "FastFlowLM/FastFlowLM" "fastflowlm"
gh_release "importantimport/hatsu" "hatsu"
gh_release "pranshuparmar/witr" "witr"
gh_release "amd/xdna-driver" "xrt-plugin-amdxdna"
gh_release "Jmgr/actiona" "actiona"
gh_release "Huluti/Curtail" "curtail"
gh_release "jesseduffield/lazydocker" "lazydocker"
gh_release "ankitpokhrel/jira-cli" "jira-cli"
gh_release "Qalculate/libqalculate" "libqalculate"
gh_release "Qalculate/qalculate-gtk" "qalculate-gtk"
gh_release "noctalia-dev/noctalia-shell" "noctalia-shell"
gh_release "franzos/shelf" "shelf"
gh_release "franzos/guix-install" "guix-install"
gh_release "rustmailer/bichon" "bichon"
gh_release "peteonrails/voxtype" "voxtype"
gh_release "firecat53/networkmanager-dmenu" "networkmanager-dmenu"
echo ""

# --- Crates.io ---
echo "Crates.io:"
crate_release "himalaya"
crate_release "jj-cli" "jj-vcs"
crate_release "keifu"
crate_release "oculante"
crate_release "oha"
crate_release "sniffnet"
crate_release "envstash"
echo ""

# --- Other APIs (npm, GitLab, Forgejo) ---
echo "Other APIs:"
gh_release_semver "microsoft/vscode" "vscode"
npm_release "@anthropic-ai/claude-code" "claude-code"
gitlab_release "250833" "gitlab-runner"
gitlab_release "20101" "papers" "gitlab.gnome.org"
gh_release "spesmilo/electrum" "electrum-cc"
gitlab_release "23104371" "darkman"
forgejo_release "forgejo-contrib/forgejo-cli" "forgejo-cli"
echo ""

# --- Manual Check Required ---
# These packages don't have a clean API for version lookup
echo "Manual Check Required:"
manual() {
    local pkg="$1" url="$2"
    local current
    current=$(pkg_version "$pkg")
    [ -z "$current" ] && return
    checked=$((checked + 1))
    printf "  ${YELLOW}%-30s %-14s %s${NC}\n" "$pkg" "$current" "$url"
}

manual "cursor"          "https://www.cursor.com/changelog"
manual "antigravity"     "https://edgedl.me.gvt1.com/edgedl/release2/j0qc3/antigravity/stable/"
manual "discord"         "https://discord.com/api/download?platform=linux&format=deb"
manual "slack-desktop"   "https://slack.com/release-notes/linux"
manual "monit"           "https://mmonit.com/monit/changes/"
manual "xrt"             "https://github.com/Xilinx/XRT/releases"
manual "mastodonpp"      "https://schlomp.space/tastytea/mastodonpp/releases (domain offline)"
manual "ghostty"         "https://github.com/dariogriffo/ghostty-debian/releases"
manual "noctalia-qs"     "https://github.com/noctalia-dev/noctalia-qs/commits/main (git-pinned)"
manual "thunderbird"     "https://archive.mozilla.org/pub/thunderbird/releases/"
manual "bitwig-studio"   "https://www.bitwig.com/download/"
manual "ngrok"           "https://ngrok.com/download"
manual "arbtt"           "https://hackage.haskell.org/package/arbtt"
manual "rtkit"           "https://gitlab.freedesktop.org/pipewire/rtkit/-/tags"
manual "chkrootkit"      "http://www.chkrootkit.org/"
manual "easyeffects-presets-framework" "https://github.com/FrameworkComputer/linux-docs (git-pinned)"
manual "litebrowser"     "https://github.com/litehtml/litebrowser-linux (git-pinned)"
echo ""

# --- Summary ---
echo "---"
echo "Checked: $checked | Outdated: $outdated | Errors: $errors"
