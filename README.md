# My guix channel "panther"

This repository contains GUIX package defintions maintained primarily by [Franz Geffke](https://gofranz.com).

## Channel Definition

```scheme
(cons* (channel
        (name 'pantherx)
        (url "https://codeberg.org/gofranz/panther.git")
        ;; Enable signature verification
        (introduction
         (make-channel-introduction
          "54b4056ac571611892c743b65f4c47dc298c49da"
          (openpgp-fingerprint
           "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB"))))
       %default-channels)
```

## Substitute Server

**URL:** `https://substitutes.guix.gofranz.com`

```scheme
;; Public key
(public-key
 (ecc
  (curve Ed25519)
  (q #0096373009D945F86C75DFE96FC2D21E2F82BA8264CB69180AA4F9D3C45BAA47#)))
```

```bash
# Authorize
sudo guix archive --authorize < /path/to/key.pub
```

Already configured if using `%os-base-services` or `%os-desktop-services`.

## Home Services

### Darkman

Darkman is a framework for managing dark/light mode transitions. It automatically switches themes based on sunrise/sunset times.

**Usage:**

```scheme
(use-modules (px home services darkman))

;; Default configuration (uses geoclue2 for location)
(service home-darkman-service-type)

;; With manual coordinates
(service home-darkman-service-type
         (home-darkman-configuration
          (latitude 52.52)
          (longitude 13.405)
          (use-geoclue? #f)))

;; Custom configuration
(service home-darkman-service-type
         (home-darkman-configuration
          (latitude 37.7749)
          (longitude -122.4194)
          (use-geoclue? #f)
          (dbus-server? #t)
          (portal? #f)))
```

**Mode-switching scripts:**

Place executable scripts in:
- `~/.local/share/dark-mode.d/` - Executed when switching to dark mode
- `~/.local/share/light-mode.d/` - Executed when switching to light mode

**Manual control:**

```bash
darkman get          # Show current mode
darkman set dark     # Switch to dark mode
darkman set light    # Switch to light mode
darkman toggle       # Toggle between modes
```

### Foot Server

Foot server mode runs the foot terminal emulator as a background daemon, allowing fast terminal startup with `footclient`.

**Usage:**

```scheme
(use-modules (px home services foot))

;; Default configuration
(service home-foot-server-service-type)

;; With custom config file
(service home-foot-server-service-type
         (home-foot-server-configuration
          (config-file "/path/to/foot.ini")))

;; With hold option (remain open after child exits)
(service home-foot-server-service-type
         (home-foot-server-configuration
          (hold? #t)))
```

**Connecting to server:**

```bash
footclient           # Open new terminal window
footclient -- htop   # Open terminal running specific command
```

**Service management:**

```bash
herd start foot-server    # Start server
herd stop foot-server     # Stop server
herd status foot-server   # Check status
```

### Unattended Upgrade

Periodically runs `guix pull` followed by `guix home reconfigure`. Drop-in home equivalent of the system `unattended-upgrade-service-type` with battery awareness — upgrades are skipped when the laptop is on battery power.

**Usage:**

```scheme
(use-modules (px home services unattended-upgrade))

;; Minimal configuration (config-file is required)
(service home-unattended-upgrade-service-type
         (home-unattended-upgrade-configuration
          (config-file "/home/user/dotfiles/home/home.scm")
          (channels #~
                    (cons* (channel
                            (name 'my-channel)
                            (url "https://example.com/channel.git"))
                           %default-channels))))

;; Full configuration
(service home-unattended-upgrade-service-type
         (home-unattended-upgrade-configuration
          (config-file "/home/user/dotfiles/home/home.scm")
          (skip-on-battery? #t)
          (schedule "0 19 * * *")
          (channels #~ %default-channels)))
```

**Configuration options:**

| Field | Default | Description |
|-------|---------|-------------|
| `config-file` | (required) | Path to `home.scm` configuration file |
| `channels` | `%default-channels` | Gexp producing a list of channels for `guix pull -C` |
| `schedule` | `"0 19 * * *"` | Cron schedule string |
| `system-expiration` | 90 days | Max age of home generations before deletion |
| `maximum-duration` | 3600 | Max seconds the upgrade may run |
| `skip-on-battery?` | `#f` | Skip upgrade when on battery power |
| `log-file` | `~/.local/state/unattended-home-upgrade.log` | Log file path |
| `warm-packages` | `'()` | List of package names to `guix build` after reconfigure |

**Service management:**

```bash
herd status unattended-home-upgrade   # Check status
herd trigger unattended-home-upgrade  # Trigger upgrade now
```

### Podman Healthcheckd

Runs podman container healthchecks on systems without systemd.

**Usage:**

```scheme
(use-modules (px services containers))

;; Default configuration
(service home-podman-healthcheckd-service-type)

;; With custom log level
(service home-podman-healthcheckd-service-type
         (home-podman-healthcheckd-configuration
          (log-level "debug")))
```

**Service management:**

```bash
herd start podman-healthcheckd    # Start daemon
herd stop podman-healthcheckd     # Stop daemon
herd status podman-healthcheckd   # Check status
```

## System Services

### Unattended Upgrade

Drop-in replacement for `(gnu services admin)` `unattended-upgrade-service-type` with battery awareness. All upstream fields are preserved; additions are `skip-on-battery?` and `system-load-paths`.

**Usage:**

```scheme
(use-modules (px services unattended-upgrade))

(service unattended-upgrade-service-type
         (unattended-upgrade-configuration
          (schedule "0 17 * * *")
          (skip-on-battery? #t)
          (system-load-paths '("/home/user/dotfiles/system"))
          (channels #~
                    (cons* (channel
                            (name 'my-channel)
                            (url "https://example.com/channel.git"))
                           %default-channels))))
```

**Additional configuration options:**

| Field | Default | Description |
|-------|---------|-------------|
| `skip-on-battery?` | `#f` | Skip upgrade when on battery power |
| `system-load-paths` | `'()` | Extra `-L` load paths for `guix system reconfigure` |

All other fields (`operating-system-file`, `schedule`, `channels`, `reboot?`, `services-to-restart`, `system-expiration`, `maximum-duration`, `log-file`) match the upstream `(gnu services admin)` defaults.

**Caveat on `system-load-paths`:** Guix only stores the top-level configuration file in the store (`/run/current-system/configuration.scm`), not its imported modules. If your config imports local modules (e.g. `(common)`) that live outside a channel, you need `system-load-paths` so the unattended upgrade can find them. These modules are resolved from disk at upgrade time — not from a stored snapshot — so they should be kept in sync with your configuration.

**Battery detection:** Reads `/sys/class/power_supply/*/type` to locate AC adapters and checks their `online` status via sysfs. Desktops without battery info proceed normally.

### IOTA Node

Run an IOTA full node or validator node.

**Prerequisites:**

1. Create configuration file (e.g., `/etc/iota/fullnode.yaml`)
2. Download genesis blob for your network (mainnet/testnet/devnet)
3. For validators: generate key pairs

**Usage:**

```scheme
(use-modules (px services iota))

;; Basic full node
(service iota-node-service-type
         (iota-node-configuration
          (config-file "/etc/iota/fullnode.yaml")))

;; With custom settings
(service iota-node-service-type
         (iota-node-configuration
          (config-file "/etc/iota/validator.yaml")
          (data-directory "/var/lib/iota")
          (log-file "/var/log/iota-node.log")
          (log-level "info,iota_core=debug,consensus=debug")))
```

**Service management:**

```bash
herd status iota-node   # Check status
herd start iota-node    # Start node
herd stop iota-node     # Stop node
```

**Configuration options:**

| Field | Default | Description |
|-------|---------|-------------|
| `config-file` | (required) | Path to fullnode.yaml or validator.yaml |
| `user` | `"iota"` | System user to run as |
| `group` | `"iota"` | System group |
| `data-directory` | `"/var/lib/iota"` | Database and state storage |
| `log-file` | `"/var/log/iota-node.log"` | Log output location |
| `log-level` | `"info,iota_core=debug,..."` | Rust log levels |

**Required ports:**

- TCP/9000 - JSON-RPC
- UDP/8084 - P2P sync

### RealtimeKit

RealtimeKit grants real-time scheduling to user processes on request. Required by PipeWire and PulseAudio for low-latency audio.

**Usage:**

```scheme
(use-modules (px services audio))

(service rtkit-daemon-service-type)
```

### Tailscale

Tailscale is a zero-config VPN built on WireGuard.

**Usage:**

```scheme
(use-modules (px services networking))

(service tailscale-service-type)
```

**After reconfiguration:**

```bash
tailscale up         # Authenticate and connect
tailscale status     # Check connection status
```

## System Configuration

This channel provides pre-configured building blocks for Guix system definitions. Import with:

```scheme
(use-modules (px system os))
```

### Packages

| Variable | Description |
|----------|-------------|
| `%os-base-packages` | Extends `%base-packages` with wpa-supplicant, libimobiledevice, neovim |

### Services

| Variable | Description |
|----------|-------------|
| `%os-base-services` | Extends `%base-services` with panther channel and substitute servers |
| `%os-desktop-services` | Extends `%desktop-services` with panther channel and substitute servers |
| `%os-desktop-services-minimal` | Desktop services without login/display managers and audio (for custom greeter setups) |

### Operating Systems

| Variable | Description |
|----------|-------------|
| `%os-base` | Minimal OS with `%os-base-services` and `%os-base-packages` |

### When to Use What

- **Headless server**: Use `%os-base` directly or inherit from it
- **Desktop with GDM/SDDM**: Inherit `%os-base` and use `%os-desktop-services`
- **Desktop with custom greeter** (greetd, etc.): Use `%os-desktop-services-minimal` to avoid conflicts

### Usage

Inherit from an OS definition and customize:

```scheme
(operating-system
  (inherit %os-base)
  (host-name "my-workstation")
  (timezone "Europe/Berlin")
  ;; Add your file-systems, users, etc.
  (services
   (cons* (service openssh-service-type)
          %os-base-services)))
```

Or use just the services/packages in your own OS:

```scheme
(operating-system
  ;; ...your configuration...
  (packages
   (cons* my-extra-package
          %os-base-packages))
  (services
   (modify-services %os-desktop-services-minimal
     (elogind-service-type config =>
       (elogind-configuration
         (inherit config)
         (handle-lid-switch 'suspend))))))
```

## Time Travel

When things break because of upstream changes, this will allow you to run a future guix commit, to fix and test the channel without updating the whole system.

Create a channels file that includes only the guix channel:

```scheme
(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (branch "master")
        ;; Specify commit
        (commit "dc1a77267f03e37b331c8597b066c5ee52a75445")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
```

Spawn a shell for a clean environment:

```bash
guix shell --container --nesting --network openssl nss-certs coreutils guix
```

And build the target package:

```bash
guix time-machine --channels=default-channel.scm -- build -L panther pimsync
```

## Known Issues

- **Channel modules shadowed by system profile** ([bug #74396](https://issues.guix.gnu.org/74396)): After `guix pull`, new package versions may not be available until you also run `guix system reconfigure`. Workaround: keep system and user guix in sync, or use `guix time-machine -C ~/.config/guix/channels.scm -- shell <package>`.

## Upstream

Packages here may be upstreamed to Guix if they meet Guix's free software requirements. Please include me in the copyright notice if you do.
