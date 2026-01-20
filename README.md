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

Already configured if using `%panther-base-services` or `%panther-desktop-services`.

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

## System Services

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
(use-modules (px system panther))
```

### Packages

| Variable | Description |
|----------|-------------|
| `%panther-base-packages` | Extends `%base-packages` with wpa-supplicant, libimobiledevice, neovim |
| `%panther-desktop-packages` | Adds U2F/FIDO2 support (pam-u2f, libu2f-*) and blueman |

### Services

| Variable | Description |
|----------|-------------|
| `%panther-base-services` | Extends `%base-services` with panther channel and substitute servers |
| `%panther-desktop-services` | Full desktop services including display managers, audio, and udev rules for security tokens |
| `%panther-desktop-services-minimal` | Desktop services without login/display managers and audio (for custom greeter setups) |

### Operating Systems

| Variable | Description |
|----------|-------------|
| `%panther-os` | Base OS with `%panther-base-services` and `%panther-base-packages` |
| `%panther-desktop-os` | Desktop OS with `%panther-desktop-services` and `%panther-desktop-packages` |

### When to Use What

- **Headless server**: Use `%panther-os` directly or inherit from it
- **Desktop with GDM/SDDM**: Use `%panther-desktop-os` or `%panther-desktop-services`
- **Desktop with custom greeter** (greetd, etc.): Use `%panther-desktop-services-minimal` to avoid conflicts

### Usage

Inherit from an OS definition and customize:

```scheme
(operating-system
  (inherit %panther-os)
  (host-name "my-workstation")
  (timezone "Europe/Berlin")
  ;; Add your file-systems, users, etc.
  (services
   (cons* (service openssh-service-type)
          %panther-base-services)))
```

Or use just the services/packages in your own OS:

```scheme
(operating-system
  ;; ...your configuration...
  (packages
   (cons* my-extra-package
          %panther-desktop-packages))
  (services
   (modify-services %panther-desktop-services-minimal
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
