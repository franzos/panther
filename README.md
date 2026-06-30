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

### Bichon

[Bichon](https://github.com/rustmailer/bichon) is a self-hosted email archiver: it syncs IMAP accounts, indexes messages for full-text search, and serves a REST API and web interface. It is not an email client — it can't send, compose, or reply. This runs it as a user daemon, with data under `$XDG_DATA_HOME/bichon` and the log under `$XDG_LOG_HOME`.

It reuses the shared `bichon-configuration` record from `(px services mail)` — see the [system service](#bichon-1) for the full field list. The encryption password is read from a file you manage, never baked into the store. Differences from the system service:

- **No system user/group** — runs as your user; `user`/`group` are ignored.
- **Data under XDG** — `root-dir` defaults to `$XDG_DATA_HOME/bichon` (`~/.local/share/bichon`) instead of `/var/lib/bichon`, unless you set it explicitly. The full-text index and blob store nest under it (`bichon-indices/`, `bichon-storage/`), and the daemon log goes to `$XDG_LOG_HOME/bichon.log` (`~/.local/var/log/bichon.log`). The service creates the data and log directories on activation.
- **Binds localhost** — `bind-ip` defaults to `127.0.0.1`, not all interfaces. Set it explicitly to expose the daemon on your network.

**Encryption password:** create the password file before first start (the daemon won't start without it):

```bash
install -d -m 700 ~/.config/bichon
printf '%s' "$(openssl rand -base64 48)" > ~/.config/bichon/encrypt-password
chmod 600 ~/.config/bichon/encrypt-password
```

`printf '%s'` keeps the trailing newline out so the file is exactly the secret. Back it up — this key encrypts your stored IMAP credentials, and it's write-once.

**Usage:**

```scheme
(use-modules (px home services mail)
             (px services mail))

(service home-bichon-service-type
         (bichon-configuration
          (encrypt-password-file "/home/user/.config/bichon/encrypt-password")))
```

**Default credentials:** first start creates `admin` / `admin@bichon` — change it immediately via the web interface (Settings → Profile).

**Service management:**

```bash
herd start bichon    # Start daemon
herd stop bichon     # Stop daemon
herd status bichon   # Check status
```

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

### Bichon

[Bichon](https://github.com/rustmailer/bichon) is a self-hosted email archiver written in Rust: it syncs IMAP accounts, indexes messages for full-text search (embedded storage — no external database), and serves a REST API and web interface on port `15630`. It is not an email client — it cannot send, compose, or reply. The service creates a dedicated `bichon:bichon` system user, the data directory at `/var/lib/bichon`, and rotates `/var/log/bichon.log` — no manual setup beyond the encryption password.

**Encryption password:** Bichon encrypts stored credentials with a password you provide. The service takes a *path* to a file holding that password (`encrypt-password-file`), so the secret never lands in the world-readable store. Create it before first start:

```bash
sudo install -d -m 700 /etc/bichon
openssl rand -base64 48 | tr -d '\n' | sudo tee /etc/bichon/encrypt-password >/dev/null
sudo chmod 600 /etc/bichon/encrypt-password
```

`tr -d '\n'` strips the trailing newline so the file is exactly the secret. Back it up — this key encrypts your stored IMAP credentials, and it's write-once: regenerating it against an existing data directory makes the stored data undecryptable. Activation warns (without failing) if the file is missing.

**Usage:**

```scheme
(use-modules (px services mail))

;; Minimal — encrypt-password-file is required
(service bichon-service-type
         (bichon-configuration
          (encrypt-password-file "/etc/bichon/encrypt-password")))

;; Behind a reverse proxy, with the SMTP receiver enabled
(service bichon-service-type
         (bichon-configuration
          (encrypt-password-file "/etc/bichon/encrypt-password")
          (bind-ip "127.0.0.1")
          (public-url "https://archive.example.org")
          (enable-smtp? #t)))
```

**Default credentials:** first start creates `admin` / `admin@bichon` — change it immediately via the web interface (Settings → Profile).

**Configuration options:**

| Field | Default | Description |
|-------|---------|-------------|
| `package` | `bichon` | The bichon package to use |
| `root-dir` | `"/var/lib/bichon"` | Data directory (`BICHON_ROOT_DIR`) |
| `encrypt-password-file` | (required) | Path to a file holding the credential-encryption password (`BICHON_ENCRYPT_PASSWORD_FILE`) |
| `http-port` | `15630` | HTTP / web interface port (`BICHON_HTTP_PORT`) |
| `bind-ip` | unset → `0.0.0.0` | Listen address; bichon binds all interfaces when unset (`BICHON_BIND_IP`) |
| `public-url` | unset | Public URL used in generated links (`BICHON_PUBLIC_URL`) |
| `base-url` | unset | Base path when behind a reverse proxy (`BICHON_BASE_URL`) |
| `log-level` | `"info"` | `trace` / `debug` / `info` / `warn` / `error` (`BICHON_LOG_LEVEL`) |
| `enable-smtp?` | `#f` | Enable the SMTP receiver (`BICHON_ENABLE_SMTP`) |
| `smtp-port` | `2525` | SMTP receiver port (`BICHON_SMTP_PORT`) |
| `index-dir` | `{root}/bichon-indices` | Full-text index directory (`BICHON_INDEX_DIR`) |
| `data-dir` | `{root}/bichon-storage` | Blob storage directory (`BICHON_DATA_DIR`) |
| `extra-env` | `'()` | Extra `BICHON_*` settings as an alist of `(string . string)` |
| `user` | `"bichon"` | System user to run as |
| `group` | `"bichon"` | System group |

Any setting not exposed above can be passed through `extra-env`, e.g. `(extra-env '(("BICHON_SYNC_CONCURRENCY" . "8")))`.

**Required ports:**

- TCP/15630 — HTTP REST API and web interface
- TCP/2525 — SMTP receiver (only when `enable-smtp?` is `#t`)

**Storage caveat:** Bichon does not support writing data to network filesystems (NFS, CIFS/SMB) — `root-dir` must be on local storage.

**Service management:**

```bash
herd status bichon   # Check status
herd start bichon    # Start daemon
herd stop bichon     # Stop daemon
```

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

### Chrony

Runs `chronyd`, the NTP daemon from the [Chrony project](https://chrony-project.org). Keeps the system clock in sync with the configured time servers. The service creates a dedicated `chrony:chrony` system user and the drift directory at `/var/lib/chrony` — no manual setup required.

**Default configuration uses NTS** ([RFC 8915](https://www.rfc-editor.org/rfc/rfc8915)) to authenticate time packets via TLS, preventing on-path attackers from forging NTP responses. The default sources are a geographically diverse mix of Stratum 1 servers:

```
server time.cloudflare.com iburst nts
server nts.netnod.se iburst nts
server ptbtime1.ptb.de iburst nts
server ptbtime2.ptb.de iburst nts
server ntppool1.time.nl iburst nts
driftfile /var/lib/chrony/drift
ntsdumpdir /var/lib/chrony
makestep 1.0 3
rtcsync
```

`ntsdumpdir` caches NTS cookies across restarts so the TLS handshake isn't repeated on every boot. There is currently no NTS pool — TLS certificates break the traditional `pool.ntp.org` pooling model, so sources are listed individually.

**Firewall requirement:** NTS needs outbound **TCP/4460** (NTS-KE handshake) in addition to the usual **UDP/123** (NTP). If TCP/4460 is blocked, `chronyc -N authdata` will show zeros in the KeyID/Type/KLen columns and the sources will never come up.

**First-boot caveat:** NTS certificate validation requires a roughly-correct clock. If the RTC is badly wrong, the TLS handshake will fail and chronyd won't be able to bootstrap. On fresh systems with unreliable RTCs, temporarily add an unauthenticated `pool 2.pool.ntp.org iburst` line until the clock is close enough for TLS to work.

**Usage:**

```scheme
(use-modules (px services ntp))

;; Default — five NTS-enabled sources (see above)
(service chrony-service-type)

;; With a custom chrony.conf
(service chrony-service-type
         (chrony-service-configuration
          (config "server time.cloudflare.com iburst nts
server ptbtime1.ptb.de iburst nts
driftfile /var/lib/chrony/drift
ntsdumpdir /var/lib/chrony
makestep 1.0 3
rtcsync
")))
```

**Configuration options:**

| Field | Default | Description |
|-------|---------|-------------|
| `package` | `chrony` | The chrony package to use |
| `config` | Five NTS sources + `driftfile` / `ntsdumpdir` / `makestep` / `rtcsync` | Raw `chrony.conf` contents |

**Service management:**

```bash
sudo herd status chrony          # Check status
sudo herd configuration chrony   # Print path to generated chrony.conf
sudo chronyc sources             # Show NTP sources and reachability
sudo chronyc tracking            # Show clock sync status
sudo chronyc -N authdata         # Verify NTS: KeyID/Type/KLen should be non-zero
```

### Forseti Stack

[Forseti](https://github.com/franzos/forseti) is a login, consent, and account portal for an [Ory Kratos](https://www.ory.sh/kratos/) (identity) and [Ory Hydra](https://www.ory.sh/hydra/) (OAuth2/OIDC) deployment. This channel packages all three (`forseti`, `ory-kratos`, `ory-hydra`) and provides a service-type for each, plus a `forseti-stack-services` helper that wires them together from a single public URL.

The three daemons serve plaintext HTTP on loopback. A TLS-terminating reverse proxy fronts the public origin (`base-url`) and routes browser traffic to Forseti's public port (3000). Hydra (4444) and Kratos (4433) public ports are reached server-to-server; the admin ports (4445 / 4434) and Forseti's internal listener (8081) must not be exposed.

Postgres uses unix-socket peer authentication: each daemon runs as a system user whose name equals its database role (`kratos`, `hydra`, `forseti`), so no database passwords exist anywhere. The stack creates the roles and databases when `manage-postgres?` is `#t`.

**Usage (the whole stack):**

`forseti-stack-services` returns a *list* of services, so splice it into your `services` field rather than wrapping it in `service`:

```scheme
(use-modules (px services forseti))

(operating-system
  ;; ...
  (services
   (append
    (forseti-stack-services
     (forseti-stack-configuration
      (base-url "https://id.example.com")))
    %base-services)))
```

**Secrets:** real secrets stay in operator-supplied env-files under `secrets-directory` (default `/etc/forseti`, root-owned 0600), sourced by each daemon's start wrapper, never baked into the store. The database DSN is *not* a secret (peer auth) and is set for you. Create the env-files before first start:

```bash
sudo install -d -m 700 /etc/forseti
AUDIT_TOKEN=$(openssl rand -hex 24)   # shared, only needed when audit-webhook? is on

# Hydra: system secret (>= 16 bytes)
printf 'SECRETS_SYSTEM=%s\n' "$(openssl rand -hex 16)" \
  | sudo tee /etc/forseti/hydra.env >/dev/null

# Kratos: cookie (>= 16 bytes) + cipher (EXACTLY 32 bytes)
sudo tee /etc/forseti/kratos.env >/dev/null <<EOF
SECRETS_COOKIE=$(openssl rand -hex 16)
SECRETS_CIPHER=$(openssl rand -hex 16)
FORSETI_AUDIT_TOKEN=$AUDIT_TOKEN
EOF

# Forseti: audit token (must match the kratos one) + cookie secret (>= 32 bytes)
sudo tee /etc/forseti/forseti.env >/dev/null <<EOF
FORSETI_AUDIT__WEBHOOK_TOKEN=$AUDIT_TOKEN
FORSETI_SECURITY__COOKIE_SECRET=$(openssl rand -hex 32)
EOF

sudo chmod 600 /etc/forseti/*.env
```

`openssl rand -hex 16` produces 32 hex characters (32 bytes), which satisfies the Kratos cipher length. When `audit-webhook?` is `#f`, the `FORSETI_AUDIT_TOKEN` line is unnecessary.

**Reverse proxy (HAProxy, single host):** the three daemons share one public origin (`base-url`), split by path. Forseti is at the root; Hydra mounts under `/hydra` and Kratos under `/kratos`, and the proxy strips those prefixes before the upstream (Hydra and Kratos do not honour subpath mounting). The generated configs follow this layout: Hydra's `issuer`/`public` is `base-url/hydra`, Kratos's `base_url` is `base-url/kratos`, and Forseti's login/consent pages plus the DCR endpoint stay at the root. This matches the "Shape 1" topology in Forseti's own [`docs/operator-guide-proxy.md`](https://github.com/franzos/forseti/blob/master/docs/operator-guide-proxy.md), which is the authoritative reference (it also covers subdomain and CORS/cookie details).

HAProxy is the recommended proxy. The config below targets the stack's loopback ports:

```haproxy
frontend fe_accounts
    bind *:443 ssl crt /etc/haproxy/certs/accounts.example.com.pem alpn h2,http/1.1
    http-request redirect scheme https code 301 unless { ssl_fc }

    # Trust only our own forwarded headers (strip client-sent first).
    http-request del-header X-Forwarded-For
    http-request del-header X-Forwarded-Proto
    http-request del-header X-Forwarded-Host
    http-request set-header X-Forwarded-Proto https
    http-request set-header X-Forwarded-Host  %[req.hdr(host)]
    http-request set-header X-Real-IP         %[src]
    http-request set-header X-Forwarded-For   %[src]

    # Capture the routing target in a var BEFORE replace-path mutates the path:
    # use_backend ACLs are evaluated after the rewrite, so a path_beg test on
    # the use_backend line would miss once the prefix has been stripped.
    acl p_hydra  path_beg /hydra/
    acl p_kratos path_beg /kratos/
    http-request set-var(txn.be) str(hydra)  if p_hydra
    http-request set-var(txn.be) str(kratos) if p_kratos

    # Strip the prefix before the upstream sees it (they serve at root).
    http-request replace-path ^/hydra/?(.*)  /\1 if p_hydra
    http-request replace-path ^/kratos/?(.*) /\1 if p_kratos

    use_backend be_hydra  if { var(txn.be) -m str hydra }
    use_backend be_kratos if { var(txn.be) -m str kratos }
    default_backend be_forseti

backend be_forseti
    server forseti 127.0.0.1:3000 check
backend be_hydra      # Hydra public; admin :4445 stays on loopback, never proxied
    server hydra  127.0.0.1:4444 check
backend be_kratos     # Kratos public; admin :4434 stays on loopback, never proxied
    server kratos 127.0.0.1:4433 check
```

`X-Forwarded-Proto: https` is mandatory: without it Hydra and Kratos emit `http://` URLs and drop `Secure` from cookies. The generated Hydra config trusts that header from loopback (`serve.tls.allow_termination_from`), so no `--dev` is needed in production. Never proxy the admin ports (4445 / 4434) or Forseti's internal listener (8081). If you add a `Content-Security-Policy`, it must allow Forseti's inline pre-paint theme script (see the operator guide).

**Quick test without a proxy:** set `dev?` to `#t` so the Ory daemons accept the https issuer over plain HTTP. Never use `dev?` in production.

**forseti-stack-configuration options:**

| Field | Default | Description |
|-------|---------|-------------|
| `base-url` | (required) | Public https origin; the issuer and all UI/redirect URLs derive from it |
| `manage-postgres?` | `#t` | Provision Postgres plus the kratos/hydra/forseti roles and databases |
| `secrets-directory` | `"/etc/forseti"` | Directory holding the per-service env-files |
| `smtp-connection-uri` | unset | Kratos courier SMTP URI for verification/recovery mail |
| `enable-dynamic-client-registration?` | `#f` | Enable Hydra DCR (`/oauth2/register`) for MCP-style clients |
| `audit-webhook?` | `#t` | Emit the Kratos audit web_hook to Forseti's internal listener |
| `dev?` | `#f` | Run the Ory daemons with `--dev` (no proxy needed); never in production |
| `kratos` / `hydra` / `forseti` | unset | Override the generated config file for a service with your own file-like |
| `postgres-host` | `"localhost"` | Reserved; peer auth uses the local socket |
| `kratos-log-file` / `hydra-log-file` / `forseti-log-file` | `/var/log/{kratos,hydra,forseti}.log` | Per-service log files |

**Standalone services:** Hydra and Kratos do not require Forseti. Use `ory-hydra-service-type` / `ory-kratos-service-type` directly with your own `config-file`; each runs a one-shot `migrate sql` (gated by `auto-migrate?`) before the serve daemon.

| Field (`ory-kratos` / `ory-hydra`) | Default | Description |
|-------|---------|-------------|
| `config-file` | (required) | The kratos.yml / hydra.yml |
| `environment-variables` | `'()` | Non-secret env (e.g. `DSN`) as a `(string . string)` alist |
| `environment-file` | `/etc/forseti/{kratos,hydra}.env` | Operator-supplied secrets file, sourced at start |
| `auto-migrate?` | `#t` | Run `migrate sql` as a one-shot before serve |
| `dev?` | `#f` | Append `--dev` to the serve command |
| `user` / `group` | `"ory"` | System user/group (the stack overrides these to the per-role names) |
| `state-directory` | `/var/lib/ory-{kratos,hydra}` | Daemon working directory |
| `log-file` | `/var/log/{kratos,hydra}.log` | Shepherd log file |
| `requirement` | `'(postgres)` | Extra shepherd requirements |

`forseti-service-type` mirrors these (default user/group `forseti`, `requirement` `'(hydra kratos postgres)`, no `auto-migrate?` / `dev?`). It reads its config via `FORSETI_CONFIG_PATH` and uses sqlite under its state directory unless you set `FORSETI_DATABASE__URL` in `forseti.env`.

**Service management:**

```bash
herd status                  # list all services
herd status forseti          # one daemon
sudo herd start postgres-roles   # one-shot: create roles + databases

# Verify the stack is wired
curl -s http://localhost:4444/.well-known/openid-configuration   # issuer == base-url
curl -s http://localhost:3000/healthz                            # forseti -> ok
```

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

### vpnmux

Runs the [vpnmux](https://github.com/franzos/vpnmux) daemon, a control loop that keeps Mullvad and Tailscale from conflicting at the netfilter and DNS layer. It continuously reconciles the system to a desired provider state by driving `nft`, `mullvad`, and `tailscale`. The service wires those binaries via `VPNMUX_NFT`/`VPNMUX_MULLVAD`/`VPNMUX_TAILSCALE`, creates `/run/vpnmux` and `/var/lib/vpnmux`, and installs the `vpnmux` CLI into the system profile.

By default the activation creates a `vpnmux` system group and chowns the runtime dirs to `root:vpnmux` with mode `02770` (setgid, group rwx), so members of the group can drive `vpnmux set`/`status` without `sudo`. Set `group` to `""` to keep the dirs root-only.

**Usage:**

```scheme
(use-modules (px services networking))

(service vpnmux-service-type)

;; Verbose logging in the shepherd log
(service vpnmux-service-type
         (vpnmux-configuration
          (log-level "debug")))

;; Root-only CLI (sudo required for set/status)
(service vpnmux-service-type
         (vpnmux-configuration
          (group "")))
```

Add yourself to the `vpnmux` group to use the CLI without sudo.

**Configuration options:**

| Field | Default | Description |
|-------|---------|-------------|
| `vpnmux` | `vpnmux` | The vpnmux package to run |
| `nftables` | `nftables` | Package providing `nft` (`VPNMUX_NFT`) |
| `mullvad` | `mullvad-vpn-desktop` | Package providing the `mullvad` CLI (`VPNMUX_MULLVAD`) |
| `tailscale` | `tailscale` | Package providing the `tailscale` CLI (`VPNMUX_TAILSCALE`) |
| `log-level` | unset | When set, exported as `VPNMUX_LOG` (e.g. `"debug"`) |
| `group` | `"vpnmux"` | System group permitted to drive the CLI without sudo. Empty string keeps dirs root-only. Exported as `VPNMUX_GROUP`. |

**After reconfiguration:**

```bash
vpnmux status                 # Show current state (no sudo if in vpnmux group)
vpnmux set mullvad            # Desired: Mullvad only
vpnmux set tailscale          # Desired: Tailscale only
vpnmux set mullvad tailscale  # Both, coexisting
vpnmux set                    # Neither (empty set)
herd status vpnmux            # Daemon status
```

### USBGuard

Runs `usbguard-daemon` to enforce a USB device authorization policy — a whitelist for USB devices that blocks BadUSB-style attacks. The generated `usbguard-daemon.conf` lives in the store; rules are kept at `/etc/usbguard/rules.conf` so they can be updated without a reconfigure.

**Usage:**

```scheme
(use-modules (px services usbguard))

;; Default: block everything not explicitly allowed, only root can use IPC
(service usbguard-service-type)

;; Allow members of the 'usbguard' group to manage rules via the CLI
(service usbguard-service-type
         (usbguard-configuration
          (ipc-allowed-groups '("usbguard"))))
```

Add yourself to the `usbguard` group (created by the service) to use the CLI without sudo.

**Before first start:**

With `implicit-policy-target` set to `'block` (the default), the daemon will block everything that isn't explicitly allowed — including devices already plugged in when it starts. Generate an initial policy from your currently-connected devices so you don't lock yourself out of your own keyboard:

```bash
sudo sh -c 'usbguard generate-policy > /etc/usbguard/rules.conf'
sudo herd restart usbguard
```

Run this once, with only the devices you trust plugged in. Review `/etc/usbguard/rules.conf` afterwards and trim anything you don't want whitelisted.

**Managing rules without reconfiguring:**

```bash
# Via the CLI — daemon persists changes to /etc/usbguard/rules.conf
sudo usbguard list-devices
sudo usbguard allow-device <id> -p       # -p = permanent
sudo usbguard append-rule 'allow id 1d6b:0002'

# Or edit the file directly
sudo $EDITOR /etc/usbguard/rules.conf
sudo herd restart usbguard
```

Changing fields in `usbguard-configuration` (policy targets, IPC allow-lists, audit settings) *does* require `guix system reconfigure` — those are baked into the store config.

**Configuration options:**

| Field | Default | Description |
|-------|---------|-------------|
| `package` | `usbguard` | The usbguard package to use |
| `rule-file` | `"/etc/usbguard/rules.conf"` | Persistent rules file |
| `rule-folder` | `"/etc/usbguard/rules.d/"` | Directory of additional rule files |
| `implicit-policy-target` | `'block` | Action for devices not matching any rule (`'allow`, `'block`, `'reject`) |
| `present-device-policy` | `'apply-policy` | How to treat devices already connected at daemon start |
| `present-controller-policy` | `'keep` | Same, for USB controllers |
| `inserted-device-policy` | `'apply-policy` | How to treat newly-inserted devices |
| `authorized-default` | `'none` | Default authorization for new devices (`'none`, `'all`, `'keep`, `'internal`) |
| `device-manager-backend` | `'uevent` | Backend (`'uevent` or `'umockdev`) |
| `ipc-allowed-users` | `'("root")` | Users permitted to use the IPC interface |
| `ipc-allowed-groups` | `'()` | Groups permitted to use the IPC interface |
| `audit-backend` | `'FileAudit` | `'FileAudit` or `'LinuxAudit` |
| `audit-file-path` | `"/var/log/usbguard/usbguard-audit.log"` | Audit log location |
| `hide-pii?` | `#f` | Strip serial numbers and descriptor hashes from audit entries |
| `log-file` | `"/var/log/usbguard.log"` | Shepherd log file |
| `auto-start?` | `#t` | Start the daemon automatically at boot |

**Service management:**

```bash
herd status usbguard    # Check status
herd restart usbguard   # Reload after editing rules.conf by hand
```

**Hardening:** The daemon is launched with `-C` (drop capabilities after startup) and `-W` (seccomp syscall allowlist). The D-Bus configuration and Polkit action from the `usbguard` package are registered automatically, so `usbguard-dbus` and desktop front-ends work without extra wiring.

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

## Known Issues

- **Channel modules shadowed by system profile** ([bug #74396](https://issues.guix.gnu.org/74396)): After `guix pull`, new package versions may not be available until you also run `guix system reconfigure`. Workaround: keep system and user guix in sync, or use `guix time-machine -C ~/.config/guix/channels.scm -- shell <package>`.

## Upstream

Packages here may be upstreamed to Guix if they meet Guix's free software requirements. Please include me in the copyright notice if you do.
