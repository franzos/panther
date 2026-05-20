# Guix Packaging Guide

## Building Packages

Build packages in this repo:

```bash
guix build -L . -L ../guix binsider
```

---

# Comprehensive Guix Packaging Reference

## Table of Contents

1. [Package Structure Fundamentals](#package-structure-fundamentals)
2. [Build Systems](#build-systems)
3. [New Rust Packaging Model (2025)](#new-rust-packaging-model-2025)
4. [Common Workflows](#common-workflows)
5. [Input Types and Dependencies](#input-types-and-dependencies)
6. [Phases and Modifications](#phases-and-modifications)
7. [Best Practices](#best-practices)
8. [Examples](#examples)

---

## Package Structure Fundamentals

### Basic Package Definition

Every Guix package follows this structure:

```scheme
(define-public package-name
  (package
    (name "package-name")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://example.com/package-" version ".tar.gz"))
       (sha256
        (base32 "hash-here"))))
    (build-system gnu-build-system)  ; or cargo-build-system, etc.
    (arguments
     `(#:tests? #t
       #:configure-flags '("--enable-feature")))
    (native-inputs (list pkg-config))
    (inputs (list openssl zlib))
    (propagated-inputs (list python))
    (home-page "https://example.com")
    (synopsis "Short one-line description")
    (description "Longer multi-line description of the package.")
    (license license:expat)))
```

### Essential Fields

- **name**: Package name (string)
- **version**: Package version (string)
- **source**: Where to get the source code (origin block)
- **build-system**: How to build the package
- **synopsis**: One-line description (<80 chars)
- **description**: Detailed description
- **license**: Package license (from `(guix licenses)`)
- **home-page**: Project URL

### Source Methods

**URL Fetch:**
```scheme
(source
 (origin
   (method url-fetch)
   (uri (string-append "https://example.com/" version ".tar.gz"))
   (sha256 (base32 "hash"))))
```

**Git Fetch:**
```scheme
(source
 (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/user/repo")
         (commit (string-append "v" version))))
   (file-name (git-file-name name version))
   (sha256 (base32 "hash"))))
```

**Crate URI (Rust):**
```scheme
(source
 (origin
   (method url-fetch)
   (uri (crate-uri "package-name" version))
   (file-name (string-append name "-" version ".tar.gz"))
   (sha256 (base32 "hash"))))
```

### Getting Source Hashes

```bash
# For URLs
guix hash https://example.com/package-1.0.0.tar.gz

# For unpacked directories
guix hash -rx /path/to/source

# For git repos
guix hash -rx $(guix build --source package-name)
```

---

## Build Systems

Guix supports multiple build systems. Choose the appropriate one for your package.

### cargo-build-system (Rust)

For Rust applications (not libraries):

```scheme
(build-system cargo-build-system)
(arguments
 `(#:install-source? #f          ; Don't install source
   #:cargo-test-flags             ; Optional test flags
   '("--release" "--"
     "--skip=failing_test")))
(inputs (px-cargo-inputs 'package-name))  ; Panther wrapper for cargo-inputs
```

**Key Points:**
- Uses `px-cargo-inputs` (defined in `px/self.scm`) which wraps upstream `cargo-inputs` to resolve from `(px packages rust-crates)`
- Dependencies defined in separate `rust-crates.scm` file
- New model (2025): focuses on application-level builds
- The deprecated `#:cargo-inputs` argument style is being removed upstream (deadline: Dec 2026)

### gnu-build-system

For autotools-based projects (./configure && make && make install):

```scheme
(build-system gnu-build-system)
(arguments
 `(#:configure-flags
   '("--enable-shared"
     "--with-feature")
   #:make-flags
   '("CC=gcc")
   #:tests? #t
   #:phases
   (modify-phases %standard-phases
     (add-after 'unpack 'patch-source
       (lambda _
         (substitute* "src/main.c"
           (("/usr/bin") (which "bin"))))))))
```

**Standard Phases:**
1. `unpack` - Extract source
2. `patch-source-shebangs` - Fix script interpreters
3. `configure` - Run ./configure
4. `build` - Run make
5. `check` - Run make check
6. `install` - Run make install
7. `patch-shebangs` - Fix installed scripts

### python-build-system / pyproject-build-system

**Legacy python-build-system:**
```scheme
(build-system python-build-system)
(arguments
 `(#:tests? #f
   #:python ,python-3))
```

**Modern pyproject-build-system:**
```scheme
(build-system pyproject-build-system)
(native-inputs (list python-setuptools python-wheel))
```

### cmake-build-system

```scheme
(build-system cmake-build-system)
(arguments
 `(#:tests? #f
   #:configure-flags
   '("-DUSE_SYSTEM_LIBS=ON"
     "-DBUILD_TESTS=OFF")))
```

### Custom Build Systems

Example from panther/bazel:

```scheme
(build-system bazel-build-system)
(arguments
 (list
  #:bazel bazel-6.5
  #:build-targets '(list "//src:target")
  #:bazel-configuration
  #~(begin
      (setenv "CC" "clang")
      (setenv "CXX" "clang++"))))
```

---

## New Rust Packaging Model (2025)

### Key Changes

**Old Model:**
- Each crate = separate Guix package
- Massive duplication (~153k lines of package definitions)
- Complex dependency management

**New Model (2025):**
- Origin-based crate sources
- Build only on leaf applications
- Uses `cargo-inputs-from-lockfile`
- Reduced to ~42k lines
- Migrated 150+ apps with 3600+ dependencies in 2 weeks

### Workflow

**1. Generate Package Definition:**
```bash
guix import crate package-name > px/packages/rust-apps.scm
```

**2. Import Dependencies from Cargo.lock:**
```bash
guix import -i px/packages/rust-crates.scm \
      crate -f /path/to/Cargo.lock package-name
```

This creates entries in `rust-crates.scm`:

```scheme
(define rust-tokio-1.41.0
  (crate-source "tokio" "1.41.0"
                "1fwb4nm630hmy9cyl2ar6wxqckgvsakwhg1rhjza4is3a09k8pql"))
```

**3. Define cargo-inputs mapping:**
```scheme
(define-cargo-inputs lookup-cargo-inputs
  (package-name =>
    (list rust-tokio-1.41.0
          rust-serde-1.0.188
          ;; ... other dependencies
          )))
```

**4. Use in Application Package:**
```scheme
(define-public my-rust-app
  (package
    (name "my-rust-app")
    (version "1.0.0")
    (source ...)
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (px-cargo-inputs 'my-rust-app))  ; References lookup-cargo-inputs
    ...))
```

### Features

**Library Management:**
- Modify via snippets/patches
- Replace specific crates
- Delete unnecessary dependencies
- Handle complex build processes in `rust-sources.scm`

**Build System Improvements:**
- Directory inputs support
- Cargo workspace support
- New `check-for-pregenerated-files` phase
- Better automation

### IMPORTANT: Do NOT Manually Edit rust-crates.scm

`rust-crates.scm` is automatically managed by `guix import -i`. Do not manually add, remove, or reorder crate-source definitions or lookup-cargo-inputs mappings. If there are issues, regenerate using the import workflow.

**Exception:** After multiple imports, `crate-source` definitions may end up after `lookup-cargo-inputs`, causing "used before definition" errors. Moving them before the block is acceptable structural maintenance. The required structure is: all `crate-source` definitions first, `lookup-cargo-inputs` at the end.

---

## Common Workflows

### Packaging a New Rust Application

```bash
# 1. Prepare environment
guix shell rust rust:cargo cargo-audit cargo-license

# 2. Generate lockfile and check deps
cd /path/to/source
cargo generate-lockfile
cargo audit          # Check for security issues
cargo license        # Verify acceptable licenses

# 3. Import package template
guix import crate package-name > px/packages/rust-apps.scm

# 4. Import dependencies (-f = --lockfile, -i = insert into file)
guix import -i px/packages/rust-crates.scm \
      crate -f /path/to/Cargo.lock package-name

# 5. Edit package definition
# - Set correct version
# - Add home-page, synopsis, description
# - Configure arguments if needed

# 6. Build
guix build -L . package-name

# 7. Test
guix shell package-name -- package-name --version
```

### Packaging a Python Application

```bash
# 1. Import from PyPI
guix import pypi package-name > px/packages/python-xyz.scm

# 2. Edit and build
guix build -L . python-package-name
```

### Unbundling Dependencies

When a package bundles libraries:

```scheme
(source
 (origin
   ...
   (snippet
    #~(begin
        (use-modules (guix build utils))
        ;; Delete bundled libraries
        (delete-file-recursively "vendor/libffi")
        (delete-file-recursively "vendor/openssl")
        ;; Patch to use system libraries
        (substitute* "build.rs"
          (("vendor/") ""))))))
```

---

## Input Types and Dependencies

### native-inputs

Tools needed **during build** but not at runtime:
- pkg-config
- compilers (gcc, rust, clang)
- build tools (cmake, autoconf)
- test frameworks

```scheme
(native-inputs (list pkg-config cmake python-pytest))
```

### inputs

Runtime dependencies:
- Libraries (openssl, zlib)
- Executables the program calls
- Shared libraries

```scheme
(inputs (list openssl curl libffi))
```

### propagated-inputs

Dependencies that must be available to users of this package:
- Python modules imported by your module
- Header files needed by your headers
- Libraries linked by your library

```scheme
(propagated-inputs (list python-requests python-numpy))
```

### Special: cargo-inputs

For Rust packages using the new model:

```scheme
;; Upstream guix packages:
(inputs (cargo-inputs 'package-name))

;; Panther packages (use px-cargo-inputs from px/self.scm):
(inputs (px-cargo-inputs 'package-name))
```

`cargo-inputs` resolves from `(gnu packages rust-crates)`. The panther wrapper `px-cargo-inputs` resolves from `(px packages rust-crates)` instead.

---

## Phases and Modifications

### Standard Phase Operations

```scheme
(arguments
 `(#:phases
   (modify-phases %standard-phases
     ;; Delete a phase
     (delete 'configure)

     ;; Replace a phase
     (replace 'install
       (lambda* (#:key outputs #:allow-other-keys)
         (let ((out (assoc-ref outputs "out")))
           (install-file "binary" (string-append out "/bin")))))

     ;; Add before existing phase
     (add-before 'build 'set-environment
       (lambda _
         (setenv "CC" "gcc")))

     ;; Add after existing phase
     (add-after 'unpack 'patch-source
       (lambda _
         (substitute* "setup.py"
           (("/usr") (assoc-ref %outputs "out"))))))))
```

### Common Phase Modifications

**Patch Shebangs:**
```scheme
(add-after 'unpack 'patch-shebangs
  (lambda _
    (substitute* "script.sh"
      (("/bin/bash") (which "bash"))
      (("/usr/bin/env") (which "env")))))
```

**Skip Tests:**
```scheme
(arguments
 `(#:tests? #f))  ; Disable all tests

;; Or skip specific tests:
(arguments
 `(#:cargo-test-flags
   '("--release" "--"
     "--skip=test_network"
     "--skip=test_timing")))
```

**Custom Install:**
```scheme
(add-after 'build 'install
  (lambda* (#:key outputs #:allow-other-keys)
    (let* ((out (assoc-ref outputs "out"))
           (bin (string-append out "/bin"))
           (lib (string-append out "/lib")))
      (mkdir-p bin)
      (copy-file "target/release/binary"
                 (string-append bin "/binary"))
      (chmod (string-append bin "/binary") #o755))))
```

---

## Best Practices

### Security and Licensing

**Always check:**
```bash
cargo audit              # Rust security vulnerabilities
cargo license            # Verify acceptable licenses
```

**In package definition:**
```scheme
;; Be specific about licenses
(license license:expat)  ; MIT
(license license:asl2.0) ; Apache 2.0
(license (list license:expat license:asl2.0))  ; Dual licensed
```

### Unbundling

**Why:** Guix philosophy requires using system libraries

**How:**
```scheme
(snippet
 #~(begin
     (use-modules (guix build utils))
     ;; Remove vendored code
     (delete-file-recursively "vendor")
     ;; Patch build files
     (substitute* "Cargo.toml"
       (("vendor = .*") ""))))
```

**Note bundled sources in comments:**
```scheme
(define rust-js-sys-0.3.72
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.72" "hash"))
```

### Cross-Compilation

**Check target system:**
```scheme
#:phases
#~(modify-phases %standard-phases
    #$@(if (%current-target-system)
           ;; Cross-compiling
           #~((add-before 'build 'set-cross-env
                (lambda _
                  (setenv "TARGET" #$(%current-target-system)))))
           ;; Native build
           #~()))
```

### File Naming and Organization

Package files are organized by category, using kebab-case:
- **By domain**: `audio.scm`, `backup.scm`, `containers.scm`, `editors.scm`, `networking.scm`
- **By language** (for bindings/libraries): `golang-xyz.scm`, `python-xyz.scm`, `ruby.scm`
- **Special files**: `rust-crates.scm` (auto-managed), `rust.scm` (Rust toolchain)

Follow the same conventions as upstream guix (`gnu/packages/`). Add packages to an existing category file when possible; only create a new file if no existing category fits.

### Module Organization

**Package file header:**
```scheme
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Your Name <email@example.com>
;;;
;;; This file is part of GNU Guix / your channel.

(define-module (px packages category)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages rust))
```

**Import organization:**
1. License module (with prefix)
2. Guix core modules
3. Build system modules
4. Package modules (alphabetically)

### Testing

```bash
# Build package
guix build -L . package-name

# Try in isolated environment
guix shell package-name -- package-name --help

# Check for issues
guix lint package-name

# Verify dependencies
guix graph package-name | dot -Tpng > graph.png
```

### Checking for Updates

Use `scripts/check-versions.sh` to check for newer versions of packaged software. It supports GitHub releases, GitHub tags, and crates.io lookups. Set `GITHUB_TOKEN` for higher API rate limits.

```bash
./scripts/check-versions.sh
```

---

## Examples

### Example 1: Simple Rust Application

```scheme
(define-public aardvark-dns
  (package
    (name "aardvark-dns")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aardvark-dns" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1d2bs5lmijv6s3n71gqc986n1wy7ny9w74741njjix7932a7yd5f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (cargo-inputs 'aardvark-dns))
    (home-page "https://github.com/containers/aardvark-dns")
    (synopsis "Container-focused DNS A/AAAA record server")
    (description
     "Aardvark-dns is an authoritative DNS server for A/AAAA container
records.  It can forward other requests to configured resolvers.")
    (license license:asl2.0)))
```

### Example 2: Python Package with Patches

```scheme
(define-public python-pycapnp
  (package
    (name "python-pycapnp")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/capnproto/pycapnp/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32 "0n9hcivqia43508fpz5712k39xxzdcxm31kcps6s0rwdip2srqx9"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-3
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-source
                    (lambda _
                      (substitute* '("setup.py")
                        (("if need_build")
                         "if False")) #t))
                  (delete 'sanity-check))))
    (native-inputs (list python-pkgconfig python-cython capnproto))
    (propagated-inputs (list python-jinja2))
    (home-page "http://jparyani.github.io/pycapnp")
    (synopsis "Capability-based RPC and serialization system")
    (description "This is a python3 wrapping of the C++
implementation of the Cap'n Proto library.")
    (license license:gpl2+)))
```

### Example 3: Package with Git Source and Snippet

```scheme
(define-public alfis
  (package
    (name "alfis")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Revertron/Alfis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "189dqgcnl11fdmd6242h1pbawlq7jdm22zykc1kkcj1dv6s55nvs"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Use packaged version of web-view
                 (substitute* "Cargo.toml"
                   (("git = .*web-view\",") "version = \"*\",")
                   ((", git = .*ureq\"") "")
                   (("git = .*ecies-ed25519-ng.*version") "version"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags
       '("--release" "--"
         "--skip=dns::client::tests::test_tcp_client"
         "--skip=dns::client::tests::test_udp_client")))
    (native-inputs (list pkg-config))
    (inputs
     (cons* at-spi2-core
            gtk
            webkitgtk-with-libsoup2
            (cargo-inputs 'alfis)))
    (home-page "https://github.com/Revertron/Alfis")
    (synopsis "Alternative Free Identity System")
    (description
     "This project represents a minimal blockchain without cryptocurrency,
capable of sustaining any number of domain names in a bunch of original
alternative zones.")
    (license license:agpl3+)))
```

### Example 4: CMake Package

```scheme
(define-public cpr
  (package
    (name "cpr")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/whoshuu/cpr/archive/v" version
                           ".tar.gz"))
       (sha256
        (base32 "18w0v6jhjz05c844wgsb07cxp4bbmcw0jiz9ka4hjsn6g5s3rmx6"))))
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DUSE_SYSTEM_CURL=ON" "-DBUILD_CPR_TESTS=OFF")))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config))
    (inputs (list curl))
    (home-page "https://whoshuu.github.io/cpr/")
    (synopsis "C++ Requests: Curl for People")
    (description "C++ Requests is a simple wrapper around libcurl
inspired by the excellent Python Requests project.")
    (license license:expat)))
```

---

## Quick Reference

### Common Commands

```bash
# Import package template
guix import crate PACKAGE > file.scm
guix import pypi PACKAGE > file.scm

# Build package
guix build -L . PACKAGE
guix build -L . -L ../guix PACKAGE

# Install locally
guix package -L . -i PACKAGE

# Shell with package
guix shell -L . PACKAGE

# Lint package
guix lint PACKAGE

# Show package info
guix show PACKAGE

# Check dependencies
guix graph PACKAGE

# Get source hash
guix hash FILE
guix hash -rx DIRECTORY
```

### File Locations

- **Guix packages**: `gnu/packages/*.scm`
- **Panther packages**: `px/packages/*.scm`
- **Rust crates**: `px/packages/rust-crates.scm`
- **Rust apps**: `px/packages/*.scm`
- **Build systems**: `guix/build-system/*.scm`

### Getting Help

```bash
# Manual
info guix

# Package documentation
guix show PACKAGE

# Online
# - Guix manual: https://guix.gnu.org/manual/
# - Guix cookbook: https://guix.gnu.org/cookbook/
# - Mailing list: help-guix@gnu.org
```

---

## Home Services

Home services are building blocks of the Guix Home environment. Unlike system services, they manage user-level configuration, packages, and daemons.

### What Are Home Services?

A home service typically:
- Declares packages to install in the home profile
- Creates configuration files in `~/.config` (or `XDG_CONFIG_HOME`)
- Sets environment variables for login shells
- Optionally manages user-level daemons via Shepherd

**Important**: Home services don't necessarily require Shepherd. Many just install files or set environment variables.

### Basic Home Service Structure

```scheme
(define-module (px services myservice)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-myservice-configuration
            home-myservice-service-type))
```

### Creating a Simple Config File Service

**Example: Managing a text configuration file**

```scheme
(define-configuration/no-serialization home-myapp-configuration
  (package
   (package myapp)
   "The myapp package to use.")
  (config-content
   (string "default config")
   "Configuration file content."))

(define (home-myapp-files-service config)
  (list `(".config/myapp/config"
          ,(mixed-text-file "myapp-config"
                           (home-myapp-configuration-config-content config)))))

(define home-myapp-service-type
  (service-type
   (name 'home-myapp)
   (description "Manage myapp configuration.")
   (extensions
    (list (service-extension home-files-service-type
                            home-myapp-files-service)
          (service-extension home-profile-service-type
                            (lambda (config)
                              (list (home-myapp-configuration-package config))))))
   (default-value (home-myapp-configuration))))
```

**Usage in home configuration:**
```scheme
(service home-myapp-service-type
         (home-myapp-configuration
          (config-content "custom config here")))
```

### Creating a Home Shepherd Service

**Example: User daemon managed by home Shepherd**

```scheme
(define (home-myapp-shepherd-service config)
  (list (shepherd-service
         (documentation "Run myapp daemon")
         (provision '(myapp))
         (requirement '())
         (start #~(make-forkexec-constructor
                   (list #$(file-append myapp "/bin/myapp-daemon")
                         "--config" #$(home-myapp-config-file config))
                   #:log-file (string-append (or (getenv "XDG_LOG_HOME")
                                                (string-append (getenv "HOME")
                                                              "/.local/var/log"))
                                            "/myapp.log")))
         (stop #~(make-kill-destructor))
         (auto-start? #t))))

(define home-myapp-service-type
  (service-type
   (name 'home-myapp)
   (extensions
    (list (service-extension home-shepherd-service-type
                            home-myapp-shepherd-service)))
   (default-value (home-myapp-configuration))
   (description "Run myapp as a user daemon.")))
```

### Configuration Serialization

For services that need to generate config files from structured data:

```scheme
;; Define serialization functions
(define (serialize-string field value)
  (string-append (symbol->string field) " = \"" value "\"\n"))

(define (serialize-integer field value)
  (string-append (symbol->string field) " = " (number->string value) "\n"))

(define (serialize-boolean field value)
  (string-append (symbol->string field) " = " (if value "true" "false") "\n"))

;; Use in configuration
(define-configuration home-myapp-configuration
  (package
   (package myapp)
   "The myapp package.")
  (port
   (integer 8080)
   "Server port number.")
  (enable-feature
   (boolean #t)
   "Whether to enable the feature."))

(define (home-myapp-config-file config)
  (mixed-text-file "myapp.conf"
                   (serialize-configuration config home-myapp-configuration-fields)))
```

### Service Extension Examples

**Extending shell environment:**
```scheme
(service-extension home-shell-profile-service-type
                   (lambda (config)
                     (list (plain-file "myapp-env.sh"
                                      "export MYAPP_HOME=$HOME/.myapp"))))
```

**Extending XDG config files:**
```scheme
(service-extension home-xdg-configuration-files-service-type
                   (lambda (config)
                     (list `("myapp/config.toml"
                            ,(myapp-config-file config)))))
```

**Extending mcron (scheduled jobs):**
```scheme
(service-extension home-mcron-service-type
                   (lambda (config)
                     (list #~(job '(next-hour (range 0 24 6))
                                 (string-append #$myapp "/bin/sync")))))
```

### Using simple-service

For quick extensions without defining a full service type:

```scheme
;; Add packages to home profile
(simple-service 'my-extra-packages
                home-profile-service-type
                (list vim git htop))

;; Add environment variables
(simple-service 'my-env-vars
                home-environment-variables-service-type
                '(("EDITOR" . "vim")
                  ("PAGER" . "less")))

;; Add a config file
(simple-service 'my-config-file
                home-xdg-configuration-files-service-type
                (list `("myapp/settings.conf"
                       ,(local-file "./myapp-settings.conf"))))
```

### Real-World Example: SSH Agent Service

From `/home/franz/git/guix/gnu/home/services/ssh.scm`:

```scheme
(define-configuration home-ssh-agent-configuration
  (openssh
   (package openssh)
   "The OpenSSH package to use.")
  (socket-directory
   (string "~/.ssh")
   "Directory to store the socket.")
  (extra-options
   (list-of-strings '())
   "Extra command-line options for ssh-agent."))

(define (home-ssh-agent-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Run the ssh-agent.")
    (provision '(ssh-agent))
    (start #~(make-forkexec-constructor
              (cons* #$(file-append (home-ssh-agent-openssh config)
                                   "/bin/ssh-agent")
                     "-D" "-a" (string-append
                               #$(home-ssh-agent-socket-directory config)
                               "/agent")
                     '#$(home-ssh-agent-extra-options config))
              #:environment-variables
              (cons (string-append "SSH_AUTH_SOCK="
                                  #$(home-ssh-agent-socket-directory config)
                                  "/agent")
                    (default-environment-variables))))
    (stop #~(make-kill-destructor)))))

(define home-ssh-agent-service-type
  (service-type
   (name 'home-ssh-agent)
   (extensions
    (list (service-extension home-shepherd-service-type
                            home-ssh-agent-shepherd-service)
          (service-extension home-environment-variables-service-type
                            (lambda (config)
                              `(("SSH_AUTH_SOCK" . ,(string-append
                                                    (home-ssh-agent-socket-directory config)
                                                    "/agent")))))))
   (default-value (home-ssh-agent-configuration))
   (description "Run and configure the SSH authentication agent.")))
```

### Discovery and Testing

```bash
# Discover available home services
guix home search KEYWORD

# List all home service types
guix home search ""

# Build home environment
guix home reconfigure home-config.scm

# Test in container
guix home container home-config.scm
```

### Home Service File Locations

- **Guix home services**: `/home/franz/git/guix/gnu/home/services/*.scm`
- **Panther home services**: Create in `px/services/` (if adding custom services)

### Key Home Service Types to Extend

- `home-profile-service-type` - Install packages
- `home-files-service-type` - Files in `$HOME`
- `home-xdg-configuration-files-service-type` - Files in `~/.config`
- `home-environment-variables-service-type` - Environment variables
- `home-shell-profile-service-type` - Shell profile additions
- `home-shepherd-service-type` - User daemons
- `home-mcron-service-type` - Scheduled jobs

---

## System Services

System services integrate with Guix System to provide daemons, configurations, system accounts, and system-wide settings.

### Service Architecture

A Guix service type consists of:

1. **Name**: Symbol for identification and debugging
2. **Extensions**: List of `service-extension` objects
3. **Default value**: Optional default configuration
4. **Compose** (for extensible services): Combines multiple extensions
5. **Extend** (for extensible services): Integrates extensions into configuration
6. **Description**: Documentation string

### Basic System Service Structure

```scheme
(define-module (px services myservice)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (myservice-configuration
            myservice-configuration?
            myservice-service-type))
```

### Defining a Configuration Record

```scheme
(define-configuration myservice-configuration
  (package
   (package myservice)
   "The myservice package to use.")
  (port
   (integer 8080)
   "TCP port to listen on.")
  (user
   (string "myservice")
   "User account to run as.")
  (group
   (string "myservice")
   "Group to run as.")
  (config-file
   maybe-string
   "Path to configuration file.")
  (log-file
   (string "/var/log/myservice.log")
   "Log file location.")
  (auto-start?
   (boolean #t)
   "Whether to start automatically."))
```

### Creating a Shepherd Service

```scheme
(define (myservice-shepherd-service config)
  (match-record config <myservice-configuration>
    (package port user group log-file auto-start?)
    (list (shepherd-service
           (documentation "Run myservice daemon.")
           (provision '(myservice))
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     (list #$(file-append package "/bin/myservice")
                           "--port" #$(number->string port)
                           "--user" #$user)
                     #:user #$user
                     #:group #$group
                     #:log-file #$log-file
                     #:environment-variables
                     (list (string-append "HOME=/var/lib/myservice"))))
           (stop #~(make-kill-destructor))
           (auto-start? auto-start?)
           (actions
            (list (shepherd-configuration-action
                   (myservice-config-file config))))))))
```

### Creating System Accounts

```scheme
(define (myservice-accounts config)
  (match-record config <myservice-configuration> (user group)
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "myservice daemon user")
           (home-directory "/var/lib/myservice")
           (shell (file-append shadow "/sbin/nologin"))))))
```

### Activation Script

```scheme
(define (myservice-activation config)
  (match-record config <myservice-configuration> (user group)
    #~(begin
        (use-modules (guix build utils))
        ;; Create necessary directories
        (mkdir-p "/var/lib/myservice")
        (mkdir-p "/var/log")
        ;; Set permissions
        (let ((user (getpwnam #$user)))
          (chown "/var/lib/myservice"
                 (passwd:uid user)
                 (passwd:gid user))))))
```

### Defining the Service Type

```scheme
(define myservice-service-type
  (service-type
   (name 'myservice)
   (description "Run and configure myservice daemon.")
   (extensions
    (list (service-extension shepherd-root-service-type
                            myservice-shepherd-service)
          (service-extension account-service-type
                            myservice-accounts)
          (service-extension activation-service-type
                            myservice-activation)))
   (default-value (myservice-configuration))))
```

### Service with Configuration File Generation

```scheme
(define (serialize-myservice-config config)
  (match-record config <myservice-configuration>
    (port user group)
    (mixed-text-file "myservice.conf"
                     "# Generated by Guix\n"
                     "port = " (number->string port) "\n"
                     "user = " user "\n"
                     "group = " group "\n")))

(define (myservice-config-file config)
  (serialize-myservice-config config))

;; Use in shepherd service
(define (myservice-shepherd-service config)
  (list (shepherd-service
         ;; ...
         (start #~(make-forkexec-constructor
                   (list #$(file-append (myservice-configuration-package config)
                                       "/bin/myservice")
                         "--config" #$(myservice-config-file config))
                   ;; ...
                   )))))
```

### Extensible Service Type

For services that can be extended by others:

```scheme
(define myservice-service-type
  (service-type
   (name 'myservice)
   (extensions
    (list (service-extension shepherd-root-service-type
                            myservice-shepherd-service)))
   ;; Compose: combine extensions from other services
   (compose concatenate)
   ;; Extend: integrate composed extensions
   (extend (lambda (config extensions)
             (myservice-configuration
              (inherit config)
              (extra-config (append (myservice-configuration-extra-config config)
                                   extensions)))))
   (default-value (myservice-configuration))
   (description "Extensible myservice.")))
```

### Real-World Example: Apcupsd Service

From `/home/franz/git/guix/gnu/services/power.scm`:

```scheme
(define-configuration apcupsd-configuration
  (apcupsd (package apcupsd) "The apcupsd package.")
  (shepherd-service-name
   (symbol 'apcupsd)
   "Shepherd service name."
   empty-serializer)
  (auto-start?
   (boolean #t)
   "Auto-start?"
   empty-serializer)
  (pid-file
   (string "/run/apcupsd.pid")
   "PID file location."
   empty-serializer)
  (name
   maybe-string
   "UPS name.")
  (cable
   (enum-cable 'usb)
   "Cable type.")
  (type
   (enum-type 'usb)
   "UPS type.")
  ;; ... more fields ...
  )

(define (apcupsd-shepherd-services config)
  (match-record config <apcupsd-configuration>
    (apcupsd pid-file debug-level run-dir
     shepherd-service-name auto-start?)
    (let ((config-file (apcupsd-config-file config)))
      (list
       (shepherd-service
        (documentation "Run the apcupsd daemon.")
        (requirement '(user-processes))
        (provision (list shepherd-service-name))
        (auto-start? auto-start?)
        (start #~(make-forkexec-constructor
                  '(#$(file-append apcupsd "/sbin/apcupsd")
                    "-b" "-f" #$config-file
                    "-P" #$pid-file
                    "-d" #$(number->string debug-level))
                  #:log-file #$(format #f "/var/log/~a.log" shepherd-service-name)
                  #:environment-variables
                  (cons* (string-append "GUIX_APCUPSD_CONF=" #$config-file)
                        (string-append "GUIX_APCUPSD_POWERFAIL_FILE="
                                      #$run-dir "/powerfail")
                        (default-environment-variables))))
        (stop #~(make-kill-destructor))
        (actions (list (shepherd-configuration-action config-file))))))))

(define (apcupsd-activation config)
  (match-record config <apcupsd-configuration> (run-dir)
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$(string-append run-dir "/lock")))))

(define (apcupsd-pam-extensions config)
  (list (pam-extension
         (transformer
          (lambda (pam)
            (pam-service
             (inherit pam)
             (auth (cons pam-nologin (pam-service-auth pam)))))))))

(define apcupsd-service-type
  (service-type
   (name 'apcupsd)
   (description "Configure and start apcupsd.")
   (extensions (list (service-extension activation-service-type
                                        apcupsd-activation)
                     (service-extension shepherd-root-service-type
                                        apcupsd-shepherd-services)
                     (service-extension pam-root-service-type
                                        apcupsd-pam-extensions)))
   (default-value (apcupsd-configuration))))
```

### Using simple-service for System Extensions

```scheme
;; Add packages to system profile
(simple-service 'my-system-packages
                profile-service-type
                (list vim git htop))

;; Add static networking
(simple-service 'my-static-network
                static-networking-service-type
                (list (static-networking
                       (addresses (list (network-address
                                        (device "eth0")
                                        (value "10.0.0.10/24"))))
                       (routes (list (network-route
                                     (destination "default")
                                     (gateway "10.0.0.1"))))
                       (name-servers '("8.8.8.8")))))

;; Add a shepherd service directly
(simple-service 'my-custom-daemon
                shepherd-root-service-type
                (list (shepherd-service
                       (provision '(my-daemon))
                       (start #~(make-forkexec-constructor
                                 '("/usr/local/bin/my-daemon")))
                       (stop #~(make-kill-destructor)))))
```

### Common Service Extensions

**1. Shepherd (daemons):**
```scheme
(service-extension shepherd-root-service-type
                   myservice-shepherd-service)
```

**2. Accounts:**
```scheme
(service-extension account-service-type
                   myservice-accounts)
```

**3. Activation (setup scripts):**
```scheme
(service-extension activation-service-type
                   myservice-activation)
```

**4. PAM (authentication):**
```scheme
(service-extension pam-root-service-type
                   myservice-pam-extension)
```

**5. Privileged programs (setuid):**
```scheme
(service-extension privileged-program-service-type
                   (lambda (config)
                     (list (file-like->setuid-program
                            (file-append package "/bin/tool")))))
```

**6. Udev rules:**
```scheme
(service-extension udev-service-type
                   (lambda (config)
                     (list package)))  ;; package provides udev rules
```

**7. D-Bus:**
```scheme
(service-extension dbus-root-service-type
                   (lambda (config)
                     (list package)))
```

**8. Profile (system packages):**
```scheme
(service-extension profile-service-type
                   (lambda (config)
                     (list package)))
```

### Shepherd Service Actions

Custom actions for `herd` command:

```scheme
(shepherd-service
 ;; ...
 (actions
  (list (shepherd-action
         (name 'reload)
         (documentation "Reload configuration without restart.")
         (procedure
          #~(lambda (running)
              (if running
                  (begin
                    (kill (car running) SIGHUP)
                    (format #t "Configuration reloaded.~%"))
                  (format #t "Service not running.~%")))))
        (shepherd-action
         (name 'status)
         (documentation "Show service status.")
         (procedure
          #~(lambda (running)
              (if running
                  (format #t "Service running with PID ~a~%" (car running))
                  (format #t "Service stopped~%"))))))))
```

**Usage:**
```bash
herd reload myservice
herd status myservice
```

### Testing Services

```bash
# Build system with new service
guix system build config.scm

# Test in VM
guix system vm config.scm

# Reconfigure running system
sudo guix system reconfigure config.scm

# Check service status
sudo herd status myservice
sudo herd doc myservice
```

### System Service File Locations

- **Guix system services**: `/home/franz/git/guix/gnu/services/*.scm`
- **Nonguix services**: `/home/franz/git/nonguix/nongnu/services/*.scm`
- **Panther services**: `px/services/*.scm`

### Key System Service Types to Extend

- `shepherd-root-service-type` - System daemons
- `account-service-type` - User/group accounts
- `activation-service-type` - Activation scripts
- `pam-root-service-type` - PAM configuration
- `profile-service-type` - System packages
- `etc-service-type` - Files in `/etc`
- `udev-service-type` - Udev rules
- `dbus-root-service-type` - D-Bus services
- `privileged-program-service-type` - Setuid programs
- `static-networking-service-type` - Network configuration

---

## Resources

### Official Documentation
- [Guix Manual](https://guix.gnu.org/manual/)
- [Guix Cookbook](https://guix.gnu.org/cookbook/)
- [New Rust Packaging Model (2025)](https://guix.gnu.org/en/blog/2025/a-new-rust-packaging-model/)
- [Service Reference](https://guix.gnu.org/manual/en/html_node/Service-Reference.html)
- [Shepherd Services](https://guix.gnu.org/manual/en/html_node/Shepherd-Services.html)
- [Home Services](https://guix.gnu.org/manual/en/html_node/Home-Services.html)

### Channel Configuration

This repo is a Guix channel (`pantherx`) with a dependency on `nonguix`. See `.guix-channel` for details.

### Commit Message Format

Follow the established pattern:

```
px: <package-name>: <action>
```

Examples:
- `px: binsider: Added`
- `px: ollama: Update to v0.18.1`
- `px: papers: Fix build with newer glib`
- `px: documentation: Remove`

### Repositories
- Guix: `/home/franz/git/guix/gnu/packages/`
- Guix Services: `/home/franz/git/guix/gnu/services/`
- Guix Home Services: `/home/franz/git/guix/gnu/home/services/`
- Nonguix: `/home/franz/git/nonguix/nongnu/packages/`
- Panther: `/home/franz/git/panther/px/packages/`

### Key Blog Posts
- [Packaging Tutorial](https://guix.gnu.org/cookbook/en/html_node/Packaging-Tutorial.html)
- [Packaging Rust Crates](https://guix.gnu.org/cookbook/en/html_node/Packaging-Rust-Crates.html)
- [Common Rust Workflow](https://guix.gnu.org/cookbook/en/html_node/Common-Workflow-for-Rust-Packaging.html)
- [GNU Shepherd User Services](https://guix.gnu.org/en/blog/2020/gnu-shepherd-user-services/)
