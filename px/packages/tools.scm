;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages tools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (px packages golang-xyz)
  #:use-module (px packages go)
  #:use-module (px self))

(define-public codex
  (package
    (name "codex")
    (version "0.132.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/openai/codex/releases/download/rust-v"
             version "/codex-"
             (match (or (%current-system) (%current-target-system))
               ("x86_64-linux" "x86_64-unknown-linux-musl")
               ("aarch64-linux" "aarch64-unknown-linux-musl")) ".tar.gz"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("x86_64-linux" "1cl3m36ia40fkiv66haf7wi8bg3nl8slbbbs35yisnzgwhp46r4b")
           ("aarch64-linux" "0h7lv458jq7man2ddg5x09xxa0q4m4xwsilpp97cmgqsnkzsi05f"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f
      #:install-plan
      #~`((,(string-append "codex-"
                           #$(match (or (%current-system) (%current-target-system))
                               ("x86_64-linux" "x86_64-unknown-linux-musl")
                               ("aarch64-linux" "aarch64-unknown-linux-musl")))
           "bin/codex"))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://github.com/openai/codex")
    (synopsis "AI coding agent from OpenAI")
    (description
     "Codex CLI is an AI-powered coding agent from OpenAI that runs locally
on your computer.  It assists with software development tasks directly within
a terminal environment, providing code suggestions, explanations, and
automated coding assistance.")
    (license license:asl2.0)))

(define-public bun
  (package
    (name "bun")
    (version "1.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/oven-sh/bun/releases/download/bun-v"
             version "/bun-linux-x64.zip"))
       (sha256
        (base32 "13w4gvgwrjq9bi3ddp53hgm3z399d8i2aqpcmsaqbw2mx2pf47lm"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f
      #:install-plan
      #~`(("bun" "bin/bun.real"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "unzip" "-q" (assoc-ref inputs "source"))
              (invoke "mv" "bun-linux-x64/bun" "bun")))
          (add-after 'install 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (glibc (assoc-ref inputs "glibc"))
                     (openssl (assoc-ref inputs "openssl")))
                (mkdir-p bin)
                (with-output-to-file (string-append bin "/bun")
                  (lambda _
                    (format #t "#!~a/bin/bash~%" (assoc-ref inputs "bash"))
                    (format #t "export LD_LIBRARY_PATH=~a/lib:~a/lib:$LD_LIBRARY_PATH~%"
                            glibc openssl)
                    (format #t "exec ~a/lib/ld-linux-x86-64.so.2 ~a/bin/bun.real \"$@\"~%"
                            glibc out)))
                (chmod (string-append bin "/bun") #o755)))))))
    (native-inputs (list unzip bash))
    (inputs `(("glibc" ,glibc)
              ("openssl" ,openssl)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://bun.sh")
    (synopsis "Fast JavaScript runtime, package manager, and bundler")
    (description
     "Bun is a modern JavaScript runtime built for speed and compatibility.
It serves as a drop-in replacement for Node.js while providing a unified
toolkit that includes a package manager and bundler, making it ideal for
building fast and scalable JavaScript applications.")
    (license license:expat)))

(define-public binsider
  (package
    (name "binsider")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/orhun/binsider")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gfw9g4852zqhhqwcmvrzffg4ywpsphsg4fjs85brriypllyjzaj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.88))
    (inputs
     (px-cargo-inputs 'binsider))
    (home-page "https://binsider.dev")
    (synopsis "TUI for analyzing ELF binaries")
    (description
     "Binsider is a terminal user interface for analyzing ELF binaries.  It
enables static and dynamic analysis, string extraction, library inspection,
and hexdump generation through an interactive command-line environment.")
    (license (list license:expat license:asl2.0))))

(define-public broot
  (package
    (name "broot")
    (version "1.56.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Canop/broot/archive/refs/tags/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ramwxpqcvjgf8f2gj71qad4ni714ryx0jxkf5nmymkn5hjy8yry"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #t))
    (inputs
     (px-cargo-inputs 'broot))
    (home-page "https://dystroy.org/broot")
    (synopsis "Modern tree-like file navigator and fuzzy searcher")
    (description
     "Broot is a command-line tool for navigating directory trees and managing
files.  It provides fast fuzzy searching, file preview capabilities, Git status
integration, and customizable panels.  Broot helps you quickly overview and
navigate large directory structures.")
    (license license:expat)))

(define-public wakatime-cli
  (package
    (name "wakatime-cli")
    (version "2.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/wakatime/wakatime-cli/releases/download/v"
             version "/wakatime-cli-linux-"
             (match (or (%current-system) (%current-target-system))
               ("x86_64-linux" "amd64")
               ("aarch64-linux" "arm64")
               ("i686-linux" "386")
               ("armhf-linux" "arm")) ".zip"))
       (sha256
        (base32 "1q5fcz4iz1i8m7hb9cyyp6d04m22nh477xfrh0a0cvv807rqj7v1"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("wakatime-cli" "bin/wakatime-cli"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "unzip" (assoc-ref inputs "source"))
              (rename-file
               #$(string-append "wakatime-cli-linux-"
                                (match (or (%current-system) (%current-target-system))
                                  ("x86_64-linux" "amd64")
                                  ("aarch64-linux" "arm64")
                                  ("i686-linux" "386")
                                  ("armhf-linux" "arm")))
               "wakatime-cli")
              (chmod "wakatime-cli" #o755)))
          (delete 'patchelf)
          (delete 'validate-runpath))))
    (native-inputs (list unzip))
    (supported-systems '("x86_64-linux" "aarch64-linux" "i686-linux" "armhf-linux"))
    (home-page "https://wakatime.com/")
    (synopsis "Command line interface to WakaTime")
    (description
     "WakaTime CLI is a command line interface used by all WakaTime text editor
plugins to track coding activity.  It provides automatic time tracking for
programmers, with dashboards showing metrics and insights about coding habits.")
    (license license:bsd-3)))

(define-public envstash
  (package
    (name "envstash")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "envstash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yl66gkmhpcvfqhp87dxp337kwz7lq400vgl71ggi2pvxdsrq81g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags
       '("--release" "--"
         "--skip=cli::commands::transport::tests::paste_0x0_round_trip")))
    (inputs
     (cons* sqlite (px-cargo-inputs 'envstash)))
    (home-page "https://github.com/franzos/envstash")
    (synopsis "Manage .env files across git branches with versioning")
    (description
     "Envstash is a CLI tool for managing .env files across git branches.  It
provides versioning, diffing, restore, and sharing of environment variables with
optional encryption using GPG or password-based AES-256-GCM.")
    (license license:gpl3)))

(define-public shelf
  (package
    (name "shelf")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/shelf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "017v6rsxf23h9534nhfbbxazqkhvbvlirzngx2fcnyw0gky9n64r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.88))
    (inputs
     (cons* sqlite (px-cargo-inputs 'shelf)))
    (home-page "https://github.com/franzos/shelf")
    (synopsis "CLI for cataloguing files by metadata-driven rules")
    (description
     "Shelf is a command-line tool that walks input folders, extracts file
metadata (EXIF, QuickTime/MP4, PDF info), and sorts files into a structured
destination via configurable templates.  It deduplicates by sha256 content hash
and tracks run state in SQLite so re-runs are cheap and deterministic.
Profile-driven TOML configuration targets photos, videos, documents, invoices,
or any files with extractable metadata.")
    (license license:gpl3)))

(define-public vatic
  (package
    (name "vatic")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/vatic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rpmij1pgfymhpx2zgyk67qiqkxz9d0772ccy3ifianjjvz6cfs1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f))
    (native-inputs (list pkg-config))
    (inputs
     (cons* openssl sqlite (px-cargo-inputs 'vatic)))
    (home-page "https://github.com/franzos/vatic")
    (synopsis "TOML-configured AI agent framework with scheduled jobs")
    (description
     "Vatic is a TOML-configured AI agent framework.  It runs prompts through
LLM backends like Claude CLI and Ollama on cron schedules or channel triggers,
with templated prompts, conversation memory, and multiple output targets
including Telegram, Matrix, and email.")
    (license license:gpl3)))

(define-public google-workspace-cli
  (package
    (name "google-workspace-cli")
    (version "0.22.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/googleworkspace/cli/releases/download/v"
             version "/google-workspace-cli-"
             (match (or (%current-system) (%current-target-system))
               ("x86_64-linux" "x86_64-unknown-linux-musl")
               ("aarch64-linux" "aarch64-unknown-linux-musl")) ".tar.gz"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("x86_64-linux" "0879hyfdm2ngsmwmwq0s8jkg3waa1ndpcpgk9wp8gaxiwkfp7d2d")
           ("aarch64-linux" "16liz5xpdy2czk655zh5c3k51a0ax7n4f2qkq87b2cj9a9izw077"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("gws" "bin/gws"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'patchelf)
          (delete 'validate-runpath))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://github.com/googleworkspace/cli")
    (synopsis "Command-line interface for Google Workspace")
    (description
     "Google Workspace CLI (@command{gws}) is a single command-line tool for
Drive, Gmail, Calendar, Sheets, Docs, Chat, Admin, and other Google Workspace
services.  Its command surface is dynamically built from the Google Discovery
Service, and it includes AI agent skills.")
    (license license:asl2.0)))

(define-public d2
  (package
    (name "d2")
    (version "0.7.1")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/terrastruct/d2")
                    (commit (string-append "v" version))
                    (sha (base32 "1i1fvy35rqjxvmpa2rlfx96j0bb1hf17xxml0pf6nhjaq8qjy435"))))
              (sha256
               (base32
                "1dcka1h312wqivamiyrvbkk3pkggqhqbnqdyp1scblcml9rd6jk7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "oss.terrastruct.com/d2"
      #:install-source? #f
      #:go go-1.25
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check))))
    (home-page "https://d2lang.com")
    (synopsis "Modern diagram scripting language")
    (description
     "D2 is a modern diagram scripting language that turns text into diagrams.
It supports flowcharts, sequence diagrams, class diagrams, and more with a
readable syntax.  D2 includes multiple layout engines and can output to SVG,
PNG, and PDF formats.")
    (license license:mpl2.0)))