;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages tools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
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
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (px packages golang-xyz)
  #:use-module (px packages go)
  #:use-module (px packages rust)
  #:use-module (px self))

(define-public codex
  (package
    (name "codex")
    (version "0.152.0")
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
           ("x86_64-linux" "04calb4wwzdzr7jj8as6n7dp5zmnjwkwwmmdxpcsrdf5sg9l5y85")
           ("aarch64-linux" "131fijq4jgagbwifjlfjdn4xz26kh0yjlkb0qhna9j03cm46pnip"))))))
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

;; Biome pulls the React compiler crates straight from the react monorepo at a
;; pinned revision; they have no crates.io release.
(define %react-compiler-commit "e71a6393e66b0d2add46ba2b2c5db563a0563828")

(define %react-compiler-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/react/react")
          (commit %react-compiler-commit)))
    ;; Doubles as the input label the build phases look up.
    (file-name "react-compiler-source")
    (sha256
     (base32 "1m6w2aa4jhxnzm0gl13ysnkddlxrqyj1ixqgmdlgsqcnvx592g1m"))))

(define-public biome
  (package
    (name "biome")
    (version "2.5.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/biomejs/biome")
             (commit (string-append "@biomejs/biome@" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nlf563lzy8sshvf8vrw3ac7msdyijf8vjslq6spz3xb6qj42g5z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.95
       #:cargo-install-paths '("crates/biome_cli")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-react-compiler-deps
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Kept outside the source tree so cargo does not expect these
             ;; crates to be members of biome's workspace.
             (copy-recursively
              (string-append (assoc-ref inputs "react-compiler-source")
                             "/compiler/crates")
              "../react-compiler-crates")
             (for-each
              (lambda (crate)
                (substitute* "crates/biome_react_compiler/Cargo.toml"
                  (((string-append "^" crate "( *)= \\{ git = [^}]*\\}") _ spaces)
                   (string-append crate spaces
                                  "= { path = \"../../../react-compiler-crates/"
                                  crate "\" }"))))
              '("react_compiler" "react_compiler_ast" "react_compiler_hir"))))
         (add-before 'build 'set-build-env
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The crate version is "0.0.0"; the real version is injected
             ;; via BIOME_VERSION, mirroring upstream's release CI.
             (setenv "BIOME_VERSION" ,version)
             ;; Link the system jemalloc; the bundled one needs /bin/sh.
             (setenv "CARGO_FEATURE_UNPREFIXED_MALLOC_ON_SUPPORTED_PLATFORMS" "1")
             (setenv "JEMALLOC_OVERRIDE"
                     (string-append (assoc-ref inputs "jemalloc")
                                    "/lib/libjemalloc.so")))))))
    (native-inputs (list pkg-config))
    (inputs (cons* jemalloc zlib %react-compiler-source
                   (px-cargo-inputs 'biome_cli)))
    (home-page "https://biomejs.dev")
    (synopsis "Fast formatter and linter for web projects")
    (description
     "Biome is a toolchain for web projects that formats and lints JavaScript,
TypeScript, JSX, TSX, JSON, CSS, GraphQL, and HTML.  It aims to be a fast,
single-binary replacement for tools such as Prettier and ESLint.")
    (license (list license:expat license:asl2.0))))

(define-public bun
  (package
    (name "bun")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/oven-sh/bun/releases/download/bun-v"
             version "/bun-linux-"
             (match (or (%current-system) (%current-target-system))
               ("x86_64-linux" "x64")
               ("aarch64-linux" "aarch64")) ".zip"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("x86_64-linux" "0lp45zljagwcv1l2jv7mi3a1j6hsrsr838m0mikvbj1sp1gzn0rd")
           ("aarch64-linux" "03pdivjkbvf8lfpbv263n8qkwkprzxqggrng7fwkx631x0p366jb"))))))
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
              (invoke "mv"
                      (string-append
                       "bun-linux-"
                       #$(match (or (%current-system) (%current-target-system))
                           ("x86_64-linux" "x64")
                           ("aarch64-linux" "aarch64"))
                       "/bun")
                      "bun")))
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
                    (format #t "exec ~a/lib/~a ~a/bin/bun.real \"$@\"~%"
                            glibc
                            #$(match (or (%current-system) (%current-target-system))
                                ("x86_64-linux" "ld-linux-x86-64.so.2")
                                ("aarch64-linux" "ld-linux-aarch64.so.1"))
                            out)))
                (chmod (string-append bin "/bun") #o755)))))))
    (native-inputs (list unzip bash))
    (inputs `(("glibc" ,glibc)
              ("openssl" ,openssl)))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
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
    (version "1.59.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Canop/broot/archive/refs/tags/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0drwcqj6ik1gc71pal6qljsqa7gmzqrrysv9gyp3p5gk5s92kjv1"))))
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
    (version "2.26.0")
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
        (base32 "0yy9q1ycrm0g6n2qgz6ljilnmm9190jl7yz9l9n82jk0ddx3d5g6"))))
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

(define-public stripe-cli
  (package
    (name "stripe-cli")
    (version "1.50.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/stripe/stripe-cli/releases/download/v"
             version "/stripe_" version "_linux_"
             (match (or (%current-system) (%current-target-system))
               ("x86_64-linux" "x86_64")
               ("aarch64-linux" "arm64")) ".tar.gz"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("x86_64-linux"
            "1ymr0p5ikkblm9q9lpd57wjdm1c8mllmgzc9dq73w7f9shihy0kx")
           ("aarch64-linux"
            "0nihils8ccqipzgp30h2asv461v709skn4l6kfp50zxsiiiw3dbg"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("stripe" "bin/stripe"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "tar" "xzf" (assoc-ref inputs "source"))
              (chmod "stripe" #o755)))
          (delete 'patchelf)
          (delete 'validate-runpath))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://stripe.com/docs/stripe-cli")
    (synopsis "Command-line interface for Stripe")
    (description
     "The Stripe CLI helps build, test, and manage a Stripe integration from
the terminal.  It can tail API request logs, trigger and forward webhook
events to a local server, make test-mode API calls, and manage Stripe
resources.  This package installs the upstream statically linked binary.")
    (license license:asl2.0)))

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
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/shelf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12x4k5qndzbhpz73cwy2c4flfji0cf94zkp86qdlgbirsmfg526i"))))
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

(define-public google-cloud-cli
  (package
    (name "google-cloud-cli")
    (version "582.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://storage.googleapis.com/cloud-sdk-release/"
                           "google-cloud-cli-" version "-linux-x86_64.tar.gz"))
       (sha256
        (base32 "19d8yr6c9rr519jg4mzjf18qm53a39c1mlar6zhmm7dw44xcl5z9"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:install-plan
      #~'(("." "share/google-cloud-sdk")
          ("completion.bash.inc" "share/bash-completion/completions/gcloud"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'validate-runpath)
          (add-after 'unpack 'unbundle-python
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Drop the prebuilt CPython shipped for generic GNU/Linux and
              ;; point the launcher preamble at the one from the store.  A
              ;; user-set CLOUDSDK_PYTHON still wins.
              (delete-file-recursively "platform/bundledpythonunix")
              (substitute* '("bin/bq"
                             "bin/docker-credential-gcloud"
                             "bin/gcloud"
                             "bin/git-credential-gcloud.sh"
                             "bin/gsutil"
                             "bin/java_dev_appserver.sh")
                (("primary_python=python3\\.14")
                 (string-append "primary_python="
                                (search-input-file inputs "/bin/python3"))))))
          (add-after 'unpack 'disable-component-manager
            (lambda _
              ;; Components are installed into a read-only store directory,
              ;; so the built-in updater can never work.
              (substitute* "lib/googlecloudsdk/core/config.json"
                (("\"disable_updater\": false")
                 "\"disable_updater\": true"))))
          (add-after 'install 'wrap-launchers
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((sdk (string-append #$output "/share/google-cloud-sdk"))
                     (bin (string-append #$output "/bin"))
                     (scripts '("bq"
                                "docker-credential-gcloud"
                                "gcloud"
                                "git-credential-gcloud.sh"
                                "gsutil")))
                (for-each (lambda (prog)
                            ;; The preamble shells out to readlink, dirname
                            ;; and uname to find its own install directory.
                            (wrap-program (string-append sdk "/bin/" prog)
                              `("PATH" ":" prefix
                                (,(dirname (search-input-file inputs
                                                              "/bin/uname"))))))
                          scripts)
                (mkdir-p bin)
                (for-each (lambda (prog)
                            (symlink (string-append sdk "/bin/" prog)
                                     (string-append bin "/" prog)))
                          (cons "gcloud-crc32c" scripts))))))))
    (inputs (list bash-minimal coreutils-minimal python))
    (supported-systems '("x86_64-linux"))
    (home-page "https://cloud.google.com/cli")
    (synopsis "Command-line interface for Google Cloud")
    (description
     "The Google Cloud CLI provides @command{gcloud} for managing Google Cloud
resources and authenticating against Google APIs, alongside @command{gsutil}
for Cloud Storage and @command{bq} for BigQuery.  This is the upstream binary
release with the bundled Python interpreter replaced by the one from Guix; the
component manager is disabled, so extra components are not available.")
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

(define-public mdbook
  (package
    (name "mdbook")
    (version "0.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rust-lang/mdBook")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r8na6dy8vcgxvn504l9z52gqfgi5rykjhhplpzx2vz92cbhrdfm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       ;; These snapshot tests compare exact stderr output; the vendored
       ;; build creates both .cargo/config and .cargo/config.toml, which
       ;; makes cargo print an extra "using .cargo/config" warning that
       ;; breaks the snapshot comparison.  Not a functional failure.
       #:cargo-test-flags
       '("--"
         "--skip=preprocessor::failing_preprocessor"
         "--skip=preprocessor::nop_preprocessor")))
    (inputs
     (px-cargo-inputs 'mdbook))
    (home-page "https://rust-lang.github.io/mdBook/")
    (synopsis "Create books from Markdown files")
    (description
     "mdBook is a utility to create modern online books from Markdown files,
similar to Gitbook.  It is used to create the Rust standard library
documentation, as well as many other books and manuals.")
    (license license:mpl2.0)))

(define-public cargo-sweep
  (package
    (name "cargo-sweep")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-sweep" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1asipxcdaxqq8v3w2c54cgn80iy5jgnxp2q5vpnm5cl4398idiyf"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; cargo-sweep shells out to `cargo metadata`; without cargo on
          ;; PATH it skips every project it finds.
          (add-after 'install 'wrap-cargo
            (lambda _
              (wrap-program (string-append #$output "/bin/cargo-sweep")
                `("PATH" ":" suffix (,(string-append #$rust:cargo "/bin")))))))))
    (inputs
     (cons bash-minimal (px-cargo-inputs 'cargo-sweep)))
    (home-page "https://github.com/holmgr/cargo-sweep")
    (synopsis "Clean unused build files created by Cargo")
    (description
     "cargo-sweep removes stale artifacts from Cargo target directories,
selecting them by age, by installed toolchain, or by shrinking the directory
below a size limit.  It can walk a directory tree and sweep every Cargo
project it finds.")
    (license license:expat)))

(define-public crunch
  (package
    (name "crunch")
    (version "0.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crunch-app" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "048imydbq50926qnyq0a6fa7w0bh9s5avx9lnq7rpi4nq8idjbdm"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; crunch shells out to `cargo` locally, and to `ssh`/`rsync` to
          ;; reach the remote build host; without them on PATH it fails.
          (add-after 'install 'wrap-runtime-deps
            (lambda _
              (wrap-program (string-append #$output "/bin/crunch")
                `("PATH" ":" suffix
                  (,(string-append #$rust:cargo "/bin")
                   ,(string-append #$openssh "/bin")
                   ,(string-append #$rsync "/bin")))))))))
    (inputs
     (cons bash-minimal (px-cargo-inputs 'crunch-app)))
    (home-page "https://github.com/liamaharon/crunch")
    (synopsis "Drop-in @command{cargo} replacement for remote compilation")
    (description
     "crunch is a drop-in @command{cargo} replacement that offloads Rust
compilation to a remote server.  It syncs the local project to the remote
host over @command{rsync}, runs the requested @command{cargo} command there
over @command{ssh}, and copies results back, cutting local compile times.")
    (license license:expat)))