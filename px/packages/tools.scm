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
    (version "0.117.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/openai/codex/releases/download/rust-v"
             version "/codex-"
             (match (or (%current-system) (%current-target-system))
               ("x86_64-linux" "x86_64-unknown-linux-gnu")
               ("aarch64-linux" "aarch64-unknown-linux-gnu")) ".tar.gz"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("x86_64-linux" "0pc9d2fl93k2mgx5mcyks0flhpym8ys54lsnswyxvs79hb7xxh85")
           ("aarch64-linux" "0ha3r0qcj1jw7bk0d3kcb1ackm26mvjz79m8m2sqwlqwzcmn6g5w"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan
      #~`((,(string-append "codex-"
                           #$(match (or (%current-system) (%current-target-system))
                               ("x86_64-linux" "x86_64-unknown-linux-gnu")
                               ("aarch64-linux" "aarch64-unknown-linux-gnu")))
           "bin/codex"))
      #:patchelf-plan
      #~`((,(string-append "codex-"
                           #$(match (or (%current-system) (%current-target-system))
                               ("x86_64-linux" "x86_64-unknown-linux-gnu")
                               ("aarch64-linux" "aarch64-unknown-linux-gnu")))
           ("glibc" "gcc:lib" "libcap" "openssl" "zlib")))))
    (inputs `(("glibc" ,glibc)
               ("gcc:lib" ,gcc "lib")
               ("libcap" ,libcap)
               ("openssl" ,openssl)
               ("zlib" ,zlib)))
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
    (version "1.3.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/oven-sh/bun/releases/download/bun-v"
             version "/bun-linux-x64.zip"))
       (sha256
        (base32 "1v876r4v8c31jvpss1sxbmgc29h32qahlx1qdxdg11pqba9vl4c6"))))
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
    (version "1.56.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Canop/broot/archive/refs/tags/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g4jm4wxb1h6ph5a8v3w17vlgm6r7vs1y69w7pgfa3bsbf2wsa8d"))))
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
    (version "2.0.14")
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
        (base32 "0fbr9psc0qbxwy7gbyzbam7w7bax5kw2ynybj2ba66wzmg6y5jmh"))))
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
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "envstash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09crlp1nai3ydpzmqz9w64j2ahjwcabj1jbqn35hbyr69j5r0g25"))))
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

(define-public witr
  (package
    (name "witr")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pranshuparmar/witr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q0gz67bxkbwg0x6kz2625wvwrbxlhi8w95pnsbxs94fbda4b8aw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/pranshuparmar/witr/cmd/witr"
      #:unpack-path "github.com/pranshuparmar/witr"
      #:go go-1.24
      #:build-flags
      #~(list (string-append
               "-ldflags=-X main.version=" #$version))))
    (home-page "https://github.com/pranshuparmar/witr")
    (synopsis "Explain why a process is running on Linux")
    (description
     "Witr (Why Is This Running) is a Linux CLI debugging tool that explains
the causal chain of why a process exists.  It traces process ancestry, maps
ports to processes, and identifies contexts like Git repositories, Docker
containers, and PM2 instances.")
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