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
  #:use-module (gnu packages compression)
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
  #:use-module (px self))

(define-public codex
  (package
    (name "codex")
    (version "0.98.0")
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
           ("x86_64-linux" "0h0d961m60ybashw2lnkxyynidj8zgxnffhvcm8mf5b425vpjrmj")
           ("aarch64-linux" "00a78qcafn8ha10c1brkd4xd0sh8fqzbw7a45r3iirqh92fcs3ib"))))))
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
           ("glibc" "gcc:lib" "libcap" "openssl")))))
    (inputs `(("glibc" ,glibc)
               ("gcc:lib" ,gcc "lib")
               ("libcap" ,libcap)
               ("openssl" ,openssl)))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://github.com/openai/codex")
    (synopsis "AI coding agent from OpenAI")
    (description
     "Codex CLI is an AI-powered coding agent from OpenAI that runs locally
on your computer.  It assists with software development tasks directly within
a terminal environment, providing code suggestions, explanations, and
automated coding assistance.")
    (license license:asl2.0)))

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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Canop/broot/archive/refs/tags/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wawx7axmyd1bga06davsv8hsyzymidb8jissiv75vf8a5h8ry4j"))))
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
    (version "1.139.1")
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
        (base32 "11jl7riqnn7c4bphrxc19x8gw2x87a9ymp2qkdil2syk24gfrjw6"))))
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
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "envstash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jn7dcc4q94s3z4pxpy1rqwisz8858vj1s13iddikp74537f8p64"))))
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

(define-public witr
  (package
    (name "witr")
    (version "0.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pranshuparmar/witr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05fd4b2scr1wkns4vkd3p67x8sw35yax2slfl75257i38nkqcp71"))))
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