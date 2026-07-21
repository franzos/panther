;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages ai)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix licenses)
  #:use-module (gnu packages rust)
  #:use-module (px self))

(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.216")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://storage.googleapis.com/claude-code-dist-"
             "86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/"
             version "/linux-x64/claude"))
       (sha256
        (base32 "11g0dm1ms4gv12h4d647nni423lqlparn2dbfpn8100b492wmpkl"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan
      #~'(("claude" ()))
      #:install-plan
      #~'(("claude" "bin/claude-unwrapped"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (copy-file (assoc-ref inputs "source") "claude")
              (chmod "claude" #o755)))
          (add-after 'install 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (unwrapped (string-append bin "/claude-unwrapped"))
                     (wrapper (string-append bin "/claude")))
                (call-with-output-file wrapper
                  (lambda (port)
                    (format port "#!~a
export DISABLE_AUTOUPDATER=1
export DISABLE_INSTALLATION_CHECKS=1
exec ~a \"$@\"
"
                            (search-input-file inputs "bin/bash")
                            unwrapped)))
                (chmod wrapper #o755)))))))
    (inputs
     (list bash-minimal))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/anthropics/claude-code")
    (synopsis "Claude AI assistant for the terminal")
    (description
     "Claude Code is an agentic coding tool that lives in your terminal.
It can understand your codebase, edit files, run terminal commands, and
handle entire workflows.  This package disables auto-updates.")
    (license (nonfree "https://code.claude.com/docs/en/legal-and-compliance"))))

(define-public claude-desktop
  (package
    (name "claude-desktop")
    (version "1.22209.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://downloads.claude.ai/claude-desktop/apt/stable/pool/"
             "main/c/claude-desktop/claude-desktop_" version "_amd64.deb"))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32 "0p350z7admw50ahcf7sxy2jvhl7p14pn06j4i96vqg93r5mg89yl"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; ~144MB deb, faster to fetch from Anthropic than a substitute.
      #:substitutable? #f
      #:wrapper-plan
      #~(map (lambda (file)
               (string-append "usr/lib/claude-desktop/" file))
             '("claude-desktop"
               "chrome-sandbox"
               "chrome_crashpad_handler"
               "libEGL.so"
               "libGLESv2.so"
               "libffmpeg.so"
               "libvk_swiftshader.so"
               "libvulkan.so.1"
               "resources/chrome-native-host"
               "resources/virtiofsd"
               "resources/app.asar.unpacked/node_modules/@ant/claude-native/claude-native-binding.node"
               "resources/app.asar.unpacked/node_modules/node-pty/prebuilds/linux-x64/pty.node"))
      #:install-plan
      #~'(("usr/lib/claude-desktop/" "/share/claude-desktop")
          ("usr/share/applications/" "/share/applications")
          ("usr/share/icons/" "/share/icons"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'patch-desktop
            (lambda _
              (substitute* "usr/share/applications/com.anthropic.Claude.desktop"
                (("Exec=claude-desktop")
                 (string-append "Exec=" #$output "/bin/claude-desktop")))))
          (add-before 'install-wrapper 'install-exe
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (symlink (string-append #$output
                                        "/share/claude-desktop/claude-desktop")
                         (string-append bin "/claude-desktop")))))
          ;; The main binary directly NEEDs the co-located libffmpeg.so and
          ;; the NSS libs (which live in nss/lib/nss); patchelf drops $ORIGIN
          ;; and only adds nss/lib, so point the RUNPATH at both.
          (add-after 'install-exe 'set-bundled-rpath
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "patchelf" "--add-rpath"
                      (string-append #$output "/share/claude-desktop" ":"
                                     (assoc-ref inputs "nss") "/lib/nss")
                      (string-append #$output
                                     "/share/claude-desktop/claude-desktop"))))
          ;; Chromium picks its password backend from the desktop environment;
          ;; on unrecognized ones (wlroots compositors such as niri) it falls
          ;; back to the plaintext store and won't persist logins.  Force
          ;; libsecret so it reaches whatever Secret Service is running.
          (add-after 'install-wrapper 'force-libsecret
            (lambda _
              (substitute* (string-append #$output "/bin/claude-desktop")
                (("claude-desktop/claude-desktop\" ")
                 "claude-desktop/claude-desktop\" --password-store=gnome-libsecret ")))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://claude.ai/download")
    (synopsis "Claude Desktop for Linux")
    (description
     "Claude Desktop is Anthropic's official desktop client for Claude,
bringing Chat, Cowork, and Claude Code into a single Electron application
with Model Context Protocol (MCP) support and system tray integration.

This package repackages the official Debian build from Anthropic's apt
repository, patching the bundled Chromium runtime for the Guix store.
Linux support is currently in beta.")
    (license (nonfree "https://www.anthropic.com/legal/consumer-terms"))))

(define-public ollama
  (package
    (name "ollama")
    (version "0.31.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ollama/ollama/releases/download/v"
             version "/ollama-linux-amd64.tar.zst"))
       (sha256
        (base32 "0cribdbjpnsaan7m97x798fmas65dqlwak5d7idar6wm3brz121c"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan
      #~'(("bin/ollama" ("glibc" "gcc")))
      #:install-plan
      #~'(("bin/ollama" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "tar" "--use-compress-program=zstd" "-xf"
                      (assoc-ref inputs "source")))))))
    (native-inputs
     (list zstd))
    (inputs
     (list glibc
           `(,gcc "lib")))
    (supported-systems '("x86_64-linux"))
    (home-page "https://ollama.com")
    (synopsis "Run large language models locally")
    (description
     "Ollama allows you to run large language models locally.
It provides a simple API for creating, running and managing models,
as well as a library of pre-built models that can be easily used.")
    (license license:expat)))

(define-public tku
  (package
    (name "tku")
    (version "0.1.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/tku")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yiw97cdwf51ga3al1m7ddn4sszvk22iiwi7l50y0ryd7piicpc3"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:rust rust-1.89
      #:install-source? #f
      #:tests? #t
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check-for-pregenerated-files))))
    (inputs
     (px-cargo-inputs 'tku))
    (home-page "https://github.com/franzos/tku")
    (synopsis "Token usage CLI for AI coding assistants")
    (description
     "TKU is a command-line tool for tracking token usage and costs across
multiple AI coding assistants. It scans local session files, fetches live
pricing, and shows aggregated reports by day, month, session, or model.")
    (license license:expat)))
