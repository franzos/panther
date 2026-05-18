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
  #:use-module (nonguix licenses)
  #:use-module (nongnu packages electron)
  #:use-module (px packages rust)
  #:use-module (px self))

(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.143")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://storage.googleapis.com/claude-code-dist-"
             "86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/"
             version "/linux-x64/claude"))
       (sha256
        (base32 "1i6wn6icac5kqzamqkgdf0wnsp2wkcs9wbqrhr5lkkfrz4zxqpzp"))))
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
    (version "1.6608.2-2.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/aaddrick/claude-desktop-debian/releases/"
             "download/v2.0.10+claude1.6608.2/"
             "claude-desktop_1.6608.2-2.0.10_amd64.deb"))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32 "02v6930yfwckw6pvs0rm160chg1n9ckkxh88k3s0jiwn4dsfzh7r"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan #~'()
      #:install-plan #~'()
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "ar" "x" (assoc-ref inputs "source"))
              (invoke "tar" "--use-compress-program=zstd" "-xf" "data.tar.zst")))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib/claude-desktop"))
                     (resources (string-append lib "/resources"))
                     (src-resources
                      "usr/lib/claude-desktop/node_modules/electron/dist/resources")
                     (electron (search-input-file inputs "/bin/electron")))
                (mkdir-p resources)
                (mkdir-p bin)
                ;; App + unpacked + co-located resources used by the asar
                (install-file (string-append src-resources "/app.asar") resources)
                (copy-recursively
                 (string-append src-resources "/app.asar.unpacked")
                 (string-append resources "/app.asar.unpacked"))
                (for-each
                 (lambda (f)
                   (let ((full (string-append src-resources "/" f)))
                     (cond ((not (file-exists? full)) #t)
                           ((file-is-directory? full)
                            (copy-recursively
                             full (string-append resources "/" f)))
                           (else
                            (install-file full resources)))))
                 '("en-US.json" "de-DE.json" "es-419.json" "es-ES.json"
                   "fr-FR.json" "hi-IN.json" "id-ID.json" "it-IT.json"
                   "ja-JP.json" "ko-KR.json" "pt-BR.json"
                   "TrayIconTemplate.png" "TrayIconTemplate@2x.png"
                   "TrayIconTemplate@3x.png" "TrayIconTemplate-Dark.png"
                   "TrayIconTemplate-Dark@2x.png" "TrayIconTemplate-Dark@3x.png"
                   "Tray-Win32.ico" "Tray-Win32-Dark.ico"
                   "ion-dist"))
                ;; Shared launcher functions + doctor diagnostics
                (install-file "usr/lib/claude-desktop/launcher-common.sh" lib)
                (install-file "usr/lib/claude-desktop/doctor.sh" lib)
                ;; Icons, desktop entry
                (copy-recursively "usr/share/icons"
                                  (string-append out "/share/icons"))
                (install-file "usr/share/applications/claude-desktop.desktop"
                              (string-append out "/share/applications"))
                ;; Launcher: keep upstream logic, retarget paths to our store
                (install-file "usr/bin/claude-desktop" bin)
                (let ((launcher (string-append bin "/claude-desktop")))
                  (substitute* launcher
                    (("/usr/lib/claude-desktop/launcher-common.sh")
                     (string-append lib "/launcher-common.sh"))
                    (("/usr/lib/claude-desktop/node_modules/electron/dist/electron")
                     electron)
                    (("/usr/lib/claude-desktop/node_modules/electron/dist/resources/app\\.asar")
                     (string-append resources "/app.asar"))
                    (("app_dir='/usr/lib/claude-desktop'")
                     (string-append "app_dir='" lib "'"))
                    (("app_dir=\"/usr/lib/claude-desktop\"")
                     (string-append "app_dir=\"" lib "\"")))
                  (chmod launcher #o755))
                (substitute* (string-append
                              out "/share/applications/claude-desktop.desktop")
                  (("/usr/bin/claude-desktop")
                   (string-append bin "/claude-desktop")))))))))
    (native-inputs
     (list binutils zstd))
    (inputs
     (list bash-minimal electron-41))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/aaddrick/claude-desktop-debian")
    (synopsis "Claude Desktop for Linux")
    (description
     "Claude Desktop is Anthropic's official desktop client for Claude,
repackaged for Linux by the @code{claude-desktop-debian} project.  It
runs as an Electron app with Model Context Protocol (MCP) support, a
global hotkey (Ctrl+Alt+Space), and system tray integration.

This package extracts the prebuilt @code{.deb} from the upstream
release and wraps it with the Guix-provided Electron runtime.")
    (license (nonfree "https://www.anthropic.com/legal/consumer-terms"))))

(define-public ollama
  (package
    (name "ollama")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ollama/ollama/releases/download/v"
             version "/ollama-linux-amd64.tar.zst"))
       (sha256
        (base32 "1m4syjdvmjk5n691d8l9a04hgfk00952fk259w7yi8vd1k6ac6sq"))))
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
    (version "0.1.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/tku")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yjvn3pxq69dg0jwr3yjwgz7d3rmihip5rpw2cv3vnqqicqif0zd"))))
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
