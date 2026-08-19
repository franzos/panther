;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages ai)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
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
    (version "2.1.233")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://storage.googleapis.com/claude-code-dist-"
             "86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/"
             version "/linux-x64/claude"))
       (sha256
        (base32 "1ngr7fwd849cihs3fgki0mycpb7kkxgbykfrppmi3m2pdw4q3ljm"))))
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
    (version "1.32352.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://downloads.claude.ai/claude-desktop/apt/stable/pool/"
             "main/c/claude-desktop/claude-desktop_" version "_amd64.deb"))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32 "11l1wf3kxg47igvb51vxfgbs9i0fjfgp1wd13hg0cdyhhvmc804x"))))
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

(define-public chatgpt
  (package
    (name "chatgpt")
    (version "26.814.41957")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://persistent.oaistatic.com/codex-app-prod/linux/deb/"
             "pool/main/c/chatgpt/chatgpt_" version "_amd64.deb"))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32 "0zdcjfpjy6kw06zxlinii2b2vgiyplbmrc6m2ir6825xg9mb4y27"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; ~390MB deb, faster to fetch from OpenAI than a substitute.
      #:substitutable? #f
      #:wrapper-plan
      #~(map (lambda (file)
               (string-append "usr/lib/chatgpt/" file))
             '("ChatGPT"
               "browser_crashpad_handler"
               "libEGL.so"
               "libGLESv2.so"
               "libvk_swiftshader.so"
               "libvulkan.so.1"
               "resources/native/hid-topology-watcher.node"
               "resources/app.asar.unpacked/node_modules/better-sqlite3/build/Release/better_sqlite3.node"
               "resources/app.asar.unpacked/node_modules/node-pty/build/Release/pty.node"
               "resources/app.asar.unpacked/node_modules/@parcel/watcher-linux-x64-glibc/watcher.node"
               "resources/app.asar.unpacked/node_modules/@worklouder/device-kit-oai/node_modules/@worklouder/wl-device-kit/dist/native/linux/x64/serial_control.node"
               "resources/app.asar.unpacked/node_modules/@worklouder/device-kit-oai/node_modules/@worklouder/wl-device-kit/node_modules/node-hid/prebuilds/HID-linux-x64/node-napi-v4.node"
               "resources/app.asar.unpacked/node_modules/@worklouder/device-kit-oai/node_modules/@worklouder/wl-device-kit/node_modules/node-hid/prebuilds/HID_hidraw-linux-x64/node-napi-v4.node"
               "resources/app.asar.unpacked/node_modules/@worklouder/device-kit-oai/node_modules/@worklouder/wl-device-kit/node_modules/serialport/node_modules/@serialport/bindings-cpp/prebuilds/linux-x64/node.napi.glibc.node"
               "resources/plugins/openai-bundled/plugins/browser/node_modules/classic-level/prebuilds/linux-x64/classic-level.node"
               "resources/plugins/openai-bundled/plugins/chrome/node_modules/classic-level/prebuilds/linux-x64/classic-level.node"
               "resources/plugins/openai-bundled/plugins/chrome/extension-host/linux/x64/extension-host"
               "resources/cua_node/bin/node"
               "resources/cua_node/bin/node_repl"
               "resources/cua_node/lib/node_modules/.bin/sky_linux_x64"
               "resources/cua_node/lib/node_modules/@oai/sky/bin/linux/sky_linux_x64"
               "resources/cua_node/lib/node_modules/@img/sharp-libvips-linux-x64/lib/libvips-cpp.so.8.18.3"
               "resources/cua_node/lib/node_modules/@img/sharp-linux-x64/lib/sharp-linux-x64-0.35.3.node"
               "resources/cua_node/lib/node_modules/@napi-rs/canvas-linux-x64-gnu/skia.linux-x64-gnu.node"))
      #:install-plan
      #~'(("usr/lib/chatgpt/" "/share/chatgpt")
          ("usr/share/applications/" "/share/applications")
          ("usr/share/pixmaps/" "/share/pixmaps"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'patch-desktop
            (lambda _
              (substitute* "usr/share/applications/chatgpt.desktop"
                (("Exec=chatgpt")
                 (string-append "Exec=" #$output "/bin/chatgpt")))))
          (add-before 'install-wrapper 'install-exe
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (symlink (string-append #$output "/share/chatgpt/ChatGPT")
                         (string-append bin "/chatgpt")))))
          ;; Chromium loads the co-located libEGL/libGLESv2/swiftshader by
          ;; name, and NSS lives one level deeper than patchelf's "/lib".
          (add-after 'install-exe 'set-bundled-rpath
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "patchelf" "--add-rpath"
                      (string-append #$output "/share/chatgpt" ":"
                                     (assoc-ref inputs "nss") "/lib/nss")
                      (string-append #$output "/share/chatgpt/ChatGPT"))
              ;; patchelf replaced sharp's $ORIGIN-relative RUNPATH, which is
              ;; how it finds its own libvips.
              (invoke "patchelf" "--add-rpath"
                      "$ORIGIN/../../sharp-libvips-linux-x64/lib"
                      (string-append
                       #$output "/share/chatgpt/resources/cua_node/lib"
                       "/node_modules/@img/sharp-linux-x64/lib"
                       "/sharp-linux-x64-0.35.3.node"))))
          ;; Chromium picks its password backend from the desktop environment;
          ;; on unrecognized ones (wlroots compositors such as niri) it falls
          ;; back to the plaintext store and won't persist logins.  Force
          ;; libsecret so it reaches whatever Secret Service is running.
          (add-after 'install-wrapper 'force-libsecret
            (lambda _
              (substitute* (string-append #$output "/bin/chatgpt")
                (("share/chatgpt/ChatGPT\" ")
                 "share/chatgpt/ChatGPT\" --password-store=gnome-libsecret ")))))))
    (inputs
     (list gdk-pixbuf libusb))
    (supported-systems '("x86_64-linux"))
    (home-page "https://developers.openai.com/codex/app")
    (synopsis "ChatGPT desktop client for Linux")
    (description
     "The ChatGPT desktop app brings ChatGPT, Work and Codex together in a
single Electron application, with access to local files and projects.

This package repackages the official Debian build from OpenAI's apt
repository, patching the bundled Chromium runtime for the Guix store.
Linux support is currently a preview; Computer Use is not available on it.")
    (license (nonfree "https://openai.com/policies/terms-of-use/"))))

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
    (version "0.1.22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/tku")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0adwx2z0xi74x75i0swjfxxl14xckq4bbwk6cxz7lapq8yvi00zs"))))
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
