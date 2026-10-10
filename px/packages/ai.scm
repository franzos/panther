;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages ai)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages serialization)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix licenses)
  #:use-module (gnu packages rust)
  #:use-module (px self))

(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.294")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://storage.googleapis.com/claude-code-dist-"
             "86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/"
             version "/linux-x64/claude"))
       (sha256
        (base32 "0qpjf63vnl5c22dkv3r5zxsdjw33qs05pwxydxa3gx94nskjq4i7"))))
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
    (version "2.26454.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://downloads.claude.ai/claude-desktop/apt/stable/pool/"
             "main/c/claude-desktop/claude-desktop_" version "_amd64.deb"))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32 "03s3nayxc8n01ic5kvhm93w2gd2ji7nzi3ar6d7qfdc698ia0ldj"))))
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

;; Node's architecture tag, which names the bundled prebuilt addon directories.
(define (chatgpt-arch)
  (match (or (%current-system) (%current-target-system))
    ("x86_64-linux" "x64")
    ("aarch64-linux" "arm64")))

(define-public chatgpt
  (package
    (name "chatgpt")
    (version "26.1007.21434")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://persistent.oaistatic.com/codex-app-prod/linux/deb/"
             "pool/main/c/chatgpt/chatgpt_" version "_"
             (match (or (%current-system) (%current-target-system))
               ("x86_64-linux" "amd64")
               ("aarch64-linux" "arm64")) ".deb"))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("x86_64-linux" "1vgkn6cl1d8clki7yxp485lf1pbyzbvhc8fm78fjcjypqd0m6sr7")
           ("aarch64-linux" "0jdg3f579iic51z1akb0alibzy6w6w3ygs900slnjq0xzgiy3yjm"))))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; ~390MB deb, faster to fetch from OpenAI than a substitute.
      #:substitutable? #f
      #:wrapper-plan
      #~(let ((arch #$(chatgpt-arch))
              ;; These prebuilt addons are named after the ABI, not the arch.
              (napi #$(match (or (%current-system) (%current-target-system))
                        ("x86_64-linux" "node.napi.glibc.node")
                        ("aarch64-linux" "node.napi.armv8.node")))
              (level #$(match (or (%current-system) (%current-target-system))
                         ("x86_64-linux" "classic-level.node")
                         ("aarch64-linux" "classic-level.armv8.node"))))
          (map (lambda (file)
                 (string-append "usr/lib/chatgpt/" file))
               (list "ChatGPT"
                     "browser_crashpad_handler"
                     "libEGL.so"
                     "libGLESv2.so"
                     "libvk_swiftshader.so"
                     "libvulkan.so.1"
                     "resources/native/hid-topology-watcher.node"
                     "resources/app.asar.unpacked/node_modules/better-sqlite3/build/Release/better_sqlite3.node"
                     "resources/app.asar.unpacked/node_modules/node-pty/build/Release/pty.node"
                     (string-append "resources/app.asar.unpacked/node_modules/@parcel/watcher-linux-"
                                    arch "-glibc/watcher.node")
                     (string-append "resources/app.asar.unpacked/node_modules/@worklouder/device-kit-oai/node_modules/@worklouder/wl-device-kit/dist/native/linux/"
                                    arch "/serial_control.node")
                     (string-append "resources/app.asar.unpacked/node_modules/@worklouder/device-kit-oai/node_modules/@worklouder/wl-device-kit/node_modules/node-hid/prebuilds/HID-linux-"
                                    arch "/node-napi-v4.node")
                     (string-append "resources/app.asar.unpacked/node_modules/@worklouder/device-kit-oai/node_modules/@worklouder/wl-device-kit/node_modules/node-hid/prebuilds/HID_hidraw-linux-"
                                    arch "/node-napi-v4.node")
                     (string-append "resources/app.asar.unpacked/node_modules/@worklouder/device-kit-oai/node_modules/@worklouder/wl-device-kit/node_modules/serialport/node_modules/@serialport/bindings-cpp/prebuilds/linux-"
                                    arch "/" napi)
                     (string-append "resources/plugins/openai-bundled/plugins/browser/node_modules/classic-level/prebuilds/linux-"
                                    arch "/" level)
                     (string-append "resources/plugins/openai-bundled/plugins/chrome/node_modules/classic-level/prebuilds/linux-"
                                    arch "/" level)
                     (string-append "resources/plugins/openai-bundled/plugins/chrome/extension-host/linux/"
                                    arch "/extension-host")
                     "resources/cua_node/bin/node"
                     "resources/cua_node/bin/node_repl"
                     (string-append "resources/cua_node/lib/node_modules/.bin/sky_linux_" arch)
                     (string-append "resources/cua_node/lib/node_modules/@oai/sky/bin/linux/sky_linux_"
                                    arch)
                     (string-append "resources/cua_node/lib/node_modules/@img/sharp-libvips-linux-"
                                    arch "/lib/libvips-cpp.so.8.18.6")
                     (string-append "resources/cua_node/lib/node_modules/@img/sharp-linux-"
                                    arch "/lib/sharp-linux-" arch "-0.35.4.node"))))
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
                      (string-append "$ORIGIN/../../sharp-libvips-linux-"
                                     #$(chatgpt-arch) "/lib")
                      (string-append
                       #$output "/share/chatgpt/resources/cua_node/lib"
                       "/node_modules/@img/sharp-linux-" #$(chatgpt-arch) "/lib"
                       "/sharp-linux-" #$(chatgpt-arch) "-0.35.4.node"))))
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
    (supported-systems '("x86_64-linux" "aarch64-linux"))
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
    (version "0.40.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ollama/ollama/releases/download/v"
             version "/ollama-linux-"
             (match (or (%current-system) (%current-target-system))
               ("x86_64-linux" "amd64")
               ("aarch64-linux" "arm64")) ".tar.zst"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("x86_64-linux" "0p9mbrakfyp8ip2ki6csbz5n6j5d31rfb8sn38sz3k3nvpivpbm7")
           ("aarch64-linux" "1jqb2dxbdlkzkm9ahmbdpqjqwihndlzrkjr8z4p51s8dgslxkjzm"))))))
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
    (supported-systems '("x86_64-linux" "aarch64-linux"))
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
    (version "0.1.25")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/tku")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12ybsn6sx54zxi7r69w5jmrn3f6mnrdbynw6z3f0pfrsx8fnrpr6"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:rust rust-1.89
      #:install-source? #f
      #:tests? #t
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check-for-pregenerated-files)
          (add-after 'unpack 'patch-test-shebangs
            (lambda _
              (substitute* "src/creds.rs"
                (("#!/bin/sh") (string-append "#!" (which "sh")))))))))
    (inputs
     (px-cargo-inputs 'tku))
    (home-page "https://github.com/franzos/tku")
    (synopsis "Token usage CLI for AI coding assistants")
    (description
     "TKU is a command-line tool for tracking token usage and costs across
multiple AI coding assistants. It scans local session files, fetches live
pricing, and shows aggregated reports by day, month, session, or model.")
    (license license:expat)))

(define onnx-for-onnxruntime-next
  (package
    (inherit onnx-for-onnxruntime)
    (version "1.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onnx/onnx")
             (commit (string-append "v" version))))
       (file-name (git-file-name "onnx" version))
       (sha256
        (base32 "14ffc3zkq6apvlkdhldqjwwkwb79lj1icqnvaxplgpjdynvvkkl1"))
       (patches
        (list (local-file "patches/onnx-1.22.0-for-onnxruntime.patch")))))))

(define-public onnxruntime-next
  (package
    (inherit onnxruntime)
    (name "onnxruntime")
    (version "1.31.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/onnxruntime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k13wf6s2rqmx2510pwddyc8ap3r1264pa5hc6ilj07ixkp2zahk"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;unit tests are not built
      #:configure-flags
      #~(list "-DFETCHCONTENT_FULLY_DISCONNECTED=ON"
              "-Donnxruntime_BUILD_UNIT_TESTS=OFF"
              "-Donnxruntime_BUILD_SHARED_LIB=ON"
              "-Donnxruntime_USE_FULL_PROTOBUF=ON"
              "-DProtobuf_USE_STATIC_LIBS=ON"
              "-DCMAKE_CXX_FLAGS=-Wl,-z,noexecstack")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "cmake")))
          (add-after 'unpack 'use-system-dependencies
            (lambda _
              (with-output-to-file "cmake/external/eigen.cmake"
                (lambda _
                  (display "find_package(Eigen3 REQUIRED)\n")))
              ;; Upstream only looks up installed cpuinfo and Boost with vcpkg.
              (substitute* "cmake/external/onnxruntime_external_deps.cmake"
                (("if\\(onnxruntime_USE_VCPKG AND NOT APPLE\\)")
                 "if(TRUE)")
                (("^if\\(NOT TARGET Boost::mp11\\)")
                 "find_package(Boost REQUIRED)
add_library(Boost::mp11 ALIAS Boost::headers)
if(NOT TARGET Boost::mp11)")))))))
    (outputs (list "out"))
    (inputs (list abseil-cpp
                  boost
                  c++-gsl
                  cpuinfo
                  date
                  eigen-for-onnxruntime
                  flatbuffers-23.5
                  nlohmann-json
                  onnx-for-onnxruntime-next
                  protobuf-static-for-onnxruntime
                  re2-next
                  safeint
                  zlib))
    (native-inputs (list pkg-config python-minimal-wrapper))
    (propagated-inputs '())))
