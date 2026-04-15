;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>
;;; Copyright © 2023 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2025 Benjamin Slade <slade@lambda-y.net>

(define-module (px packages networking)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix licenses)
  #:use-module ((guix licenses)
                :prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (px packages go)
  #:use-module (px packages golang-xyz)
  #:use-module (px packages rust)
  #:use-module (px self)
  #:use-module (ice-9 match))

(define-public nebula
  (package
    (name "nebula")
    (version "1.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/slackhq/nebula/releases/download/v" version
             "/nebula-linux-amd64.tar.gz"))
       (sha256
        (base32 "0m996iwb6v8fqlf0lj6zf4wyqdhabm5ks2mhnsk057dnmrf37b4r"))))
    (build-system binary-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'adjust-paths
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin")))
                        (mkdir-p bin)
                        (install-file "nebula" bin)
                        (install-file "nebula-cert" bin)))))))
    (inputs `(("expat" ,expat)))
    (home-page "https://github.com/slackhq/nebula")
    (synopsis
     "A scalable overlay networking tool with a focus on performance, simplicity and security")
    (description
     "Nebula is a scalable overlay networking tool with a focus on performance, simplicity
and security.It lets you seamlessly connect computers anywhere in the world. Nebula is portable,
and runs on Linux, OSX, Windows, iOS, and Android. It can be used to connect a small number of computers,
but is also able to connect tens of thousands of computers.")
    (license license:expat)))

(define-public ngrok
  (package
    (name "ngrok")
    (version "3.37.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://bin.equinox.io/c/bNyj1mQVY4c/ngrok-v3-" version
             "-linux-"
             (match (or (%current-system) (%current-target-system))
               ("x86_64-linux" "amd64")
               ("aarch64-linux" "arm64"))
             ".tgz"))
       (file-name (string-append name "-" version "-"
                                 (match (or (%current-system)
                                            (%current-target-system))
                                   ("x86_64-linux" "amd64")
                                   ("aarch64-linux" "arm64"))
                                 ".tgz"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("x86_64-linux"
            "1l1jx407cknglpl0q9lhp289czybj8d6ypydffc7y9gd9h3by7gp")
           ("aarch64-linux"
            "1jn1cannq1724vyzj8fwgmy7l7aslwnn0vpm5v0rbcw0297nx80p"))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:patchelf-plan #~'()
      #:install-plan #~'(("ngrok" "bin/"))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://ngrok.com")
    (synopsis "Secure tunnels to localhost")
    (description
     "ngrok exposes local networked services behind NATs and firewalls to
the public internet over a secure tunnel.  It is useful for sharing local
websites, building and testing webhook consumers, and self-hosting personal
services.  This package ships the official statically-linked binary from
ngrok.")
    (license (nonfree "https://ngrok.com/tos"))))

(define-public v2ray
  (package
    (name "v2ray")
    (version "5.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/v2fly/v2ray-core/releases/download/v" version
             "/v2ray-linux-64.zip"))
       (sha256
        (base32 "0lpypnfnavsjc12g7c4g03x25kcnqx50hd2f18l68y788n4g96bk"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("v2ray" "/bin/")
                        ("geoip.dat" "/bin/")
                        ("geosite.dat" "/bin/")
                        ("config.json" "/bin/"))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source))))))
    (native-inputs (list unzip))
    (home-page "https://tricks.aseman.io")
    (synopsis
     " A platform for building proxies to bypass network restrictions.")
    (description
     "Project V is a set of network tools that help you to build your
own computer network. It secures your network connections and thus protects your
privacy. See our website for more information.")
    (license license:expat)))

(define-public xray-core
  (package
    (name "xray-core")
    (version "26.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/XTLS/Xray-core/releases/download/v" version
             "/Xray-linux-64.zip"))
       (file-name (string-append "Xray-linux-64-" version ".zip"))
       (sha256
        (base32 "11g3z34sncmpid8fry7g0gh0nhfc5n2d9hm1zw3a81z2ardm7ki9"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("xray" "/bin/")
                        ("geoip.dat" "/bin/")
                        ("geosite.dat" "/bin/"))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source))))))
    (native-inputs (list unzip))
    (home-page "https://tricks.aseman.io")
    (synopsis
     " A platform for building proxies to bypass network restrictions.")
    (description
     "Project V is a set of network tools that help you to build your
own computer network. It secures your network connections and thus protects your
privacy. See our website for more information.")
    (license license:expat)))

(define-public nng-1.5
  (package
    (name "nng")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nanomsg/nng")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sap0iny3z9lhmaiassv8jc399md1307y32xxx3mrr74jcpcrf59"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DNNG_ENABLE_COVERAGE=ON"
                               "-DNNG_ENABLE_TLS=ON" "-DBUILD_SHARED_LIBS=ON")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-failing-tests
                    (lambda _
                      ;; These tests require network access.
                      (substitute* "tests/CMakeLists.txt"
                        (("add_nng_test1\\(httpclient 60 NNG_SUPP_HTTP\\)")
                         "")
                        (("add_nng_test\\(multistress 60\\)")
                         "")
                        (("add_nng_test\\(tls 60\\)")
                         ""))
                      (substitute* "src/supplemental/websocket/CMakeLists.txt"
                        (("nng_test\\(wssfile_test\\)")
                         ""))
                      (substitute* "src/sp/transport/ws/CMakeLists.txt"
                        (("nng_test_if\\(WS_ON ws_test\\)")
                         ""))
                      (substitute* "src/sp/transport/tcp/CMakeLists.txt"
                        (("nng_test\\(tcp_test\\)")
                         ""))
                      (substitute* "src/platform/CMakeLists.txt"
                        (("nng_test\\(resolver_test\\)")
                         ""))
                      #t)))))
    (native-inputs `(("ksh" ,oksh)))
    (inputs `(("mbedtls" ,mbedtls)))
    (synopsis "Lightweight messaging library")
    (description
     "NNG project is a rewrite of the scalability protocols library
known as libnanomsg, and adds significant new capabilities, while retaining
compatibility with the original.  It is a lightweight, broker-less library,
offering a simple API to solve common recurring messaging problems, such as
publish/subscribe, RPC-style request/reply, or service discovery.")
    (home-page "https://nng.nanomsg.org/")
    (license license:expat)))

;;
;; Tailscale
;;
;; Derived from guix-tailscale by Brennan Vincent
;; https://github.com/umanwizard/guix-tailscale
;; Licensed under the Apache License, Version 2.0

(define-public tailscale
  (let ((version "1.94.2"))
    (package
      (name "tailscale")
      (version version)
      (source (origin
                (method go-fetch-vendored)
                (uri (go-git-reference
                      (url "https://github.com/tailscale/tailscale")
                      (commit "v1.94.2")
                      (sha (base32 "07pz43lfmxkvg3b1a4dq9wh8a247gaqxppqaa8ah4mjnrh3radda"))))
                (sha256
                 (base32
                  "0dqwgihl9wjv4kwsv0dj7c7l8n99bkfsd7v1ipy3cw3akgclilf7"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "tailscale.com/cmd/tailscale"
         #:unpack-path "tailscale.com"
         #:install-source? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'check))
         #:go ,go-1.25))
      (home-page "https://tailscale.com")
      (synopsis "Tailscale VPN client")
      (description "Tailscale is a zero-config VPN based on WireGuard.
This package provides the Tailscale client command-line interface.")
      (license license:bsd-3))))

(define-public tailscaled
  (let ((import-path "tailscale.com/cmd/tailscaled"))
    (package
      (inherit tailscale)
      (name "tailscaled")
      (arguments
       (substitute-keyword-arguments (package-arguments tailscale)
         ((#:import-path _ #f)
          import-path)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (replace 'build
                (lambda _
                  (unsetenv "GO111MODULE")
                  (chdir "./src/tailscale.com")
                  (invoke "go" "build" "-o" "tailscaled"
                          #$import-path)
                  (chdir "../..")))
              (replace 'install
                (lambda _
                  (install-file "src/tailscale.com/tailscaled"
                                (string-append #$output "/bin"))))))))
      (synopsis "Tailscale VPN daemon")
      (description "Tailscale is a zero-config VPN based on WireGuard.
This package provides the Tailscale daemon (tailscaled) which manages
the VPN connection."))))

(define-public ivpn
  (package
    (name "ivpn")
    (version "3.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ivpn/desktop-app")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "108dlvivn8sbr1wcb6p6lhs45xqwqhncaznlr7c7z443cpzidsk3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/ivpn/desktop-app/daemon"
      #:unpack-path "github.com/ivpn/desktop-app"
      #:install-source? #f
      #:tests? #f  ; Tests require external binaries and network
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key import-path build-flags #:allow-other-keys)
              (let ((ldflags (string-append
                              "-s -w"
                              " -X github.com/ivpn/desktop-app/daemon/version._version="
                              #$version)))
                ;; Build the daemon
                (apply invoke "go" "build" "-v" "-x"
                       (string-append "-ldflags=" ldflags)
                       "-o" "bin/ivpn-service"
                       import-path
                       build-flags)
                ;; Build the CLI
                (apply invoke "go" "build" "-v" "-x"
                       (string-append "-ldflags=" ldflags)
                       "-o" "bin/ivpn"
                       "github.com/ivpn/desktop-app/cli"
                       build-flags))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (sbin (string-append out "/sbin")))
                (mkdir-p bin)
                (mkdir-p sbin)
                (install-file "bin/ivpn-service" sbin)
                (install-file "bin/ivpn" bin)))))))
    (inputs
     (list openvpn
           wireguard-tools))
    (propagated-inputs
     (list go-github-com-fsnotify-fsnotify
           go-github-com-google-uuid
           go-github-com-stretchr-testify
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-zx2c4-com-wireguard
           go-golang-zx2c4-com-wireguard-wgctrl
           go-github-com-mdlayher-wifi-next
           go-github-com-mdlayher-netlink
           go-github-com-mdlayher-genetlink
           go-github-com-mdlayher-socket
           go-github-com-josharian-native
           go-github-com-olekukonko-tablewriter))
    (home-page "https://www.ivpn.net/")
    (synopsis "IVPN client daemon and CLI")
    (description
     "IVPN is a privacy-focused VPN service.  This package provides the daemon
service and command-line interface for connecting to IVPN servers using
OpenVPN or WireGuard protocols.  Features include kill-switch, multi-hop
connections, and custom DNS settings.")
    (license license:gpl3+)))

(define-public oha
  (package
    (name "oha")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oha" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pr71vifz2mqybq2n8mnaik6mgirrs4jfr2na5d1mj5494xqvkbn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.87
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-jemalloc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jemalloc (assoc-ref inputs "jemalloc")))
               (setenv "CARGO_FEATURE_UNPREFIXED_MALLOC_ON_SUPPORTED_PLATFORMS" "1")
               (setenv "JEMALLOC_OVERRIDE"
                       (string-append jemalloc "/lib/libjemalloc.so"))))))))
    (native-inputs (list pkg-config))
    (inputs
     (cons* jemalloc
            sqlite
            (px-cargo-inputs 'oha)))
    (home-page "https://github.com/hatoo/oha")
    (synopsis "HTTP load generator with real-time TUI")
    (description
     "Oha is an HTTP load generator inspired by rakyll/hey, written in Rust.
It sends configurable load to web applications and displays real-time
statistics in a terminal user interface powered by ratatui.")
    (license license:expat)))

(define-public sniffnet
  (package
    (name "sniffnet")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sniffnet" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04fx5k9nlxwspnm3nvcw8idnvr1fdqqv6sxapgwj6w1j9n7j7qkq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.88
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (mesa (assoc-ref inputs "mesa"))
                   (wayland (assoc-ref inputs "wayland"))
                   (libxkbcommon (assoc-ref inputs "libxkbcommon")))
               (wrap-program (string-append out "/bin/sniffnet")
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append mesa "/lib")
                    ,(string-append wayland "/lib")
                    ,(string-append libxkbcommon "/lib"))))))))))
    (native-inputs (list pkg-config))
    (inputs
     (cons* alsa-lib
            fontconfig
            libpcap
            libxkbcommon
            mesa
            wayland
            (px-cargo-inputs 'sniffnet)))
    (home-page "https://sniffnet.net")
    (synopsis "Application to comfortably monitor your network traffic")
    (description
     "Sniffnet is a cross-platform GUI application to monitor your network
traffic. It provides real-time visualization of network connections, traffic
statistics, and allows filtering by protocol, IP address, and port. Features
include geolocation of remote hosts, custom notifications, and export of
captured data.")
    (license (list license:expat license:asl2.0))))

(define-public halloy
  (package
    (name "halloy")
    (version "2026.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/squidowl/halloy/archive/refs/tags/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wvvj7mzd6dfxjhvzrv3slc3y0sdjgcpck0iyhzj9lbda7ik9627"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:tests? #f
      #:rust rust-1.92
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'patch-git-deps-to-paths
            (lambda _
              (define (find-vendor-dir pattern)
                (let ((dirs (find-files "guix-vendor" pattern
                                        #:directories? #t
                                        #:fail-on-error? #f)))
                  (if (pair? dirs) (car dirs) #f)))
              (let ((iced-dir (find-vendor-dir "^rust-iced-0\\.15"))
                    (cryoglyph-dir (find-vendor-dir "^rust-cryoglyph-0\\.1\\.0\\.1d68"))
                    (winit-dir (find-vendor-dir "^rust-winit-0\\.30.*05b8ff")))
                ;; Patch halloy's [patch.crates-io] git refs to local paths
                (substitute* "Cargo.toml"
                  (("iced = \\{ git = [^}]+\\}")
                   (string-append "iced = { path = \"" iced-dir "\" }"))
                  (("iced_core = \\{ git = [^}]+\\}")
                   (string-append "iced_core = { path = \"" iced-dir "/core\" }"))
                  (("iced_wgpu = \\{ git = [^}]+\\}")
                   (string-append "iced_wgpu = { path = \"" iced-dir "/wgpu\" }")))
                ;; Patch iced workspace's own git deps (cryoglyph, winit)
                (let ((iced-cargo (string-append iced-dir "/Cargo.toml")))
                  (when (file-exists? iced-cargo)
                    (substitute* iced-cargo
                      (("cryoglyph = \\{ git = [^}]+\\}")
                       (string-append "cryoglyph = { path = \"../../"
                                      cryoglyph-dir "\" }"))
                      (("winit = \\{ git = [^}]+\\}")
                       (string-append "winit = { path = \"../../"
                                      winit-dir "\" }")))))
                (when (file-exists? "Cargo.lock")
                  (delete-file "Cargo.lock")))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (mesa (assoc-ref inputs "mesa"))
                    (wayland (assoc-ref inputs "wayland"))
                    (libxkbcommon (assoc-ref inputs "libxkbcommon")))
                (wrap-program (string-append out "/bin/halloy")
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append mesa "/lib")
                     ,(string-append wayland "/lib")
                     ,(string-append libxkbcommon "/lib"))))))))))
    (native-inputs (list pkg-config))
    (inputs
     (cons* alsa-lib
            fontconfig
            libxkbcommon
            mesa
            openssl
            wayland
            (px-cargo-inputs 'halloy)))
    (home-page "https://halloy.chat")
    (synopsis "IRC client written in Rust")
    (description
     "Halloy is an open-source IRC client written in Rust with the Iced GUI
framework.  It supports IRCv3 features including SASL authentication, DCC
transfers, auto-completion, desktop notifications, and custom themes.")
    (license license:gpl3+)))

(define-public hatsu
  (package
    (name "hatsu")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/importantimport/hatsu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nrxpczz1wi3fcpz9lrgxgz80kxdrgv0kkjg01ww4g4qq20lv87b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f))
    (native-inputs (list pkg-config))
    (inputs
     (cons* openssl
            sqlite
            (px-cargo-inputs 'hatsu)))
    (home-page "https://github.com/importantimport/hatsu")
    (synopsis "Self-hosted bridge for static sites to the Fediverse")
    (description
     "Hatsu is a self-hosted bridge that brings static websites into the
Fediverse via ActivityPub.  It handles user discovery, content federation,
follow management, and backfeeding of replies from Fediverse users to your
static site.")
    (license license:agpl3+)))

(define %mullvad-vpn-desktop-version "2026.1")

(define (mullvad-vpn-desktop-origin-url system)
  (string-append "https://github.com/mullvad/mullvadvpn-app/releases/"
                 "download/" %mullvad-vpn-desktop-version "/MullvadVPN-"
                 %mullvad-vpn-desktop-version "_" system "64.deb"))

(define* (mullvad-vpn-desktop-origin-values #:key amd64-hash aarch64-hash)
  (match (%current-system)
    ("x86_64-linux"
     (values (mullvad-vpn-desktop-origin-url "amd") amd64-hash))
    ("aarch64-linux"
     (values (mullvad-vpn-desktop-origin-url "arm") aarch64-hash))))

(define-public mullvad-vpn-desktop
  (define-values (url hash)
    (mullvad-vpn-desktop-origin-values
     #:amd64-hash "0gpg5yb1b4fw6zw06ymgicw46v7qj4sf7i5zd5srdhqvn66rlmqy"
     #:aarch64-hash "052rd1vivfclg701galsw5r00qlf9z6xac4v8vh93rkmy3l8av9j"))
  (package
    (name "mullvad-vpn-desktop")
    (version %mullvad-vpn-desktop-version)
    (source
     (origin
       (method url-fetch)
       (uri url)
       (file-name (string-append name "-" version "-" (%current-system) ".deb"))
       (sha256 (base32 hash))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
       ;; There's no point in substitutes.
       #:substitutable? #f
       #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
       #:wrapper-plan
        #~(append
           (list "usr/bin/mullvad"
                 "usr/bin/mullvad-daemon"
                 "usr/bin/mullvad-exclude")
           (map (lambda (file)
                  (string-append "opt/Mullvad VPN/" file))
                '("chrome-sandbox"
                  "chrome_crashpad_handler"
                  "libEGL.so"
                  "libffmpeg.so"
                  "libGLESv2.so"
                  "libvk_swiftshader.so"
                  "libvulkan.so.1"
                  "mullvad-gui"
                  "resources/mullvad-problem-report"
                  "resources/mullvad-setup")))
       #:install-plan
        #~'(("opt/" "/share")
            ("usr/bin/" "/bin")
            ("usr/lib/" "/lib")
            ("usr/local/share/" "/share")
            ("usr/share/" "/share"))
       #:phases
        #~(modify-phases %standard-phases
            (replace 'binary-unpack
              (lambda* (#:key inputs #:allow-other-keys)
                 (invoke "ar" "x" #$source)
                 (invoke "rm" "-v" "control.tar.xz"
                                   "debian-binary"
                                   "_gpgbuilder"
                                   (string-append #$name "-" #$version "-" #$(%current-system) ".deb"))
                 (invoke "tar" "xvf" "data.tar.xz")
                 (invoke "rm" "-vrf" "data.tar.xz" "./usr/bin/mullvad-problem-report")))
            (add-before 'install 'patch-assets
              (lambda _
                 (let* ((bin (string-append #$output "/bin"))
                        (icon (string-append #$output "/share/icons/hicolor/1024x1024/apps/mullvad-vpn.png"))
                        (usr/share "./usr/share")
                        (old-exe "/opt/Mullvad VPN/mullvad-vpn")
                        (exe (string-append bin "/mullvad-vpn")))
                   (patch-shebang (string-append (getcwd) old-exe))
                   (substitute* (string-append usr/share "/applications/mullvad-vpn.desktop")
                    (("^Icon=mullvad-vpn") (string-append "Icon=" icon))
                    (((string-append "^Exec=" old-exe)) (string-append "Exec=" exe))))))
            (add-before 'install-wrapper 'symlink-entrypoint
             (lambda _
               (let* ((bin (string-append #$output "/bin"))
                      (exe (string-append bin "/mullvad-vpn"))
                      (daemon-exe (string-append bin "/mullvad-daemon"))
                      (share (string-append #$output "/share/Mullvad VPN"))
                      (share/resources (string-append share "/resources"))
                      (target (string-append share "/mullvad-vpn")))
                 (symlink (string-append share "/resources/mullvad-problem-report")
                          (string-append bin "/mullvad-problem-report"))
                 (symlink target exe)
                 (wrap-program exe
                   `("MULLVAD_DISABLE_UPDATE_NOTIFICATION" = ("1"))
                   `("LD_LIBRARY_PATH" = (,share)))
                 (wrap-program daemon-exe
                   `("MULLVAD_RESOURCE_DIR" = (,share/resources)))))))))
    (native-inputs (list tar))
    (inputs
     (list iputils
           libnotify))
    (synopsis "The Mullvad VPN client app for desktop")
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (description "This is the VPN client software for the Mullvad VPN service.
For more information about the service, please visit Mullvad's website,
mullvad.net (Also accessible via Tor on this onion service).")
    (home-page "https://mullvad.net")
    (license license:gpl3)))
