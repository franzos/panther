;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages networking)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses)
                :prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix records)
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
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages glib)
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
  #:use-module (px packages golang-xyz)
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

(define-public v2ray
  (package
    (name "v2ray")
    (version "5.44.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/v2fly/v2ray-core/releases/download/v" version
             "/v2ray-linux-64.zip"))
       (sha256
        (base32 "1sw5qbx0gn88irwa4y5fax9vyvrikyc2azl94f4s6vmv606xxlyp"))))
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

(define-record-type* <go-git-reference>
  go-git-reference make-go-git-reference
  go-git-reference?
  (url go-git-reference-url)
  (commit go-git-reference-commit)
  (sha go-git-reference-sha256))

(define-record-type* <go-url-reference>
  go-url-reference make-go-url-reference
  go-url-reference?
  (url go-url-reference-url)
  (sha go-url-reference-sha))

(define* (go-fetch-vendored uri hash-algorithm hash-value name #:key system)
  (let ((src
         (match uri
           (($ <go-git-reference> url commit sha)
            (origin
              (method git-fetch)
              (uri (git-reference
                    (url url)
                    (commit commit)))
              (sha256 sha)))
           (($ <go-url-reference> url commit sha)
            (origin
              (method url-fetch)
              (uri url)
              (sha256 sha)))))
        (name (or name "go-git-checkout")))
    (gexp->derivation
     (string-append name "-vendored.tar.gz")
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           (let ((inputs (list
                          #+go-1.25
                          #+tar
                          #+bzip2
                          #+gzip)))
             (set-path-environment-variable "PATH" '("/bin") inputs))
           (mkdir "source")
           (chdir "source")
           (if (file-is-directory? #$src)
               (begin
                 (copy-recursively #$src "."
                                   #:keep-mtime? #t)
                 (for-each (lambda (f)
                             (false-if-exception (make-file-writable f)))
                           (find-files ".")))
               (begin
                 (cond
                  ((string-suffix? ".zip" #$src)
                   (invoke "unzip" #$src))
                  ((tarball? #$src)
                   (invoke "tar" "xvf" #$src))
                  (else
                   (let ((name (strip-store-file-name #$src))
                         (command (compressor #$src)))
                     (copy-file #$src name)
                     (when command
                       (invoke command "--decompress" name)))))))

           (setenv "GOCACHE" "/tmp/gc")
           (setenv "GOMODCACHE" "/tmp/gmc")
           (setenv "SSL_CERT_DIR" #+(file-append nss-certs "/etc/ssl/certs/"))

           (invoke "go" "mod" "vendor")

           (invoke "tar" "czvf" #$output
                   "--mtime=@0"
                   "--owner=root:0"
                   "--group=root:0"
                   "--sort=name"
                   "--hard-dereference"
                   ".")))
     #:hash hash-value
     #:hash-algo hash-algorithm)))

(define-public tailscale
  (let ((version "1.94.1"))
    (package
      (name "tailscale")
      (version version)
      (source (origin
                (method go-fetch-vendored)
                (uri (go-git-reference
                      (url "https://github.com/tailscale/tailscale")
                      (commit "v1.94.1")
                      (sha (base32 "0n5k8adplcffjwlpiy35nrklc0sf9ms18zlilcis5g1s3kdh4klf"))))
                (sha256
                 (base32
                  "1mdrq6yj3jnn6gp03lv1k7zapzp5iqshp8cxvlxrxd16dh8806j6"))))
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
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oha" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "095dfi7xrcgrisdgb1s2richwpxy7i4sdhwd46ikwfsb3w060yps"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.86
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
