;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages finance)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages serialization)
  #:use-module (px packages python-xyz)
  #:use-module (px packages rust)
  #:use-module (px self))

(define-public electrum-cc
  (package
    (inherit electrum)
    (name "electrum-cc")
    (inputs
     (list electrum-aionostr
           python-aiohttp
           python-aiohttp-socks
           python-aiorpcx
           python-attrs
           python-certifi
           python-cryptography
           python-dnspython
           python-electrum-ecc
           python-hidapi
           python-jsonpatch
           python-protobuf
           python-pyaes
           python-pyqt-6
           python-qdarkstyle
           python-qrcode
           zbar
           ;; Coldcard support
           python-cbor
           python-ckcc-protocol))))

(define-public flowsurface
  (package
    (name "flowsurface")
    (version "0.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flowsurface-rs/flowsurface")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02jk1vw7niipz2bghdb9s42a1531cnxckql6ypk19qn6q24qgl7a"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:tests? #f
      #:rust rust-1.89
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'patch-git-deps-to-paths
            (lambda _
              (define (find-vendor-dir pattern)
                (let ((dirs (find-files "guix-vendor" pattern
                                        #:directories? #t
                                        #:fail-on-error? #f)))
                  (if (pair? dirs) (car dirs) #f)))
              (let ((iced-dir (find-vendor-dir "^rust-iced-0\\.14"))
                    (winit-dir (find-vendor-dir "^rust-winit-0\\.30"))
                    (cryoglyph-dir (find-vendor-dir "^rust-cryoglyph"))
                    (cargo-hot-dir (find-vendor-dir "^rust-cargo-hot")))
                (substitute* "Cargo.toml"
                  (("iced = \\{ git = [^}]+\\}")
                   (string-append "iced = { path = \"" iced-dir "\" }"))
                  (("iced_futures = \\{ git = [^}]+\\}")
                   (string-append "iced_futures = { path = \"" iced-dir "/futures\" }"))
                  (("iced_core = \\{ git = [^}]+\\}")
                   (string-append "iced_core = { path = \"" iced-dir "/core\" }")))
                (let ((iced-cargo (string-append iced-dir "/Cargo.toml")))
                  (when (file-exists? iced-cargo)
                    (substitute* iced-cargo
                      (("cargo-hot = \\{ package = \"cargo-hot-protocol\", git = [^}]+\\}")
                       (string-append "cargo-hot = { package = \"cargo-hot-protocol\", path = \"../../"
                                      cargo-hot-dir "/protocol\" }"))
                      (("cryoglyph = \\{ git = [^}]+\\}")
                       (string-append "cryoglyph = { path = \"../../" cryoglyph-dir "\" }"))
                      (("winit = \\{ git = [^}]+\\}")
                       (string-append "winit = { path = \"../../" winit-dir "\" }")))))
                (when (file-exists? "Cargo.lock")
                  (delete-file "Cargo.lock")))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (mkdir-p bin)
                (install-file "target/release/flowsurface" bin)
                (let ((wayland-lib (string-append (assoc-ref inputs "wayland") "/lib"))
                      (xkbcommon-lib (string-append (assoc-ref inputs "libxkbcommon") "/lib"))
                      (vulkan-lib (string-append (assoc-ref inputs "vulkan-loader") "/lib"))
                      (binary (string-append bin "/flowsurface")))
                  (invoke "patchelf" "--add-rpath"
                          (string-join (list wayland-lib xkbcommon-lib vulkan-lib) ":")
                          binary))))))))
    (native-inputs (list patchelf pkg-config))
    (inputs
     (cons* alsa-lib
            libxkbcommon
            vulkan-loader
            wayland
            (list zstd "lib")
            (px-cargo-inputs 'flowsurface)))
    (home-page "https://github.com/flowsurface-rs/flowsurface")
    (synopsis "Desktop charting application for cryptocurrency markets")
    (description
     "Flowsurface is a desktop charting application for cryptocurrency market
analysis.  It provides heatmaps, candlestick charts, footprint analysis, trade
listings, and depth-of-market displays across multiple exchanges including
Binance, Bybit, and OKX.")
    (license license:gpl3+)))