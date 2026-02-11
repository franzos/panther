;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Franz Geffke <franz@pantherx.org>
;;;
;;; This file is part of Panther.

(define-module (px packages iota)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (nonguix build-system binary)
  #:use-module (px packages rust)
  #:use-module (px self))

(define-public iota
  (package
    (name "iota")
    (version "1.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/iotaledger/iota/releases/download/v"
             version "/iota-v" version "-linux-x86_64.tgz"))
       (sha256
        (base32 "1vza3pjyv7i6nvh09nwglkm41jg55rrn5fh2bsmvcl0fkasf27im"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:patchelf-plan
      #~'(("iota" ("libc" "gcc" "eudev" "postgresql"))
          ("iota-node" ("libc" "gcc"))
          ("iota-indexer" ("libc" "gcc" "postgresql"))
          ("iota-tool" ("libc" "gcc"))
          ("iota-faucet" ("libc" "gcc" "postgresql"))
          ("iota-graphql-rpc" ("libc" "gcc" "postgresql"))
          ("iota-data-ingestion" ("libc" "gcc"))
          ("move-analyzer" ("libc" "gcc")))
      #:install-plan
      #~'(("iota" "/bin/")
          ("iota-node" "/bin/")
          ("iota-indexer" "/bin/")
          ("iota-tool" "/bin/")
          ("iota-faucet" "/bin/")
          ("iota-graphql-rpc" "/bin/")
          ("iota-data-ingestion" "/bin/")
          ("move-analyzer" "/bin/"))))
    (inputs
     (list eudev
           `(,gcc "lib")
           postgresql))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/iotaledger/iota")
    (synopsis "Scalable distributed ledger technology infrastructure")
    (description
     "IOTA is a distributed ledger technology (DLT) infrastructure bringing
Web3 capabilities with a scalable, decentralized and programmable platform.
It uses an asset-oriented programming model built on the Move programming
language.  This package includes the CLI client, node software, indexer,
and related tools.")
    (license license:asl2.0)))

(define-public iota-wallet
  (package
    (name "iota-wallet")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/iota-wallet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "182babxx6y35vx4lwylpbmfly8w3snaqv1j42c2qjbskrkvm096r"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:tests? #f
      #:rust rust-1.89
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (mkdir-p bin)
                (install-file "target/release/iota-wallet" bin)
                (install-file "target/release/iota-wallet-gui" bin)
                (let ((wayland-lib (string-append (assoc-ref inputs "wayland") "/lib"))
                      (xkbcommon-lib (string-append (assoc-ref inputs "libxkbcommon") "/lib"))
                      (vulkan-lib (string-append (assoc-ref inputs "vulkan-loader") "/lib"))
                      (gui-binary (string-append bin "/iota-wallet-gui")))
                  (invoke "patchelf" "--add-rpath"
                          (string-join (list wayland-lib xkbcommon-lib vulkan-lib) ":")
                          gui-binary))))))))
    (native-inputs (list patchelf pkg-config))
    (inputs
     (cons* libxkbcommon
            sqlite
            vulkan-loader
            wayland
            (px-cargo-inputs 'iota-wallet)))
    (home-page "https://github.com/franzos/iota-wallet")
    (synopsis "Monero-inspired wallet for IOTA Rebased")
    (description
     "A desktop wallet for IOTA Rebased with both an interactive CLI and a
native GUI.  Features include encrypted wallet files, one-shot commands for
scripting, and support for sending, receiving, and staking IOTA tokens.")
    (license license:expat)))
