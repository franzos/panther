;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>
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
    (version "1.17.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/iotaledger/iota/releases/download/v"
             version "/iota-v" version "-linux-x86_64.tgz"))
       (sha256
        (base32 "1vn560n5n9l4sn1pv25ssf2z9x4wvl00x9rmmq2rsvfr55q873yi"))))
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

(define-public jota
  (package
    (name "jota")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/jota")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zn92s5wxsbk401vg7c544d474qwydaafwmhk7h9d66bja8yryqq"))))
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
                (install-file "target/release/jota" bin)
                (install-file "target/release/jota-gui" bin)
                (let ((wayland-lib (string-append (assoc-ref inputs "wayland") "/lib"))
                      (xkbcommon-lib (string-append (assoc-ref inputs "libxkbcommon") "/lib"))
                      (vulkan-lib (string-append (assoc-ref inputs "vulkan-loader") "/lib"))
                      (gui-binary (string-append bin "/jota-gui")))
                  (invoke "patchelf" "--add-rpath"
                          (string-join (list wayland-lib xkbcommon-lib vulkan-lib) ":")
                          gui-binary))))))))
    (native-inputs (list patchelf pkg-config))
    (inputs
     (cons* eudev
            libxkbcommon
            sqlite
            vulkan-loader
            wayland
            (px-cargo-inputs 'jota)))
    (home-page "https://github.com/franzos/jota")
    (synopsis "Monero-inspired wallet for IOTA Rebased")
    (description
     "A desktop wallet for IOTA Rebased with both an interactive CLI and a
native GUI.  Features include encrypted wallet files, one-shot commands for
scripting, and support for sending, receiving, and staking IOTA tokens.")
    (license license:expat)))
