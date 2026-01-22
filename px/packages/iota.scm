;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Franz Geffke <franz@pantherx.org>
;;;
;;; This file is part of Panther.

(define-module (px packages iota)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (nonguix build-system binary))

(define-public iota
  (package
    (name "iota")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/iotaledger/iota/releases/download/v"
             version "/iota-v" version "-linux-x86_64.tgz"))
       (sha256
        (base32 "13sg6w7xp2py6m15bifyh3wz03y66zw1y9ys3vwg0pq8cb6266ia"))))
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
