;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages vnc)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

;; wayvnc 0.10 and neatvnc 1.0 link against the aml1 pkg-config module, which
;; only exists from aml 1.0 onward.
(define-public aml-1
  (package
    (inherit aml)
    (name "aml")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/any1/aml")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cp0hmlfrsvmdrdhcr2b4msvhv80zx43bxqq995vlrk1ibljcj6p"))))))

(define-public neatvnc-1
  (package
    (inherit neatvnc)
    (name "neatvnc")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/any1/neatvnc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q7sqck7xvnxk0sr43z9hmph3q85b04vcxdip6jiy5p8vgf721v5"))))
    (inputs '())
    ;; These land in neatvnc.pc's Requires.private, which pkg-config resolves
    ;; even for --cflags, so a consumer's configure fails unless they are on
    ;; its search path.  nettle, hogweed and gmp provide the RSA-AES security
    ;; types.
    (propagated-inputs
     (list aml-1
           gmp
           gnutls
           libdrm
           libjpeg-turbo
           mesa
           nettle
           pixman
           zlib))))

(define-public wayvnc-0.10
  (package
    (inherit wayvnc)
    (name "wayvnc")
    (version "0.10.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/any1/wayvnc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "082g6kfn66yqjr2qi3dx6m381ljk0zs1mr2qbq2ypsnf7srbwnsx"))))
    ;; The pam option defaults to "auto", which would quietly produce a wayvnc
    ;; without PAM support if detection ever broke.
    (arguments
     (list #:configure-flags #~(list "-Dpam=enabled")))
    (inputs (list aml-1
                  jansson
                  libdrm
                  libxkbcommon
                  linux-pam
                  mesa
                  neatvnc-1
                  pixman
                  wayland))))
