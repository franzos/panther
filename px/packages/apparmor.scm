;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages apparmor)
  #:use-module (gnu packages compression)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;;;
;;; AppArmor, ahead of the 4.1.2 in Guix.
;;;
;;; 4.1.2 can't emit network_v9 policy, so a file rule denying a unix socket
;;; path stops open() but not connect() - the socket stays reachable and the
;;; rule looks like it works.  5.0 ships abi/5.0 and the encoder for it.
;;;

(define %apparmor-version "5.0.2")

(define (apparmor-source name)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.com/apparmor/apparmor")
          (commit (string-append "v" %apparmor-version))))
    (file-name (git-file-name name %apparmor-version))
    (sha256
     (base32 "1629c3xd5mkhdwvlk52acc6pigj9zn9703lh1d6ax2pqphv3krra"))))

(define-public libapparmor
  (package
    (inherit (@ (gnu packages apparmor) libapparmor))
    (version %apparmor-version)
    (source (apparmor-source "libapparmor"))))

(define-public apparmor
  (package
    (inherit (@ (gnu packages apparmor) apparmor))
    (version %apparmor-version)
    (source (apparmor-source "apparmor"))
    ;; New in 5.0: cached policy is compressed, parser.h includes <zstd.h>.
    (inputs (list `(,zstd "lib")))
    (propagated-inputs (list libapparmor))))
