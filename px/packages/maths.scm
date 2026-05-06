;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages maths)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages maths))

(define-public libqalculate
  (package
    (inherit (@ (gnu packages maths) libqalculate))
    (version "5.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Qalculate/libqalculate/")
             (commit (string-append "v" version))))
       (file-name (git-file-name "libqalculate" version))
       (sha256
        (base32 "1wra4r099s60lv6m9ab94ybdxlsxqn82h138jkvlziszz9ab94mk"))))))
