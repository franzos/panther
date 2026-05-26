;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages maths)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages maths))

(define-public libqalculate
  (package
    (inherit (@ (gnu packages maths) libqalculate))
    (version "5.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Qalculate/libqalculate/")
             (commit (string-append "v" version))))
       (file-name (git-file-name "libqalculate" version))
       (sha256
        (base32 "0kmjgssrwm9djn8pb44vyajzrzxsmxzkjrdz2hvbslfql9ykc04p"))))))

(define-public qalculate-gtk
  (package
    (inherit (@ (gnu packages maths) qalculate-gtk))
    (version "5.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Qalculate/qalculate-gtk/")
             (commit (string-append "v" version))))
       (file-name (git-file-name "qalculate-gtk" version))
       (sha256
        (base32 "0yrbbpfya60d51md84dyxk7sb0rx8jcxnpvgwxpvni2b4jayi5r5"))))
    (inputs
     (modify-inputs (package-inputs (@ (gnu packages maths) qalculate-gtk))
       (replace "libqalculate" libqalculate)))))
