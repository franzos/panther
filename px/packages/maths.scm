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
    (version "5.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Qalculate/libqalculate/")
             (commit (string-append "v" version))))
       (file-name (git-file-name "libqalculate" version))
       (sha256
        (base32 "1w9ncjsgj3lbqzpxdn272mc5c4b7arlsljlra3bnb3jljppqfx45"))))))

(define-public qalculate-gtk
  (package
    (inherit (@ (gnu packages maths) qalculate-gtk))
    (version "5.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Qalculate/qalculate-gtk/")
             (commit (string-append "v" version))))
       (file-name (git-file-name "qalculate-gtk" version))
       (sha256
        (base32 "129pcbwvjlc4w89r21lsx6s2ym0023633vyrxl54n1qaxy5g8jbk"))))
    (inputs
     (modify-inputs (package-inputs (@ (gnu packages maths) qalculate-gtk))
       (replace "libqalculate" libqalculate)))))
