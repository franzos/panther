;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages sof)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (px packages python-xyz))

(define-public sof-bin
  (package
    (name "sof-bin")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/thesofproject/sof-bin")
             (commit "a06502dfe1d6d7c00fa355788430dab7527dc829")
             (recursive? #t)))
       (sha256
        (base32 "17bl463qm01d534fld7gb4cd658y0hyzd1p3vvj7n08k8ggp8q9y"))
       (file-name (git-file-name name version))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (bash (assoc-ref %build-inputs "bash"))
                          (coreutils (assoc-ref %build-inputs "coreutils"))
                          (lib-dir (string-append %output "/lib/firmware")))
                     (mkdir-p lib-dir)
                     (setenv "PATH"
                             (string-append bash "/bin:" coreutils "/bin"))
                     (copy-recursively source ".")
                     (substitute* "go.sh"
                       (("\\$\\{ROOT\\}")
                        %output))
                     (substitute* "go.sh"
                       (("\\$\\{VERSION\\}")
                        ,version))
                     (invoke (string-append bash "/bin/bash") "go.sh")
                     #t))))
    (inputs `(("bash" ,bash)
              ("coreutils" ,coreutils)))
    (home-page "https://thesofproject.github.io")
    (synopsis "SOF Firmware and Topology Binaries.")
    (description
     "This is the living area and distribution channel for SOF firmware and topology binaries. It's still very much WiP and may churn a little until things settle down.")
    (license license:lgpl2.1+)))

