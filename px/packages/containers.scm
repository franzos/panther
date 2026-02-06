;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages containers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix git-download)
  #:use-module (px self))

(define-public podman-healthcheckd
  (package
    (name "podman-healthcheckd")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/podman-healthcheckd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dhjavv02b7qmblyyikvgvmcmxnx1l3a3fa2cnb4ah4qnqamhr3f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs
     (px-cargo-inputs 'podman-healthcheckd))
    (home-page "https://github.com/franzos/podman-healthcheckd")
    (synopsis "Podman healthcheck scheduler for systems without systemd")
    (description
     "Podman-healthcheckd is a daemon that schedules and runs Podman container
healthchecks on systems that do not use systemd.  It monitors running containers
and executes their configured healthcheck commands at the specified intervals.")
    (license license:expat)))
