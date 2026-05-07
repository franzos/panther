;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages containers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (px packages go)
  #:use-module (px self))

(define-public podman-healthcheckd
  (package
    (name "podman-healthcheckd")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/podman-healthcheckd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q6nrp1r0b7y3amv631l473814rcfn8f8lswf5sncshqh9ss248h"))))
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

(define-public lazydocker
  (package
    (name "lazydocker")
    (version "0.25.2")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/jesseduffield/lazydocker")
                    (commit (string-append "v" version))
                    (sha (base32 "1xip20rv38zcqcn59pzf7fyc6y7g455jpm0s7991sgqbxz89jx1h"))))
              (sha256
               (base32
                "0z9xqy6k8m39n3rap0sk6pgl5gcma5ky3gl5pvf6j14szvi2rqiq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jesseduffield/lazydocker"
      #:install-source? #f
      #:go go-1.24
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check))))
    (home-page "https://github.com/jesseduffield/lazydocker")
    (synopsis "Lazier way to manage everything Docker")
    (description
     "Lazydocker is a terminal user interface for Docker and docker-compose.
It provides a quick overview of containers, services, images, and volumes,
streams logs, displays resource usage stats, and allows running custom
commands against containers without remembering long Docker invocations.")
    (license license:expat)))
