;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages terraform)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (gnu packages golang)
  #:use-module (px packages go))

(define-public opentofu
  (package
    (name "opentofu")
    (version "1.12.4")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/opentofu/opentofu")
                    (commit (string-append "v" version))
                    (sha (base32
                          "1m237055s5c7a4v9frwyzg8shzdm0jkjal9ms7bbd47yxclszr2s"))))
              (sha256
               (base32
                "1lx2kw38xhl6yphl0lvfygvyqafycip63z8ka8x8lygn0svq47z2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/opentofu/opentofu/cmd/tofu"
      #:unpack-path "github.com/opentofu/opentofu"
      #:install-source? #f
      #:go go-1.25
      #:build-flags
      #~(list "-ldflags=-s -w -X github.com/opentofu/opentofu/version.dev=no")
      #:phases
      #~(modify-phases %standard-phases
          ;; The released git tag embeds a stale version/VERSION file, so
          ;; stamp the real version like upstream's release pipeline does.
          (add-after 'unpack 'set-version
            (lambda _
              (call-with-output-file
                  "src/github.com/opentofu/opentofu/version/VERSION"
                (lambda (port) (display #$version port)))))
          (delete 'check))))
    (home-page "https://opentofu.org")
    (synopsis "Open-source infrastructure as code tool")
    (description
     "OpenTofu is a community-driven fork of Terraform, providing a declarative
infrastructure-as-code workflow.  It lets you define cloud and on-premises
resources in human-readable configuration files that can be versioned, reused,
and shared.  OpenTofu generates an execution plan, builds a dependency graph,
and applies changes in the correct order.")
    (license license:mpl2.0)))
