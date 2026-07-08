;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages containers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust)
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

;; Pods 2.4.0 ports its sidebar to Adw.Sidebar and enables libadwaita's
;; "v1_9" feature, so it needs the 1.9 widgets; the channel-wide libadwaita
;; stays on 1.8.2.  gtk 4.22 and glib 2.86 in guix already satisfy 1.9.1's
;; >= 4.21.1 / >= 2.84.0 build requirements.
(define-public libadwaita-1.9
  (package
    (inherit libadwaita)
    (name "libadwaita")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libadwaita/"
                                  (version-major+minor version) "/"
                                  "libadwaita-" version ".tar.xz"))
              (sha256
               (base32
                "0rhsqivm76k1nw81h7vlib42wj2hc2iyyz3h4l4jfvd57sxlvqra"))))))

;; blueprint-compiler wraps its binary with the GI_TYPELIB_PATH captured at
;; build time (see gi-wrap phase upstream), which bakes in its native-input
;; libadwaita.  pods' .blp files reference Adw.Sidebar (1.9), so the compiler
;; must see the 1.9 typelib; rebuild it against libadwaita-1.9.
(define blueprint-compiler-for-pods
  (package
    (inherit blueprint-compiler)
    (native-inputs
     (modify-inputs (package-native-inputs blueprint-compiler)
       (replace "libadwaita" libadwaita-1.9)))))

(define-public pods
  (package
    (name "pods")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marhkb/pods")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1awfk9h4fxk4wddxdhyj8sfvgyzjzfyy4hapfl461qpyjbc07aps"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:tests? #f
      #:imported-modules `(,@%meson-build-system-modules
                           ,@%cargo-build-system-modules)
      #:modules `(((guix build cargo-build-system) #:prefix cargo:)
                  (guix build meson-build-system)
                  (guix build utils))
      #:phases
      (with-extensions (list (cargo-guile-json))
        #~(modify-phases %standard-phases
            (add-after 'unpack 'prepare-for-build
              (lambda _
                (substitute* "meson.build"
                  (("gtk_update_icon_cache: true")
                   "gtk_update_icon_cache: false")
                  (("update_desktop_database: true")
                   "update_desktop_database: false"))
                (delete-file "Cargo.lock")))
            (add-after 'configure 'prepare-cargo-build-system
              (lambda args
                (for-each
                 (lambda (phase)
                   (format #t "Running cargo phase: ~a~%" phase)
                   (apply (assoc-ref cargo:%standard-phases phase)
                          #:vendor-dir "vendor"
                          #:cargo-target #$(cargo-triplet)
                          args))
                 ;; 'check-for-pregenerated-files is omitted: the winapi
                 ;; *-windows-gnu crates ship prebuilt .a files it flags,
                 ;; which are never used on this platform.
                 '(unpack-rust-crates
                   configure
                   patch-cargo-checksums))))))))
    (native-inputs
     (append
      (list blueprint-compiler-for-pods
            desktop-file-utils
            gettext-minimal
            `(,glib "bin")
            pkg-config
            rust
            `(,rust "cargo"))
      (or (and=> (%current-target-system)
                 (compose list make-rust-sysroot))
          '())))
    (inputs
     (cons* glib
            graphene
            gtk
            gtksourceview
            libadwaita-1.9
            pango
            vte
            (px-cargo-inputs 'pods)))
    (home-page "https://github.com/marhkb/pods")
    (synopsis "Podman desktop client for GNOME")
    (description
     "Pods is a graphical client for @command{podman}, built with GTK4 and
libadwaita.  It lets you manage containers, pods, and images: create and run
containers, inspect logs and resource usage, open a terminal inside a
container, build and pull images, and group containers into pods.  A running
Podman service is required at run time.")
    (license license:gpl3+)))

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

(define-public netavark
  (package
    (inherit (@ (gnu packages rust-apps) netavark))
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "netavark" version))
       (file-name (string-append "netavark-" version ".tar.gz"))
       (sha256
        (base32 "12ibyylgnwps6f298jjb49qnr3q9prahzdb4jskhx1gzxhyfxxil"))))
    (inputs (px-cargo-inputs 'netavark))))
