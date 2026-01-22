;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages bluetooth)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages webkit)
  #:use-module (px packages rust-crates)
  #:use-module (ice-9 match))

(define-public overskride
  (package
    (name "overskride")
    (version "0.6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kaii-lb/overskride")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wkvf09q6ds7njn0zhxpkqzvy01vhxksll5b4bzazc7nbv9iz9i4"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:imported-modules (,@%meson-build-system-modules
                           (guix build cargo-utils))
       #:modules ((guix build cargo-utils)
                  (guix build meson-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-for-build
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")
               (("update_desktop_database: true")
                "update_desktop_database: false"))
             (delete-file "Cargo.lock")))
         (add-after 'configure 'unpack-cargo-vendor
           (lambda* (#:key inputs #:allow-other-keys)
             (define (crate-dir? path)
               (file-exists? (string-append path "/Cargo.toml")))
             (define vendor-dir "guix-vendor")
             (mkdir-p vendor-dir)
             ;; Process each input that looks like a Rust crate
             (for-each
              (match-lambda
                ((name . path)
                 (when (and (string-prefix? "rust-" name)
                            (or (crate-dir? path)
                                (file-exists? path)))
                   (let ((dest (string-append vendor-dir "/"
                                 (strip-store-file-name path))))
                     (unless (file-exists? dest)
                       (if (directory-exists? path)
                           (when (crate-dir? path)
                             (copy-recursively path dest))
                           ;; It's a tarball - extract it
                           (begin
                             (mkdir-p dest)
                             (invoke "tar" "xf" path "-C" dest
                                     "--strip-components" "1"))))))))
              inputs)
             ;; Create cargo config (checksums generated later)
             (mkdir-p ".cargo")
             (call-with-output-file ".cargo/config.toml"
               (lambda (port)
                 (format port "[source.crates-io]~%")
                 (format port "replace-with = \"vendored-sources\"~%~%")
                 (format port "[source.vendored-sources]~%")
                 (format port "directory = \"~a\"~%" vendor-dir)))))
         ;; Generate checksums AFTER patch-generated-file-shebangs
         (add-before 'build 'generate-cargo-checksums
           (lambda _
             (generate-all-checksums "guix-vendor"))))))
    (native-inputs
     (list blueprint-compiler
           desktop-file-utils
           `(,glib "bin")
           `(,gtk "bin")
           gettext-minimal
           pkg-config
           rust
           `(,rust "cargo")))
    (inputs
     (cons* bluez
            dbus
            gtk
            libadwaita
            pulseaudio
            (cargo-inputs 'overskride #:module '(px packages rust-crates))))
    (home-page "https://github.com/kaii-lb/overskride")
    (synopsis "Simple yet powerful Bluetooth client")
    (description
     "Overskride is a Bluetooth and OBEX client designed to be straight to the
point, desktop environment and window manager agnostic, and beautiful.  It
provides device enumeration, file transfer via OBEX, device pairing and
authentication, adapter management, battery polling, and audio profile
selection.")
    (license license:gpl3)))

(define-public goveebttemplogger
  (package
    (name "goveebttemplogger")
    (version "3.20251208.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/wcbonner/GoveeBTTempLogger/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "1c4wr1vkrwzdl5fadf22lal2ca97ia41knqmpvk6gg8j95vlrzxr"))))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config))
    (inputs (list bluez dbus))
    (home-page "https://github.com/wcbonner/GoveeBTTempLogger")
    (synopsis "Temperature and Humidity Logger for Govee devices")
    (description
     "Govee H5074, H5075, H5100, H5110, H5174, H5177, H5179,
H5181, H5182, and H5183 Bluetooth Low Energy Temperature and Humidity Logger")
    (license license:expat)))

;; TODO: Switch back to go-build-system once Go dependencies are packaged
(define-public bluetuith
  (package
    (name "bluetuith")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/bluetuith-org/bluetuith/releases/download/v"
             version "/bluetuith_" version "_Linux_x86_64.tar.gz"))
       (sha256
        (base32 "05r7lvpqlxib591zf74i29xg0gpdc7wqip07k7issin42qfp61pj"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan #~'(("bluetuith" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chmod
            (lambda _
              (chmod "bluetuith" #o755))))))
    (inputs (list bluez))
    (home-page "https://github.com/bluetuith-org/bluetuith")
    (synopsis "TUI-based Bluetooth connection manager")
    (description
     "Bluetuith is a TUI-based Bluetooth connection manager which can interact
with Bluetooth adapters and devices.  It aims to be a replacement to most
Bluetooth managers, like blueman.")
    (license license:expat)))

(define-public nothing-linux
  (package
    (name "nothing-linux")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/sn99/nothing-linux/releases/download/v"
             version "/nothing-linux_" version "_amd64.deb"))
       (sha256
        (base32 "0gq7bsjbg0q1zq7g4k9brv58sj23lkk1pl8pznbldzlpf6qzaa9p"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list
      #:patchelf-plan
      #~'(("bin/nothing-linux" ("glibc" "gcc" "gtk+" "webkitgtk-for-gtk3"
                                "glib" "dbus" "zlib" "at-spi2-core"
                                "pango" "harfbuzz" "cairo" "gdk-pixbuf"
                                "libsoup")))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "ar" "x" (assoc-ref inputs "source"))
              (invoke "tar" "-xzf" "data.tar.gz")
              (copy-recursively "usr/" ".")
              (delete-file-recursively "usr")))
          (add-after 'install 'install-desktop-file
            (lambda _
              (let ((apps (string-append #$output "/share/applications")))
                (substitute* "share/applications/nothing-linux.desktop"
                  (("/usr/bin/nothing-linux")
                   (string-append #$output "/bin/nothing-linux")))
                (install-file "share/applications/nothing-linux.desktop" apps))))
          (add-after 'install 'install-icons
            (lambda _
              (for-each
               (lambda (size)
                 (let ((icons (string-append #$output "/share/icons/hicolor/"
                                             size "/apps")))
                   (mkdir-p icons)
                   (when (file-exists? (string-append "share/icons/hicolor/"
                                                      size "/apps/nothing-linux.png"))
                     (copy-file (string-append "share/icons/hicolor/"
                                               size "/apps/nothing-linux.png")
                                (string-append icons "/nothing-linux.png")))))
               '("32x32" "128x128" "256x256@2"))))
          (add-after 'install 'wrap-binary
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (string-append #$output "/bin/nothing-linux")
                `("LD_LIBRARY_PATH" ":" prefix
                  ,(map (lambda (pkg)
                          (string-append (assoc-ref inputs pkg) "/lib"))
                        '("gtk+" "webkitgtk-for-gtk3" "glib" "dbus" "zlib"
                          "at-spi2-core" "glibc" "gcc" "pango" "harfbuzz"
                          "cairo" "gdk-pixbuf" "libsoup")))))))))
    (native-inputs (list gzip tar (@ (gnu packages base) binutils)))
    (inputs (list glibc
                  `(,gcc "lib")
                  gtk+
                  webkitgtk-for-gtk3
                  glib
                  dbus
                  zlib
                  at-spi2-core
                  pango
                  harfbuzz
                  cairo
                  gdk-pixbuf
                  libsoup))
    (home-page "https://github.com/sn99/nothing-linux")
    (synopsis "Control Nothing Ear (2) earbuds on Linux")
    (description
     "Nothing Linux is a desktop application for controlling Nothing Ear (2)
wireless earbuds via Bluetooth.  It provides a graphical interface to manage
settings like low latency mode that would otherwise require the official
Nothing mobile app.")
    (license license:expat)))