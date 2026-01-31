;;; Package Repository for GNU Guix
;;; Copyright © 2021-2024 Hamzeh Nasajpour <h.nasajpour@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages document)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages base)
  #:use-module (px packages rust-crates))

(define-public poppler-next
  (package
    (inherit poppler)
    (name "poppler")
    (version "24.02.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://poppler.freedesktop.org/poppler-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0bi28dii7wyw1lvqpj20hq8s0p6yhg0rjiscc1ykxwq5vlzpl60r"))))
    (arguments
     (substitute-keyword-arguments (package-arguments poppler)
       ((#:configure-flags flags)
        #~(cons* "-DENABLE_GPGME=OFF"
                 "-DENABLE_QT5=OFF"
                 "-DENABLE_QT6=OFF"
                 "-DENABLE_LIBCURL=OFF"
                 #$flags))))))

(define-public featherpad
  (package
    (name "featherpad")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tsujan/FeatherPad/archive/V"
                           version ".tar.gz"))
       (sha256
        (base32 "029ygxwzrncc8q437b9532fgn1caxn50g8pqw3f5as40v29vdsp1"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("hunspell" ,hunspell)
                     ("qtsvg" ,qtsvg)
                     ("qtbase" ,qtbase)))
    (home-page "https://github.com/tsujan/FeatherPad")
    (synopsis "FeatherPad is a lightweight Qt6 plain-text editor for Linux")
    (description "FeatherPad is a lightweight Qt6 plain-text editor for Linux")
    (license license:expat)))

(define-public papers
  (package
    (name "papers")
    (version "47.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.gnome.org/GNOME/papers/-/archive/"
                           version "/papers-" version ".tar.gz"))
       (sha256
        (base32 "1mdc9rkdqqln7hrfy250zz57ihzac8870szx0alsp6bqkvyl36p5"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list "-Dnautilus=false"
              "-Ddocumentation=false"
              "-Dps=disabled")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")
                (("update_desktop_database: true")
                 "update_desktop_database: false"))))
          (add-after 'unpack 'setup-cargo-vendor
            (lambda* (#:key inputs #:allow-other-keys)
              (use-modules (ice-9 ftw)
                           (ice-9 popen)
                           (ice-9 rdelim))
              (let ((vendor-dir "guix-vendor"))
                (mkdir-p vendor-dir)
                ;; Collect tarball->crate-dir mappings for checksum computation
                (let ((tarball-dirs '()))
                  ;; Unpack all rust crate sources into vendor directory
                  (for-each
                   (lambda (input)
                     (when (pair? input)
                       (let ((name (car input))
                             (path (cdr input)))
                         (when (and (string? name)
                                    (string-prefix? "rust-" name)
                                    (string? path)
                                    (file-exists? path))
                           (format #t "Vendoring ~a from ~a~%" name path)
                           ;; Get the directory name from tarball
                           (let* ((port (open-pipe* OPEN_READ "tar" "-tzf" path))
                                  (first-line (read-line port))
                                  (_ (close-pipe port))
                                  (crate-dir-name (and (string? first-line)
                                                       (car (string-split first-line #\/)))))
                             (invoke "tar" "xzf" path "-C" vendor-dir)
                             (when crate-dir-name
                               (set! tarball-dirs
                                     (cons (cons path crate-dir-name)
                                           tarball-dirs))))))))
                   inputs)
                  ;; Add checksum files with package hash from tarball
                  (for-each
                   (lambda (entry)
                     (let* ((tarball-path (car entry))
                            (crate-dir-name (cdr entry))
                            (crate-dir (string-append vendor-dir "/" crate-dir-name))
                            (checksum-file (string-append crate-dir "/.cargo-checksum.json")))
                       (when (and (file-exists? crate-dir)
                                  (file-is-directory? crate-dir)
                                  (not (file-exists? checksum-file)))
                         ;; Compute sha256 of tarball for package checksum
                         (let* ((port (open-pipe* OPEN_READ "sha256sum" tarball-path))
                                (line (read-line port))
                                (_ (close-pipe port))
                                (hash (and (string? line)
                                           (car (string-split line #\space)))))
                           (call-with-output-file checksum-file
                             (lambda (port)
                               (format port "{\"files\":{},\"package\":\"~a\"}" (or hash ""))))
                           (format #t "Created checksum for ~a~%" crate-dir-name)))))
                   tarball-dirs))
                ;; Create .cargo/config.toml to use vendored sources
                (let ((vendor-abs-path (canonicalize-path vendor-dir)))
                  (mkdir-p ".cargo")
                  (call-with-output-file ".cargo/config.toml"
                    (lambda (port)
                      (format port "[source.crates-io]~%")
                      (format port "replace-with = 'vendored-sources'~%~%")
                      (format port "[source.vendored-sources]~%")
                      (format port "directory = '~a'~%" vendor-abs-path)
                      (format port "~%[net]~%")
                      (format port "offline = true~%")))
                  ;; Also set environment variable for cargo
                  (setenv "CARGO_NET_OFFLINE" "true")))))
          (add-before 'build 'setup-cargo-home-config
            (lambda _
              ;; The meson build runs from build/ and sets CARGO_HOME to cargo-home/
              ;; The source is at ../papers-47.3/ relative to build/
              (let* ((build-dir (getcwd))
                     (source-dir (string-append build-dir "/../papers-47.3"))
                     (source-config (string-append source-dir "/.cargo/config.toml"))
                     (cargo-home (string-append build-dir "/cargo-home")))
                (format #t "Build dir: ~a~%" build-dir)
                (format #t "Source config: ~a~%" source-config)
                (format #t "Cargo home: ~a~%" cargo-home)
                (if (file-exists? source-config)
                    (begin
                      (mkdir-p cargo-home)
                      (copy-file source-config
                                 (string-append cargo-home "/config.toml"))
                      (format #t "Copied cargo config to ~a~%" cargo-home))
                    (format #t "WARNING: Config not found at ~a~%" source-config)))))
          (add-before 'validate-runpath 'fix-papers-runpath
            (lambda* (#:key outputs #:allow-other-keys)
              (use-modules (ice-9 popen)
                           (ice-9 rdelim))
              ;; The Rust binary needs RUNPATH to include the output lib directory
              ;; This runs after shrink-runpath which incorrectly removes it
              (let* ((out (assoc-ref outputs "out"))
                     (lib-dir (string-append out "/lib"))
                     (papers-bin (string-append out "/bin/.papers-real")))
                (when (file-exists? papers-bin)
                  ;; Get current RUNPATH and prepend lib-dir
                  (let* ((port (open-pipe* OPEN_READ "patchelf" "--print-rpath" papers-bin))
                         (current-rpath (read-line port))
                         (_ (close-pipe port))
                         (new-rpath (if (and (string? current-rpath)
                                             (not (string-null? current-rpath)))
                                        (string-append lib-dir ":" current-rpath)
                                        lib-dir)))
                    (invoke "patchelf" "--set-rpath" new-rpath papers-bin)
                    (format #t "Fixed RUNPATH for ~a to ~a~%" papers-bin new-rpath)))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           itstool
           patchelf
           pkg-config
           `(,rust "cargo")
           rust))
    (inputs
     (append
      (list adwaita-icon-theme
            dbus
            djvulibre
            exempi
            glib
            gsettings-desktop-schemas
            gtk
            libadwaita
            libarchive
            libgxps
            libsecret
            libspelling
            libtiff
            libxml2
            poppler-next
            zlib)
      (cargo-inputs 'papers #:module '(px packages rust-crates))))
    (home-page "https://gitlab.gnome.org/GNOME/papers")
    (synopsis "Document viewer for PDF and other formats")
    (description
     "Papers is a document viewer for the GNOME desktop, capable of displaying
PDF, DjVu, TIFF, XPS, and comic book archive formats.  It features a modern
GTK4 interface with libadwaita, annotations, printing support, and integration
with the GNOME desktop environment.")
    (license license:gpl2+)))
