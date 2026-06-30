;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages graphics)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system meson)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (px self))

(define-public rapidraw
  (package
    (name "rapidraw")
    (version "1.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/CyberTimon/RapidRAW/releases/download/v"
             version "/03_RapidRAW_v" version "_ubuntu-24.04_amd64.deb"))
       (sha256
        (base32 "0xhh4q98xcgrd6h14988cciv5s93fx8rvriscbbq2pv6k8y7vjav"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("usr/bin/RapidRAW"
                          ("glib" "gtk+" "gdk-pixbuf" "cairo"
                           "webkitgtk-for-gtk3" "libsoup" "openssl" "gcc:lib"
                           "vulkan-loader" "mesa"))
                         ("usr/lib/RapidRAW/resources/libonnxruntime.so"
                          ("gcc:lib" "glibc")))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "ar" "x" (assoc-ref inputs "source"))
             (invoke "tar" "-xzf" "data.tar.gz")
             (delete-file "control.tar.gz")
             (delete-file "data.tar.gz")
             (delete-file "debian-binary")))
         (add-after 'install 'install-extras
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib/RapidRAW"))
                    (share (string-append out "/share"))
                    (apps (string-append share "/applications"))
                    (icons (string-append share "/icons")))
               ;; Create directories
               (mkdir-p bin)
               (mkdir-p lib)
               (mkdir-p apps)
               ;; Copy resources
               (copy-recursively "usr/lib/RapidRAW" lib)
               ;; Install desktop file
               (copy-file "usr/share/applications/RapidRAW.desktop"
                          (string-append apps "/RapidRAW.desktop"))
               (substitute* (string-append apps "/RapidRAW.desktop")
                 (("Exec=RapidRAW")
                  (string-append "Exec=" out "/bin/rapidraw"))
                 (("Icon=RapidRAW")
                  (string-append "Icon=" out "/share/icons/hicolor/128x128/apps/RapidRAW.png")))
               ;; Install icons
               (copy-recursively "usr/share/icons" icons))))
         (add-after 'install-extras 'create-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib/RapidRAW")))
               (wrap-program (string-append out "/usr/bin/RapidRAW")
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append lib "/resources")
                    ,(string-append (assoc-ref inputs "vulkan-loader") "/lib")
                    ,(string-append (assoc-ref inputs "mesa") "/lib")
                    ,(string-append (assoc-ref inputs "webkitgtk-for-gtk3") "/lib")
                    ,(string-append (assoc-ref inputs "libsoup") "/lib")
                    ,(string-append (assoc-ref inputs "gtk+") "/lib")
                    ,(string-append (assoc-ref inputs "glib") "/lib")
                    ,(string-append (assoc-ref inputs "gdk-pixbuf") "/lib")
                    ,(string-append (assoc-ref inputs "cairo") "/lib")
                    ,(string-append (assoc-ref inputs "openssl") "/lib")
                    ,(string-append (assoc-ref inputs "gcc:lib") "/lib")))
                 ;; Point to hardware GPU Vulkan drivers (exclude llvmpipe)
                 `("VK_ICD_FILENAMES" ":" prefix
                   (,(string-append (assoc-ref inputs "mesa")
                                    "/share/vulkan/icd.d/radeon_icd.x86_64.json")
                    ,(string-append (assoc-ref inputs "mesa")
                                    "/share/vulkan/icd.d/intel_icd.x86_64.json")
                    ,(string-append (assoc-ref inputs "mesa")
                                    "/share/vulkan/icd.d/nouveau_icd.x86_64.json"))))
               ;; Create symlink in bin
               (symlink (string-append out "/usr/bin/RapidRAW")
                        (string-append bin "/rapidraw"))))))))
    (native-inputs `(("binutils" ,binutils)))
    (inputs `(("bash-minimal" ,bash-minimal)
              ("cairo" ,cairo)
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("glib" ,glib)
              ("glibc" ,glibc)
              ("gcc:lib" ,gcc "lib")
              ("gtk+" ,gtk+)
              ("libsoup" ,libsoup)
              ("mesa" ,mesa)
              ("openssl" ,openssl)
              ("vulkan-loader" ,vulkan-loader)
              ("webkitgtk-for-gtk3" ,webkitgtk-for-gtk3)))
    (home-page "https://github.com/CyberTimon/RapidRAW")
    (synopsis "GPU-accelerated RAW image editor")
    (description
     "RapidRAW is a high-performance, non-destructive RAW image editor built
with Rust and Tauri.  It features GPU-accelerated processing via WGPU,
support for various camera RAW formats through the rawler library, and
optional AI-powered features using ONNX Runtime.")
    (license license:agpl3+)))

(define-public vipsdisp
  (package
    (name "vipsdisp")
    (version "4.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/libvips/vipsdisp/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "1ky88pvk68sj7pcfh8qda0h3s04pqwdpgsp559s7mpsankja2dw9"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t))
    (native-inputs
     (list `(,glib "bin")
           pkg-config
           python))
    (inputs
     (list gtk
           vips))
    (home-page "https://github.com/libvips/vipsdisp")
    (synopsis "Image viewer for libvips")
    (description
     "Vipsdisp is a lightweight image viewer powered by libvips and GTK4.
It can display very large images quickly and supports a wide range of image
formats including TIFF, PNG, JPEG, RAW, OpenEXR, FITS, and many scientific
and technical formats.  It handles various pixel types from 1-bit mono to
128-bit double precision complex.")
    (license license:expat)))

(define-public oculante
  (package
    (name "oculante")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oculante" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jdwz5i01bmvm9n5bhs3wly6295s13ca5jv9991ysmbpd1n83bq4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-cmake-policy
           (lambda _
             ;; Workaround for bundled glslang having old cmake_minimum_required
             (setenv "CMAKE_POLICY_VERSION_MINIMUM" "3.5")))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (mesa (assoc-ref inputs "mesa"))
                   (vulkan (assoc-ref inputs "vulkan-loader"))
                   (wayland (assoc-ref inputs "wayland"))
                   (libxkbcommon (assoc-ref inputs "libxkbcommon")))
               (wrap-program (string-append out "/bin/oculante")
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append mesa "/lib")
                    ,(string-append vulkan "/lib")
                    ,(string-append wayland "/lib")
                    ,(string-append libxkbcommon "/lib"))))))))))
    (native-inputs
     (list cmake
           nasm
           perl
           pkg-config))
    (inputs
     (cons* alsa-lib
            expat
            fontconfig
            freetype
            gtk+
            libxcb
            libxkbcommon
            mesa
            vulkan-loader
            wayland
            `(,zstd "lib")
            (px-cargo-inputs 'oculante)))
    (home-page "https://github.com/woelper/oculante")
    (synopsis "Minimalistic image viewer with analysis and editing tools")
    (description
     "Oculante is a fast, minimalistic, cross-platform image viewer with
analysis and editing capabilities.  It supports numerous image formats
including common formats like PNG, JPEG, GIF, and specialized formats like
RAW, PSD, HDR, HEIC, and AVIF.  Features include GPU-accelerated rendering,
color analysis tools, basic editing operations, and support for animated
images.")
    (license license:expat)))

(define-public oculante-next
  ;; Development branch tracking the egui 0.34 / Rust 2024 rewrite.
  (let ((revision "0")
        (commit "39f4e92dd081980fdb1fd93e3278bf9e270bec75"))
    (package
      (inherit oculante)
      (name "oculante-next")
      (version (git-version "0.9.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/woelper/oculante")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "09hf8npvwp942anraq9wjs24fqrg4fdvz1wwdwhlc2jiks38b2sw"))
         (snippet
          #~(begin
              (use-modules (guix build utils))
              (substitute* "Cargo.toml"
                ;; Resolve the pinned dark-light git revision from the
                ;; vendored crate source instead of fetching it offline.
                (("^dark-light = \\{ git =.*")
                 "dark-light = \"2.0.0\"\n")
                ;; sysinfo 0.39 requires rustc 1.95 (cfg_select!); 0.38 keeps
                ;; the same Disks API and builds with the packaged toolchain.
                (("^sysinfo = \"0.39\"") "sysinfo = \"0.38\""))))))
      (arguments
       `(#:install-source? #f
         #:tests? #f
         #:rust ,rust-1.94
         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'set-cmake-policy
             (lambda _
               ;; Workaround for bundled glslang having old cmake_minimum_required
               (setenv "CMAKE_POLICY_VERSION_MINIMUM" "3.5")))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (mesa (assoc-ref inputs "mesa"))
                     (vulkan (assoc-ref inputs "vulkan-loader"))
                     (wayland (assoc-ref inputs "wayland"))
                     (libxkbcommon (assoc-ref inputs "libxkbcommon")))
                 (wrap-program (string-append out "/bin/oculante")
                   `("LD_LIBRARY_PATH" ":" prefix
                     (,(string-append mesa "/lib")
                      ,(string-append vulkan "/lib")
                      ,(string-append wayland "/lib")
                      ,(string-append libxkbcommon "/lib"))))))))))
      (inputs
       (cons* alsa-lib
              expat
              fontconfig
              freetype
              gtk+
              libxcb
              libxkbcommon
              mesa
              vulkan-loader
              wayland
              `(,zstd "lib")
              (px-cargo-inputs 'oculante-next))))))

(define hayro-source
  ;; rnote depends on hayro via a pinned git revision; vendor it locally.
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/LaurenzV/hayro")
          (commit "56c62b2a25232d5407af8efe819c49ad46605979")))
    (file-name "hayro-checkout")
    (sha256
     (base32 "0sv8q11538nsmsqy11xwp12vwqand17bibnka1f23msafl95b6sx"))))

(define-public rnote
  (package
    (name "rnote")
    (version "0.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flxzt/rnote")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bhsrxar06kg463py0qr8pr4dpcaqbwax4pmkgbvv5fnb5ryiqms"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            ;; Replace git dependencies with a local path and keep the vendored
            ;; hayro workspace separate from rnote's own workspace.
            (substitute* "Cargo.toml"
              (("hayro = \\{ git.*\\}")
               "hayro = { path = \"hayro/hayro\" }")
              (("hayro-svg = \\{ git.*\\}")
               "hayro-svg = { path = \"hayro/hayro-svg\" }")
              (("^resolver = \"2\"")
               "exclude = [\"hayro\"]\nresolver = \"2\""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.92
       #:cargo-build-flags '("--release" "-p" "rnote")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-hayro
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (copy-recursively
              (assoc-ref (or native-inputs inputs) "hayro")
              "hayro")
             ;; Drop the test workspace member, which pulls an external git
             ;; dependency (sitro) that cannot be fetched offline.
             (substitute* "hayro/Cargo.toml"
               (("^[[:space:]]*\"hayro-tests\",?[[:space:]]*$") ""))))
         (add-after 'unpack-hayro 'generate-config
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Generate config.rs from template
               (substitute* "crates/rnote-ui/src/config.rs.in"
                 (("@APP_NAME@") "\"rnote\"")
                 (("@APP_NAME_CAPITALIZED@") "\"Rnote\"")
                 (("@APP_ID@") "\"com.github.flxzt.rnote\"")
                 (("@APP_IDPATH@") "\"/com/github/flxzt/rnote/\"")
                 (("@APP_VERSION@") "\"0.14.2\"")
                 (("@APP_VERSION_SUFFIX@") "\"\"")
                 (("@APP_AUTHOR_NAME@") "\"Felix Zwettler\"")
                 (("@APP_AUTHORS@") "\"Felix Zwettler\"")
                 (("@APP_WEBSITE@") "\"https://rnote.flxzt.net\"")
                 (("@APP_REPO_URL@") "\"https://github.com/flxzt/rnote\"")
                 (("@APP_ISSUES_URL@") "\"https://github.com/flxzt/rnote/issues\"")
                 (("@APP_SUPPORT_URL@")
                  "\"https://github.com/flxzt/rnote/discussions\"")
                 (("@APP_DONATE_URL@") "\"https://rnote.flxzt.net/donate/\"")
                 (("@PROFILE@") "\"default\"")
                 (("@GETTEXT_PACKAGE@") "\"rnote\"")
                 (("@LOCALEDIR@") (string-append "\"" out "/share/locale\""))
                 (("@DATADIR@") (string-append "\"" out "/share\""))
                 (("@LIBDIR@") (string-append "\"" out "/lib\"")))
               (copy-file "crates/rnote-ui/src/config.rs.in"
                          "crates/rnote-ui/src/config.rs")
               ;; Substitute meson template variables in data files
               (let ((app-id "com.github.flxzt.rnote"))
                 (substitute* "crates/rnote-ui/data/app.desktop.in.in"
                   (("@APP_ID@") app-id)
                   (("@APP_NAME@") "rnote")
                   (("@DESKTOP_FILE_NAME_ENTRY@") "Rnote"))
                 (substitute* "crates/rnote-ui/data/app.gschema.xml.in"
                   (("@APP_ID@") app-id)
                   (("@APP_IDPATH@") "/com/github/flxzt/rnote/"))
                 (substitute* "crates/rnote-ui/data/app.metainfo.xml.in.in"
                   (("@APP_ID@") app-id)
                   (("@APP_NAME_CAPITALIZED@") "Rnote"))
                 (substitute* "crates/rnote-ui/data/app.mimetype.xml.in"
                   (("@APP_NAME@") "rnote"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (apps (string-append share "/applications"))
                    (icons-scalable (string-append share
                                     "/icons/hicolor/scalable/apps"))
                    (icons-symbolic (string-append share
                                     "/icons/hicolor/symbolic/apps"))
                    (icons-mime (string-append share
                                 "/icons/hicolor/scalable/mimetypes"))
                    (schemas (string-append share "/glib-2.0/schemas"))
                    (metainfo (string-append share "/metainfo"))
                    (mime (string-append share "/mime/packages")))
               ;; Install binary
               (mkdir-p bin)
               (install-file "target/release/rnote" bin)
               ;; Desktop file
               (mkdir-p apps)
               (install-file "crates/rnote-ui/data/app.desktop.in.in"
                             apps)
               (rename-file (string-append apps "/app.desktop.in.in")
                            (string-append apps
                                           "/com.github.flxzt.rnote.desktop"))
               ;; Icons
               (mkdir-p icons-scalable)
               (copy-file "crates/rnote-ui/data/icons/scalable/apps/rnote.svg"
                          (string-append icons-scalable
                                         "/com.github.flxzt.rnote.svg"))
               (mkdir-p icons-symbolic)
               (copy-file
                "crates/rnote-ui/data/icons/symbolic/apps/rnote-symbolic.svg"
                (string-append icons-symbolic
                               "/com.github.flxzt.rnote-symbolic.svg"))
               (mkdir-p icons-mime)
               (copy-file
                "crates/rnote-ui/data/icons/scalable/mimetypes/application-rnote.svg"
                (string-append icons-mime "/application-rnote.svg"))
               ;; GSchema
               (mkdir-p schemas)
               (install-file "crates/rnote-ui/data/app.gschema.xml.in"
                             schemas)
               (rename-file
                (string-append schemas "/app.gschema.xml.in")
                (string-append schemas
                               "/com.github.flxzt.rnote.gschema.xml"))
               ;; Metainfo
               (mkdir-p metainfo)
               (install-file "crates/rnote-ui/data/app.metainfo.xml.in.in"
                             metainfo)
               (rename-file
                (string-append metainfo "/app.metainfo.xml.in.in")
                (string-append metainfo
                               "/com.github.flxzt.rnote.metainfo.xml"))
               ;; MIME type
               (mkdir-p mime)
               (install-file "crates/rnote-ui/data/app.mimetype.xml.in"
                             mime)
               (rename-file
                (string-append mime "/app.mimetype.xml.in")
                (string-append mime "/com.github.flxzt.rnote.xml")))))
         (add-after 'install 'compile-gschemas
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((schemas (string-append (assoc-ref outputs "out")
                                           "/share/glib-2.0/schemas")))
               (invoke "glib-compile-schemas" schemas))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Wrap for GSettings schemas
               (wrap-program (string-append out "/bin/rnote")
                 `("XDG_DATA_DIRS" ":" prefix
                   (,(string-append out "/share"))))))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("hayro" ,hayro-source)
       ("pkg-config" ,pkg-config)))
    (inputs
     (cons* alsa-lib
            cairo
            gettext-minimal
            gtk
            libadwaita
            poppler
            (px-cargo-inputs 'rnote)))
    (home-page "https://github.com/flxzt/rnote")
    (synopsis "Vector-based drawing app for handwritten notes and sketches")
    (description
     "Rnote is an open-source vector-based drawing application for sketching,
handwritten notes, and annotating documents and pictures.  It features an
adaptive UI powered by libadwaita, an infinite canvas, pressure-sensitive
input with pen/stylus support, various document export options (SVG, PDF, PNG,
JPEG), and import of PDF, bitmap images, and Xournal++ files.")
    (license license:gpl3+)))

(define-public curtail
  (package
    (name "curtail")
    (version "1.16.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Huluti/Curtail")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z1gxi4swynh77sm47j7zsnyj99xrwz7kk0x7ipif55qzpdxi1b7"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-update-checks
            (lambda _
              ;; Don't run gtk-update-icon-cache or update-desktop-database
              ;; at install time -- the build user can't write to the cache.
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")
                (("update_desktop_database: true")
                 "update_desktop_database: false"))))
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin/curtail"))
                    (compressors
                     (map (lambda (file)
                            (dirname (search-input-file inputs file)))
                          '("/bin/jpegoptim"
                            "/bin/pngquant"
                            "/bin/cwebp"
                            "/bin/scour"))))
                (wrap-program bin
                  `("GUIX_PYTHONPATH" ":" prefix
                    (,(getenv "GUIX_PYTHONPATH")))
                  `("GI_TYPELIB_PATH" ":" prefix
                    (,(getenv "GI_TYPELIB_PATH")))
                  `("PATH" ":" prefix ,compressors))))))))
    (native-inputs
     (list `(,glib "bin")
           blueprint-compiler
           desktop-file-utils
           gettext-minimal
           gobject-introspection
           `(,gtk "bin")
           pkg-config))
    (inputs
     (list bash-minimal
           gtk
           jpegoptim
           libadwaita
           libwebp
           pngquant
           python
           python-pygobject
           python-scour))
    (home-page "https://github.com/Huluti/Curtail")
    (synopsis "Image compression tool for GNOME")
    (description
     "Curtail is a useful image compressor for GNOME, supporting PNG, JPEG,
WebP and SVG file types.  Both lossy and lossless compression are available,
and metadata can be preserved.")
    (license license:gpl3+)))
