;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages graphics)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
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
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
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
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/CyberTimon/RapidRAW/releases/download/v"
             version "/03_RapidRAW_v" version "_ubuntu-24.04_amd64.deb"))
       (sha256
        (base32 "0ckc9q4g8mvrdjyzx79f0c36y2f9rv7v274sfkdc27jaab8kr0wp"))))
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
    (version "4.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/libvips/vipsdisp/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "1hvnb7hjjh35gqsd5hha9v4fqmfp9fs7dsadkylsdbwk08jpqsv9"))))
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
