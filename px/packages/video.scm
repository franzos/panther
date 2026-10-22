;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages video)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (px packages gstreamer)
  #:use-module (px self))

(define-public kooha
  (package
    (name "kooha")
    (version "2.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SeaDve/Kooha")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12z5fkm7ikkmh279j34akljcfmls09xd74175v9lyp397qx7mn8m"))))
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
               '(unpack-rust-crates
                 configure
                 check-for-pregenerated-files
                 patch-cargo-checksums))))
          (add-after 'glib-or-gtk-wrap 'wrap-gstreamer
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (gst-plugin-path
                     (string-join
                      (map (lambda (pkg)
                             (string-append (assoc-ref inputs pkg)
                                            "/lib/gstreamer-1.0"))
                           '("gstreamer" "gst-plugins-base"
                             "gst-plugins-good" "gst-plugins-bad"
                             "gst-plugins-ugly-full"))
                      ":")))
                (wrap-program (string-append out "/bin/kooha")
                  `("GST_PLUGIN_PATH" ":" prefix (,gst-plugin-path))))))))))
    (native-inputs
     (append
      (list gettext-minimal
            `(,glib "bin")
            pkg-config
            rust-1.88
            `(,rust-1.88 "cargo"))
      (or (and=> (%current-target-system)
                 (compose list make-rust-sysroot))
          '())))
    (inputs (cons* glib
                   gstreamer
                   gst-plugins-base
                   gst-plugins-good
                   gst-plugins-bad
                   gst-plugins-ugly-full
                   gtk
                   libadwaita
                   pipewire
                   (px-cargo-inputs 'kooha)))
    (home-page "https://github.com/SeaDve/Kooha")
    (synopsis "Screen recorder for GNOME")
    (description
     "Kooha is a minimalist screen recorder for GNOME.  It allows you to
capture your screen with a simple click, without having to configure
complicated settings.  It supports recording from microphones and desktop
audio simultaneously, and can save recordings in WebM, MP4, GIF, and
Matroska formats.")
    (license license:gpl3+)))

(define %sunshine-version "2026.1018.5342")

(define (sunshine-submodule name url commit hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url url)
          (commit commit)
          (recursive? #t)))
    (file-name (git-file-name (string-append "sunshine-" name) %sunshine-version))
    (sha256 (base32 hash))))

;; Only `package-lock.cmake' out of this one is used, so skip its submodules --
;; they carry a full FFmpeg source tree.
(define sunshine-build-deps
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/LizardByte/build-deps")
          (commit "d0ac5ef4b7673a8d8d2b03981ab5226cbf25810c")))
    (file-name (git-file-name "sunshine-build-deps" %sunshine-version))
    (sha256
     (base32 "1a0aw1c9bq8w999dw4mgi964bwxggxrp773niwnn5b1mvhjfvypn"))))

(define sunshine-simple-web-server
  (sunshine-submodule "Simple-Web-Server"
                      "https://github.com/LizardByte-infrastructure/Simple-Web-Server"
                      "546895a93a29062bb178367b46c7afb72da9881e"
                      "1hbwlbw1ll3cd56br90q9af742r6457r822wnf7yiwaam999k2xh"))

(define sunshine-glad
  (sunshine-submodule "glad"
                      "https://github.com/Dav1dde/glad"
                      "73db193f853e2ee079bf3ca8a64aa2eaf6459043"
                      "0c9cygiq35aiq6bpdvbwqs0wxc2dvxsh4jnx50466savscxalsk9"))

(define sunshine-libdisplaydevice
  (sunshine-submodule "libdisplaydevice"
                      "https://github.com/LizardByte/libdisplaydevice"
                      "a2afc0430d44223c25a2ebc598000f78b338aa52"
                      "01d817g76chyilnbi12rzdfa64z2zzi5sxab0gx8wv54fjfcs5v0"))

(define sunshine-libvirtualhid
  (sunshine-submodule "libvirtualhid"
                      "https://github.com/LizardByte/libvirtualhid"
                      "dfff3ea8c34022c915799740806c17c0ed5c2492"
                      "13a7sdvyqy88227m4cl027kg5nn1pzliqbkkkxc6kw3scwrqvqcj"))

(define sunshine-lizardbyte-common
  (sunshine-submodule "lizardbyte-common"
                      "https://github.com/LizardByte/lizardbyte-common"
                      "f9d91e1d29b7473f58e43acde4579da4e56c4abe"
                      "0f3xjnmnb3537v4dkdnykm6dzcl3kdvdmzvyvw56giaqzdjvbqwj"))

(define sunshine-moonlight-common-c
  (sunshine-submodule "moonlight-common-c"
                      "https://github.com/moonlight-stream/moonlight-common-c"
                      "aeba988e0a302145eff2e70fa3e11cd98ec01f5d"
                      "1v31whwqyb33f7yyx7c1fgnjj148cxpxvbhw43vhqw594d305cm8"))

(define sunshine-plasma-wayland-protocols
  (sunshine-submodule "plasma-wayland-protocols"
                      "https://github.com/KDE/plasma-wayland-protocols"
                      "c5ac4db818f4a575a6ccfe0065b73ecfaba6e93e"
                      "0g456mwvva4algzjwal4rrkld51pckxbci3rjd94cd5qxp9ssjnq"))

(define sunshine-tray
  (sunshine-submodule "tray"
                      "https://github.com/LizardByte/tray"
                      "c1f2a8ab56dd399b881fb7aeab4fe4fc9c58188d"
                      "1fxg7d728bkavsnfx4bh9bac81jfjzyrv6affm5mvi4mzwblf6hq"))

(define sunshine-wayland-protocols
  (sunshine-submodule "wayland-protocols"
                      "https://github.com/LizardByte-infrastructure/wayland-protocols"
                      "ee78491a237eaff9389a0ccf8680521d074407d3"
                      "1810ymkhgxhrslmllz4zkadi71mjpa7srw4wm1kq22vqp08rp04i"))

(define sunshine-wlr-protocols
  (sunshine-submodule "wlr-protocols"
                      "https://github.com/LizardByte-infrastructure/wlr-protocols"
                      "bf4fc79abc359eea5a0edec0ac6d4a2b2955f82a"
                      "1wr3d6m2ykjpz4cq5zzmm6hfaxrysxf3rki3hhkzd025lsdd2vfl"))

(define sunshine-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/LizardByte/Sunshine")
          (commit (string-append "v" %sunshine-version))))
    (file-name (git-file-name "sunshine" %sunshine-version))
    (sha256
     (base32 "1sc9ch1r67igjlp1khs3vhnsg095hfqsklrvgfs7abmy3w9hi7k9"))))

;; Sunshine calls FFmpeg's private codec bitstream API (ff_cbs_*), so it cannot
;; link against a normal shared FFmpeg.  Upstream downloads a purpose-built
;; static FFmpeg at configure time; take the same archive here and hand it to
;; cmake via FFMPEG_PREPARED_BINARIES.  The tag must match the commit that
;; third-party/build-deps is pinned to in the Sunshine release.
(define sunshine-ffmpeg
  (package
    (name "sunshine-ffmpeg")
    (version "2026.1017.131201")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/LizardByte/build-deps/releases/download/v"
                           version "/Linux-x86_64-ffmpeg.tar.gz"))
       (sha256
        (base32 "05c83x8kkmmdd1q5ad4xqyh9fgr1kclpbjwa0fv9rf0arccm3pmj"))))
    (build-system copy-build-system)
    (arguments
     (list #:strip-binaries? #f
           #:validate-runpath? #f
           #:install-plan #~'(("include" "include")
                              ("lib" "lib"))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/LizardByte/build-deps")
    (synopsis "Static FFmpeg build used by Sunshine")
    (description
     "Pre-built static FFmpeg libraries published by the LizardByte project,
including the @code{libcbs} archive that exposes FFmpeg's internal codec
bitstream API.  Sunshine links against these.")
    (license (list license:lgpl2.1+ license:gpl2+))))

;; The web UI is built by npm/vite, which resolves its dependency tree over the
;; network.  Do that in a fixed-output derivation, then hand the result to the
;; cmake build; upstream's own `npm ci' target is disabled below.
(define sunshine-web-ui
  (computed-file
   "sunshine-web-ui"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (setenv "PATH" (string-append #$node "/bin:" (getenv "PATH")))
         (setenv "HOME" "/tmp")
         (setenv "SSL_CERT_DIR" (string-append #$nss-certs "/etc/ssl/certs"))
         (setenv "SSL_CERT_FILE"
                 (string-append #$nss-certs "/etc/ssl/certs/ca-certificates.crt"))
         (copy-recursively #$sunshine-source "/tmp/sunshine")
         (for-each make-file-writable
                   (find-files "/tmp/sunshine" #:directories? #t))
         (with-directory-excursion "/tmp/sunshine"
           ;; The Codecov plugin uploads bundle statistics to a third party
           ;; during the build.
           (substitute* "vite.config.js"
             (("^import \\{ codecovVitePlugin \\}.*") "")
             (("codecovVitePlugin\\(\\{" all)
              "((() => null))({"))
           (invoke "npm" "ci" "--ignore-scripts")
           ;; Run vite directly; `npm run build' would need a shell.
           (invoke "node" "node_modules/vite/bin/vite.js" "build"))
         (copy-recursively "/tmp/sunshine/build" #$output)))
   #:options `(#:hash-algo sha256
               #:hash ,(base32 "1sshr0l269m127sskbpqp1f9v2fmi45z67bs65ikgmzr2i80fa7n")
               #:recursive? #t)))

(define-public sunshine
  (package
    (name "sunshine")
    (version %sunshine-version)
    (source sunshine-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-Wno-dev"
              "-DBUILD_DOCS=OFF"
              "-DBUILD_TESTS=OFF"
              "-DBOOST_USE_STATIC=OFF"
              "-DSUNSHINE_ENABLE_CUDA=OFF"
              "-DSUNSHINE_SYSTEM_VULKAN_HEADERS=ON"
              ;; Jinja2 and setuptools come from native-inputs, so glad must
              ;; not try to pip-install them.
              "-DGLAD_SKIP_PIP_INSTALL=ON"
              (string-append "-DFFMPEG_PREPARED_BINARIES=" #$sunshine-ffmpeg)
              "-DSUNSHINE_PUBLISHER_NAME=panther"
              "-DSUNSHINE_PUBLISHER_WEBSITE=https://git.gofranz.com/franz/panther"
              "-DSUNSHINE_PUBLISHER_ISSUE_URL=https://git.gofranz.com/franz/panther/issues"
              ;; Guix System has no systemd; ship only the udev rules.
              "-DUDEV_FOUND=ON"
              "-DUDEV_RULES_INSTALL_DIR=lib/udev/rules.d"
              "-DSYSTEMD_FOUND=OFF"
              (string-append "-DSUNSHINE_EXECUTABLE_PATH=" #$output "/bin/sunshine"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-submodules
            (lambda _
              (define (install-submodule name source)
                (let ((target (string-append "third-party/" name)))
                  (mkdir-p target)
                  (copy-recursively source target)
                  (for-each make-file-writable
                            (find-files target #:directories? #t))))
              (install-submodule "build-deps" #$sunshine-build-deps)
              (install-submodule "Simple-Web-Server" #$sunshine-simple-web-server)
              (install-submodule "glad" #$sunshine-glad)
              (install-submodule "libdisplaydevice" #$sunshine-libdisplaydevice)
              (install-submodule "libvirtualhid" #$sunshine-libvirtualhid)
              (install-submodule "lizardbyte-common" #$sunshine-lizardbyte-common)
              (install-submodule "moonlight-common-c" #$sunshine-moonlight-common-c)
              (install-submodule "plasma-wayland-protocols"
                                 #$sunshine-plasma-wayland-protocols)
              (install-submodule "tray" #$sunshine-tray)
              (install-submodule "wayland-protocols" #$sunshine-wayland-protocols)
              (install-submodule "wlr-protocols" #$sunshine-wlr-protocols)))
          (add-after 'unpack 'patch-cmake
            (lambda _
              ;; The web UI is supplied by `sunshine-web-ui'; drop upstream's
              ;; npm target instead of letting it reach for the network.
              (substitute* "cmake/targets/common.cmake"
                (("find_program\\(NPM npm REQUIRED\\)") "")
                (("add_custom_target\\(web-ui ALL") "add_custom_target(web-ui"))
              ;; Both probe for FHS install directories; they are passed in as
              ;; cache variables instead.
              (substitute* "cmake/packaging/linux.cmake"
                (("find_package\\(Systemd\\)") "")
                (("find_package\\(Udev\\)") ""))
              ;; Without systemd there is no unit to start.
              (substitute* "packaging/linux/dev.lizardbyte.app.Sunshine.desktop"
                (("/usr/bin/env systemctl start --u app-@PROJECT_FQDN@")
                 "sunshine"))))
          (add-after 'unpack 'set-build-version
            (lambda _
              ;; Without these the version is derived from git, which is not
              ;; available here.
              (setenv "BRANCH" "master")
              (setenv "BUILD_VERSION" #$%sunshine-version)))
          (add-after 'configure 'install-web-ui
            (lambda _
              ;; cmake installs the vite output from the build directory.
              (copy-recursively #$sunshine-web-ui (getcwd))
              (for-each make-file-writable
                        (find-files "assets" #:directories? #t)))))))
    (native-inputs
     (list pkg-config
           python-jinja2
           python-setuptools
           python-wrapper
           shaderc
           vulkan-headers
           wayland))
    (inputs
     (list avahi
           boost
           curl
           glib
           libcap
           libdrm
           libevdev
           libnotify
           libva
           libvdpau
           libx11
           libxcb
           libxfixes
           libxi
           libxrandr
           libxtst
           mesa
           miniupnpc
           nlohmann-json
           numactl
           openssl
           opus
           pipewire
           pulseaudio
           qtbase
           qtsvg
           vulkan-loader
           wayland))
    (supported-systems '("x86_64-linux"))
    (home-page "https://app.lizardbyte.dev/Sunshine")
    (synopsis "Self-hosted game stream host for Moonlight")
    (description
     "Sunshine is a self-hosted game stream host for Moonlight clients.  It
offers low latency cloud gaming server capabilities with support for AMD, Intel
and Nvidia GPUs for hardware encoding; software encoding is also available.  A
web interface is provided for configuration and client pairing.")
    (license license:gpl3)))
