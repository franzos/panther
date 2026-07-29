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

(define %sunshine-version "2026.516.143833")

(define (sunshine-submodule name url commit hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url url)
          (commit commit)
          (recursive? #t)))
    (file-name (git-file-name (string-append "sunshine-" name) %sunshine-version))
    (sha256 (base32 hash))))

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

(define sunshine-inputtino
  (sunshine-submodule "inputtino"
                      "https://github.com/games-on-whales/inputtino"
                      "f4ce2b0df536ef309e9ff318f75b460f7097d7c1"
                      "04y9m7g9jx0cp3yn8rlf7syhrsvrb1znbknyr25x8s5vh9n1f04q"))

(define sunshine-libdisplaydevice
  (sunshine-submodule "libdisplaydevice"
                      "https://github.com/LizardByte/libdisplaydevice"
                      "fe7e6a81f65deae91594702e1a185f47229745b9"
                      "1jgn82f4zfbkk24nb58ynrjj6x1f7kl7yfkdsybc9p4sjj8hhjd5"))

(define sunshine-moonlight-common-c
  (sunshine-submodule "moonlight-common-c"
                      "https://github.com/moonlight-stream/moonlight-common-c"
                      "2600beaf13f18bfa43453609cf5e3b84a4227760"
                      "1jxi6gfsxli9apijjbylfi5mlfqjy5lzvlyxjdmfdm1l6zypl0a7"))

(define sunshine-nanors
  (sunshine-submodule "nanors"
                      "https://github.com/sleepybishop/nanors"
                      "19f07b513e924e471cadd141943c1ec4adc8d0e0"
                      "05y8jkj6x9kwvzx5rvkdqzmm9ip0kbyx6llrfh7m762rj9dh74cn"))

(define sunshine-nv-codec-headers
  (sunshine-submodule "nv-codec-headers"
                      "https://github.com/FFmpeg/nv-codec-headers"
                      "33a9ede8d9914299d9262539c576a15bd0a19621"
                      "1lpn87975hr6wndlig6002mm1axwnf8l5szdndp4z5jnrws8bn65"))

(define sunshine-plasma-wayland-protocols
  (sunshine-submodule "plasma-wayland-protocols"
                      "https://github.com/KDE/plasma-wayland-protocols"
                      "4c015e90ae6c88f2ffa766e899387ef431eade49"
                      "1q012lr2pys6slf9aa6mrxwkp081qg4r4bqjbjzjishwmayn0y8a"))

(define sunshine-tray
  (sunshine-submodule "tray"
                      "https://github.com/LizardByte/tray"
                      "563dee475f8878d252ab2b9938d3a014e776ed08"
                      "0ig973xrvv62n5chqmr428bn6hbnmqqq2418cnhy7is0k9yhqcan"))

(define sunshine-wayland-protocols
  (sunshine-submodule "wayland-protocols"
                      "https://github.com/LizardByte-infrastructure/wayland-protocols"
                      "88223018d1b578d0d8869866da66d9608e05f928"
                      "0i30fp6p03qrvv4msm9yj39sk6gmcryp3q6x29v2wycbv27wsgmw"))

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
     (base32 "0diq6gj6dfs0v32dg394pmb9cvgvg2b567dabsf475kyajjs05mr"))))

;; Sunshine calls FFmpeg's private codec bitstream API (ff_cbs_*), so it cannot
;; link against a normal shared FFmpeg.  Upstream downloads a purpose-built
;; static FFmpeg at configure time; take the same archive here and hand it to
;; cmake via FFMPEG_PREPARED_BINARIES.  The tag must match the commit that
;; third-party/build-deps is pinned to in the Sunshine release.
(define sunshine-ffmpeg
  (package
    (name "sunshine-ffmpeg")
    (version "2026.516.30821")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/LizardByte/build-deps/releases/download/v"
                           version "/Linux-x86_64-ffmpeg.tar.gz"))
       (sha256
        (base32 "1v667cf34scrkh1gkzhvkhx8qdsq796npqiszzlfyyw6qby1j8y3"))))
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
               #:hash ,(base32 "16fm9h53jyy6ji41jm62jril68j8skmxfi6fnfk6yzzvhy887z02")
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
              (install-submodule "Simple-Web-Server" #$sunshine-simple-web-server)
              (install-submodule "glad" #$sunshine-glad)
              (install-submodule "inputtino" #$sunshine-inputtino)
              (install-submodule "libdisplaydevice" #$sunshine-libdisplaydevice)
              (install-submodule "moonlight-common-c" #$sunshine-moonlight-common-c)
              (install-submodule "nanors" #$sunshine-nanors)
              (install-submodule "nv-codec-headers" #$sunshine-nv-codec-headers)
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
           libappindicator
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
