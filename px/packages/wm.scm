;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

;; Niri with SHM screencast support (PR #1791)
;; Fixes black screen in Chrome/WebRTC screen sharing
(define-public niri-shm
  (package
    (inherit niri)
    (name "niri-shm")
    (source
     (origin
       (inherit (package-source niri))
       (patches (list (local-file "patches/niri-shm-support.patch")))))
    (synopsis "Niri with SHM screencast support")
    (description
     "Niri scrollable-tiling Wayland compositor with shared memory (SHM)
fallback for PipeWire screencasting.  This fixes screen sharing in browsers
like Chrome that cannot handle DMA-BUF formats.")))

;; Noctalia's fork of Quickshell, adding the ext-background-effect-v1 Wayland
;; protocol and Noctalia-specific build defaults.  Required by noctalia-4.
(define-public noctalia-qs
  (let ((commit "d3e26ccd9eecde9139be00caf5dc2d4260fb31ee")
        (revision "0"))
    (package
      (inherit quickshell)
      (name "noctalia-qs")
      (version (git-version "0.0.12" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/noctalia-dev/noctalia-qs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "18s6mi934dbq837kfh5pda9az0sb9sm0zlzvidg440rq3wl3xdd4"))))
      (inputs
       (modify-inputs (package-inputs quickshell)
         (append glib polkit vulkan-headers)))
      (home-page "https://github.com/noctalia-dev/noctalia-qs")
      (synopsis "Quickshell fork tailored for Noctalia Shell")
      (description
       "noctalia-qs is a custom fork of Quickshell, the QtQuick-based desktop
shell toolkit, with patches and defaults specific to the Noctalia Shell
ecosystem.  It adds support for the @code{ext-background-effect-v1} Wayland
protocol.  The binary is named @code{quickshell} (with a @code{qs} alias) and
is a drop-in replacement for upstream Quickshell when running Noctalia."))))

(define-public noctalia-4
  (package
    (name "noctalia-4")
    (version "4.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/noctalia-dev/noctalia")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pchvsd89js4q7k10q621w3jqndv6raw6zy0px2b43ygh2kcpk22"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("Assets"  "share/noctalia-4/")
          ("Commons" "share/noctalia-4/")
          ("Helpers" "share/noctalia-4/")
          ("Modules" "share/noctalia-4/")
          ("Scripts" "share/noctalia-4/")
          ("Services" "share/noctalia-4/")
          ("Shaders" "share/noctalia-4/")
          ("Widgets" "share/noctalia-4/")
          ("shell.qml" "share/noctalia-4/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-launcher
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (share (string-append out "/share/noctalia-4"))
                     (launcher (string-append bin "/noctalia-4"))
                     (path (string-join
                            (map (lambda (pkg)
                                   (string-append (assoc-ref inputs pkg) "/bin"))
                                 '("brightnessctl"
                                   "cliphist"
                                   "ddcutil"
                                   "wlsunset"
                                   "wl-clipboard"
                                   "wlr-randr"
                                   "imagemagick"
                                   "wget"
                                   "python"
                                   "noctalia-qs"))
                            ":")))
                (mkdir-p bin)
                (call-with-output-file launcher
                  (lambda (port)
                    (format port "#!~a
export PATH=~a:$PATH
exec ~a/bin/quickshell -p ~a \"$@\"~%"
                            (string-append (assoc-ref inputs "bash")
                                           "/bin/bash")
                            path
                            (assoc-ref inputs "noctalia-qs")
                            share)))
                (chmod launcher #o755)))))))
    (inputs (list bash-minimal
                  brightnessctl
                  cliphist
                  ddcutil
                  imagemagick
                  noctalia-qs
                  python
                  wget
                  wl-clipboard
                  wlr-randr
                  wlsunset))
    (home-page "https://github.com/noctalia-dev/noctalia")
    (synopsis "Minimal desktop shell for Wayland built on Quickshell")
    (description
     "Noctalia is a minimal desktop shell for Wayland compositors, built on
Quickshell (Qt/QML).  It provides a status bar, panels, application launcher,
notifications, lock screen, idle management, OSD, theming, wallpaper
management, desktop widgets, dock, and multi-monitor support.  Noctalia
natively supports Niri, Hyprland, Sway, Scroll, Labwc and MangoWC.  This is
the v4 line, built on Quickshell; see @code{noctalia-5} for the from-scratch
C++ rewrite.")
    (license license:expat)))

;; Noctalia v5: a from-scratch C++ rewrite dropping Qt/Quickshell in favour of
;; Wayland + OpenGL ES.  A breaking rewrite, not a drop-in successor, so it's
;; packaged and tracked separately from noctalia-4 rather than replacing it.
(define-public noctalia-5
  (package
    (name "noctalia-5")
    (version "5.0.0-beta.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/noctalia-dev/noctalia")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xjkn0zrvg6hk9lp1sa2vf82d2g0d83j5lqkd1chy59zx22w4bdb"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:build-type "release"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-for-build
            (lambda _
              ;; /bin/sh doesn't exist in the build environment.
              (substitute* "tests/process_test.cpp"
                (("/bin/(sh)" _ cmd) (which cmd)))
              ;; Guix installs the STB headers without the "stb/" prefix.
              (substitute* (find-files "." "\\.cpp$|^meson\\.build$")
                (("\\bstb/stb_") "stb_")))))))
    (native-inputs (list pkg-config))
    (inputs
     (list cairo
           curl
           fontconfig
           freetype
           glib
           gmp
           harfbuzz
           jemalloc
           (librsvg-for-system)
           libical
           libjxl
           libqalculate
           libsecret
           libsndfile
           libsodium
           libwebp
           libxkbcommon
           libxml2
           linux-pam
           md4c
           mesa
           mpfr
           nlohmann-json
           pango
           pipewire
           polkit
           sdbus-c++
           stb-image-resize2
           stb-image-write
           tomlplusplus
           wayland
           wayland-protocols
           wireplumber))
    (home-page "https://noctalia.dev/")
    (synopsis "Sleek, customizable desktop shell crafted for Wayland")
    (description
     "Noctalia is a desktop shell for Wayland compositors, providing bars,
dock, application launcher, notifications, lock screen, wallpaper management
and settings.  This is the version 5 rewrite, built directly on Wayland and
OpenGL ES with no Qt or GTK toolkit dependency.")
    (license license:expat)))
