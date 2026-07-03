;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages package-management)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (px packages setup)
  #:use-module (px self))

(define-public guix-tools
  (package
    (name "guix-tools")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/guix-tools_v" version
                           ".tgz"))
       (sha256
        (base32 "0b6z2fx5prkibdqsc2n13v322jcl9h29h9g0r67556h21x8f84n8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("guile-json" ,guile-json-1)
              ("guile" ,guile-3.0)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX guix tools to automate guix related tasks")
    (description "Automate `guix` package manager tasks using scheme scripts.
this tool is developed for PantherX team internal usage.")
    (license license:expat)))

(define-public guix-gui
  (package
    (name "guix-gui")
    (version "0.1.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/guix-rs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zxgd9nimifq9w85q5840d16an670hwpsfqp6685y155gfkdbc4d"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      ;; libguix's live-tests require a working `guix` binary on the host
      ;; and the iced GUI has no useful headless test coverage.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; The cargo-build-system's default 'install' runs `cargo install'
          ;; from the workspace root, which is a no-op here (no root crate).
          ;; Copy the binary out of the workspace target dir instead.
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (install-file "target/release/guix-gui" bin))))
          (add-after 'install 'install-resources
            (lambda _
              (let* ((apps  (string-append #$output "/share/applications"))
                     (icons (string-append #$output
                                           "/share/icons/hicolor/scalable/apps")))
                (mkdir-p apps)
                (copy-file "assets/guix-gui.desktop"
                           (string-append apps "/guix-gui.desktop"))
                (mkdir-p icons)
                (copy-file "assets/icon.svg"
                           (string-append icons "/guix-gui.svg")))))
          (add-after 'install-resources 'wrap-binary
            (lambda _
              ;; iced/winit dlopens Vulkan, Wayland, X11 and xkbcommon at
              ;; runtime; rpath alone won't find them.
              (wrap-program (string-append #$output "/bin/guix-gui")
                `("LD_LIBRARY_PATH" ":" prefix
                  (#$(file-append vulkan-loader "/lib")
                   #$(file-append wayland "/lib")
                   #$(file-append libxkbcommon "/lib")
                   #$(file-append libx11 "/lib")
                   #$(file-append libxcursor "/lib")
                   #$(file-append libxi "/lib")
                   #$(file-append libxrandr "/lib")
                   #$(file-append fontconfig "/lib")
                   #$(file-append freetype "/lib")))))))))
    (native-inputs (list pkg-config))
    (inputs
     (cons* bash-minimal
            fontconfig
            freetype
            libx11
            libxcursor
            libxi
            libxkbcommon
            libxrandr
            vulkan-loader
            wayland
            (px-cargo-inputs 'guix-gui)))
    (home-page "https://github.com/franzos/guix-rs")
    (synopsis "Iced GUI for day-to-day Guix package management")
    (description
     "@code{guix-gui} is an unofficial desktop frontend for GNU Guix.
It lets you search for packages, manage profiles, view generations,
and (combined with the polkit actions from @code{libguix-polkit}) run
@command{guix pull} and @command{guix system reconfigure} via
@command{pkexec}.")
    (license license:gpl3+)))

(define-public guix-install-gui
  (package
    (inherit guix-install)
    (name "guix-install-gui")
    (arguments
     (list
      #:install-source? #f
      #:tests? #f
      ;; iced GUI crate of the guix-install workspace; tiny-skia renderer.
      #:cargo-build-flags ''("--release" "-p" "guix-install-gui")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (install-file "target/release/guix-install-gui" bin))))
          (add-after 'install 'wrap-binary
            (lambda _
              ;; iced/winit dlopens Wayland, X11, xkbcommon and GL at runtime;
              ;; rpath alone won't find them.
              (wrap-program (string-append #$output "/bin/guix-install-gui")
                `("LD_LIBRARY_PATH" ":" prefix
                  (#$(file-append wayland "/lib")
                   #$(file-append libxkbcommon "/lib")
                   #$(file-append libx11 "/lib")
                   #$(file-append libxcursor "/lib")
                   #$(file-append libxi "/lib")
                   #$(file-append libxrandr "/lib")
                   #$(file-append mesa "/lib")
                   #$(file-append libglvnd "/lib")
                   #$(file-append fontconfig "/lib")
                   #$(file-append freetype "/lib")))))))))
    (native-inputs (list pkg-config))
    (inputs
     (cons* bash-minimal
            expat
            fontconfig
            freetype
            libglvnd
            libx11
            libxcursor
            libxi
            libxkbcommon
            libxrandr
            mesa
            wayland
            (px-cargo-inputs 'guix-install)))
    (synopsis "Graphical (iced) frontend for the Guix System installer")
    (description
     "@code{guix-install-gui} is the iced graphical frontend for
@code{guix-install}.  It drives the same installation logic as the CLI
through a fullscreen interface, with a built-in @file{system.scm} editor
and live progress for @command{guix pull} and @command{guix system init}.")))