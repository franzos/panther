;;; Package Repository for GNU Guix
;;; Copyright © 2021-2024 Hamzeh Nasajpour <h.nasajpour@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages themes)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system font)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xml)
  #:use-module (px packages kde-plasma))

(define-public px-widget-style
  (package
    (name "px-widget-style")
    (version "5.19.6-1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0m2bkgsijghxj526zwl2zppqnw7q03zmfs8vvpbiggnxl2n77hf6"))))
    (properties `((tags quote
                        ("Desktop" "KDE" "Plasma"))))
    (build-system cmake-build-system)
    (native-inputs `(("extra-cmake-modules" ,extra-cmake-modules)
                     ("pkg-config" ,pkg-config)))
    ;; TODO: Warning at /gnu/store/…-kpackage-5.34.0/…/KF5PackageMacros.cmake:
    ;; warnings during generation of metainfo for org.kde.breezedark.desktop:
    ;; Package type "Plasma/LookAndFeel" not found
    ;; TODO: Unknown property type for key "X-KDE-ParentApp",
    ;; "X-Plasma-MainScript"
    (inputs `(("kcmutils" ,kcmutils-5)
              ("kconfigwidgets" ,kconfigwidgets-5)
              ("kcoreaddons" ,kcoreaddons-5)
              ("kde-frameworkintegration" ,kde-frameworkintegration-5)
              ("kdecoration" ,kdecoration-5)
              ("kguiaddons" ,kguiaddons-5)
              ("ki18n" ,ki18n-5)
              ("kiconthemes" ,kiconthemes-5)
              ("kpackage" ,kpackage-5)
              ("kwayland" ,kwayland-5)
              ("kwindowsystem" ,kwindowsystem-5)
              ("plasma-framework" ,plasma-framework) ;missing in CMakeList.txt
              ;; ("qtbase" ,qtbase)
              ("qtdeclarative" ,qtdeclarative-5)
              ("qtx11extras" ,qtx11extras)))
    (propagated-inputs `(("qtbase" ,qtbase-5)))
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'fix-source
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "CMakeLists.txt"
                          (("include\\(KDEClangFormat\\)")
                           "#include(KDEClangFormat)"))
                        (substitute* "CMakeLists.txt"
                          (("kde_clang_format")
                           "#kde_clang_format"))
                        ;; Fixing the path of kde4breeze to pointing to the store of breeze theme
                        (substitute* "colors-px/breeze-default-colorscheme.desktop"
                          (("kde4breeze")
                           (string-append out
                                          "/lib/kconf_update_bin/kde4breeze")))
                        #t))))))
    (home-page "https://kde.org/plasma-desktop")
    (synopsis "Default Plasma theme (meta-package)")
    (description
     "Forked version of breeze style theme with additional color-schemes for PantherX.")
    (license license:gpl2+)))

(define-public lxqt-arc-dark-theme
  (package
    (name "lxqt-arc-dark-theme")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.pantherx.org/franz/lxqt-arc-dark-theme")
             (commit "928cddad613a4b28f5453e1c2414b6ce0438d9c8")))
       (sha256
        (base32 "0qi5vy2hjkkyydycmpilbig6rwf870n8yjy2knz9jm0y6a2bx07w"))
       (file-name (git-file-name name version))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (theme-dir (string-append %output
                                                    "/share/lxqt/themes/")))
                     (mkdir-p (string-append theme-dir "arc-dark"))
                     (copy-recursively (string-append source "/arc-dark")
                                       (string-append theme-dir "arc-dark"))))))
    (synopsis "LXQt Arc Dark Theme")
    (home-page "https://git.pantherx.org/franz/lxqt-arc-dark-theme")
    (description "LXQt Theme based on Arc by horst3180 and LXQt dark theme")
    (license license:gpl3+)))

(define-public sddm-darkine-theme
  (package
    (name "sddm-darkine-theme")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Rokin05/darkine-kde.git")
             (commit "cd37f809b3f3ca7fd8865e6e91887a26b591c8d3")))
       (sha256
        (base32 "0wfq5ydlmf92y7xj8qad2rjvvicy1f97cd7n3j5i3pmq9pl74zys"))
       (file-name (git-file-name name version))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (theme-dir (string-append %output
                                      "/share/sddm/themes/darkine")))
                     (mkdir-p theme-dir)
                     (copy-recursively (string-append source
                                                      "/sddm/themes/darkine")
                                       theme-dir)))))
    (home-page "https://github.com/Rokin05/darkine-kde")
    (synopsis "SDDM theme from Darkine KDE collection")
    (description
     "SDDM theme from Darkine KDE collection, a pure QtQuick2 based SDDM login theme")
    (license license:expat)))

(define-public chilie-login-theme
  (package
    (name "chilie-login-theme")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/MarianArlt/sddm-chili/archive/"
             version ".tar.gz"))
       (sha256
        (base32 "03wswhqp9980blsrhk60jj49gbqmpkij85i71bzqyi0gyxxn1h53"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar (assoc-ref %build-inputs "tar"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (theme-dir (string-append %output
                                                    "/share/sddm/themes")))
                     (mkdir-p theme-dir)
                     (setenv "PATH"
                             (string-append gzip "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" source "-C"
                             theme-dir)))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)))
    (home-page "https://github.com/MarianArlt/sddm-chili")
    (synopsis "Chili login theme for SDDM")
    (description "Chili login theme for SDDM.")
    (license license:gpl3)))

(define-public paper-icon-theme
  (package
    (name "paper-icon-theme")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/snwh/paper-icon-theme/archive/v." version
             ".tar.gz"))
       (sha256
        (base32 "1klf545hk6g7dx9g0bkblrzd46kz6hr0yj1mqvjq3r7cjpz1vwk2"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'disable-post-install
                    (lambda _
                      (substitute* "meson.build"
                        (("meson.add_install_script.*")
                         "")))))))
    (synopsis "Paper icon theme")
    (home-page "https://snwh.org")
    (description "Paper is an open source FreeDesktop icon project")
    (license license:expat)))


(define-public font-openmoji
  (package
    (inherit (@ (gnu packages fonts) font-openmoji))
    (version "17.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/hfg-gmuend/openmoji/releases"
                           "/download/" version "/openmoji-font.zip"))
       (file-name (string-append "openmoji-font-" version ".zip"))
       (sha256
        (base32 "1fmd7iplxy1msqdvdr3csgm5gngmcmx0biqkvnxvxl1qrf6ivrhx"))))
    (arguments
     (list
      #:phases
      ;; The zip holds one directory per variant; keep only the two Guix ships.
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "unzip" "-q" source
                      "OpenMoji-black-glyf/*.ttf"
                      "OpenMoji-color-glyf_colr_0/*.ttf"))))))
    (native-inputs (list unzip))))
