;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages terminals)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xorg))

(define-public ghostty
  (package
    (name "ghostty")
    (version "1.3.1-3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/dariogriffo/ghostty-debian/releases/download/"
             "1.3.1%2B3/ghostty_1.3.1-3%2Btrixie_amd64.deb"))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32 "04npnfl4xarsj0g24xnag3b56clqmyqjlbnvswsw4qhyns29ssy4"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     `(#:validate-runpath? #f
       #:patchelf-plan
       '(("usr/bin/ghostty"
          ("gtk" "glib" "libadwaita" "gtk4-layer-shell" "wayland" "libx11"
           "fontconfig" "freetype" "harfbuzz" "oniguruma" "gcc:lib"))
         ("usr/lib/libghostty-vt.so.0.1.0"
          ("gcc:lib")))
       #:install-plan
       '(("usr/bin" "bin")
         ("usr/lib" "lib")
         ("usr/include" "include")
         ("usr/share/applications" "share/applications")
         ("usr/share/bash-completion" "share/bash-completion")
         ("usr/share/fish" "share/fish")
         ("usr/share/zsh" "share/zsh")
         ("usr/share/icons" "share/icons")
         ("usr/share/locale" "share/locale")
         ("usr/share/man" "share/man")
         ("usr/share/metainfo" "share/metainfo")
         ("usr/share/pkgconfig" "share/pkgconfig")
         ("usr/share/terminfo" "share/terminfo")
         ("usr/share/nvim" "share/nvim")
         ("usr/share/vim" "share/vim")
         ("usr/share/bat" "share/bat")
         ("usr/share/kio" "share/kio")
         ("usr/share/dbus-1" "share/dbus-1")
         ("usr/share/systemd" "share/systemd")
         ("usr/share/nautilus-python" "share/nautilus-python")
         ("usr/share/ghostty" "share/ghostty"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (string-append
                             out "/share/applications/"
                             "com.mitchellh.ghostty.desktop")
                 (("/usr/bin/ghostty")
                  (string-append out "/bin/ghostty")))))))))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gcc:lib" ,gcc "lib")
       ("glib" ,glib)
       ("gtk" ,gtk)
       ("gtk4-layer-shell" ,gtk4-layer-shell)
       ("harfbuzz" ,harfbuzz)
       ("libadwaita" ,libadwaita)
       ("libx11" ,libx11)
       ("oniguruma" ,oniguruma)
       ("wayland" ,wayland)))
    (home-page "https://ghostty.org/")
    (synopsis "Fast, feature-rich, native terminal emulator")
    (description
     "Ghostty is a terminal emulator that differentiates itself by being fast,
feature-rich, and native.  It uses platform-native UI and GPU acceleration.
This package repackages the unofficial Debian binary build maintained at
@url{https://github.com/dariogriffo/ghostty-debian}.")
    (license license:expat)))
