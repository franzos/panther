;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages vm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (px self))

(define-public ironbar
  (package
    (name "ironbar")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JakeStanger/ironbar/archive/refs/tags/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "169nr9wv5f0jpfh2krkgrr3bj10xlxvbh4mf3109p0rgqzqc498d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.88))
    (native-inputs
     (list pkg-config))
    (inputs
     (cons* dbus
            eudev
            gtk
            gtk4-layer-shell
            libevdev
            libinput
            luajit
            openssl
            pulseaudio
            (px-cargo-inputs 'ironbar)))
    (home-page "https://github.com/JakeStanger/ironbar")
    (synopsis "Customizable GTK4 bar for Wayland compositors")
    (description
     "Ironbar is a customizable and feature-rich GTK4 bar for Wayland
compositors.  It supports CSS theming, popups, and integrations with
native desktop libraries.  Features include workspaces, tray icons,
clock, volume control, network status, and custom widgets via Lua
scripting.")
    (license license:expat)))
