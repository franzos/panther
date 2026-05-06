;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wm))

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
;; protocol and Noctalia-specific build defaults.  Required by noctalia-shell.
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
