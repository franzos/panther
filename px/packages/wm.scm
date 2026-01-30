;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages wm)
  #:use-module (guix packages)
  #:use-module (guix gexp)
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
