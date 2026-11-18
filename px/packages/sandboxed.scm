;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages sandboxed)
  #:use-module (px packages document)
  #:use-module (px packages graphics)
  #:use-module (px packages music)
  #:use-module (px sandbox))

(define-public papers-sandboxed
  (sandboxed papers
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (file-arguments? #t)
              (wayland? #t)
              (gpu? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public strawberry-sandboxed
  (sandboxed strawberry
             (sandbox-policy
              (read-only-paths '("Music" ".cache/fontconfig"))
              (writable-paths '(".config/strawberry" ".local/share/strawberry"
                                ".cache/strawberry"))
              (file-arguments? #t)
              (network? #t)
              (wayland? #t)
              (gpu? #t)
              (audio? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=org.mpris.MediaPlayer2.strawberry"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.kde.StatusNotifierWatcher"))))))

(define-public tidal-hifi-sandboxed
  (sandboxed tidal-hifi
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/tidal-hifi"))
              (network? #t)
              (wayland? #t)
              (gpu? #t)
              (audio? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=org.mpris.MediaPlayer2.tidal-hifi"
                         ;; Chromium appends its PID, always 2 in the sandbox.
                         "--own=org.freedesktop.StatusNotifierItem-2-1"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.kde.StatusNotifierWatcher"))))))

(define-public vitrine-sandboxed
  (sandboxed vitrine
             (sandbox-policy
              (read-only-paths '(".config/vitrine" "Pictures"))
              (writable-paths '(".local/share/vitrine" ".cache/vitrine"))
              (wayland? #t)
              (gpu? #t))))
