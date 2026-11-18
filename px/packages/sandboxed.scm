;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages sandboxed)
  #:use-module (gnu packages telegram)
  #:use-module (gnu packages video)
  #:use-module (nongnu packages messaging)
  #:use-module (px packages desktop-tools)
  #:use-module (px packages document)
  #:use-module (px packages graphics)
  #:use-module (px packages maths)
  #:use-module (px packages music)
  #:use-module (px sandbox))

(define-public element-desktop-sandboxed
  (sandboxed element-desktop
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/Element" "Downloads"))
              (network? #t)
              (wayland? #t)
              (gpu? #t)
              (audio? #t)
              (camera? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       %portal-screencast-dbus-rules
                       '(;; Chromium appends its PID, always 2 in the sandbox.
                         "--own=org.freedesktop.StatusNotifierItem-2-1"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.kde.StatusNotifierWatcher"))))))

(define-public mpv-sandboxed
  (sandboxed mpv
             (sandbox-policy
              (read-only-paths '(".config/mpv" ".cache/fontconfig"))
              (writable-paths '(".cache/mpv" ".local/state/mpv"))
              (file-arguments? #t)
              (wayland? #t)
              (gpu? #t)
              (audio? #t))))

(define-public papers-sandboxed
  (sandboxed papers
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (file-arguments? #t)
              (wayland? #t)
              (gpu? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public qalculate-gtk-sandboxed
  (sandboxed qalculate-gtk
             (sandbox-policy
              (read-only-paths '(".config/gtk-3.0/settings.ini"
                                 ".cache/fontconfig"))
              (writable-paths '(".config/qalculate" ".local/share/qalculate"))
              (network? #t)
              (wayland? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=io.github.Qalculate"))))))

(define-public signal-desktop-sandboxed
  (sandboxed signal-desktop
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/Signal" "Downloads"))
              (network? #t)
              (wayland? #t)
              (gpu? #t)
              (audio? #t)
              (camera? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       %portal-screencast-dbus-rules
                       '(;; Chromium appends its PID, always 2 in the sandbox.
                         "--own=org.freedesktop.StatusNotifierItem-2-1"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.kde.StatusNotifierWatcher"))))))

(define-public slack-desktop-sandboxed
  (sandboxed slack-desktop
             (sandbox-policy
              ;; Slack won't open its window without an os-release file.
              (read-only-paths '("/etc/os-release" ".cache/fontconfig"))
              (writable-paths '(".config/Slack" "Downloads"))
              (network? #t)
              (wayland? #t)
              (gpu? #t)
              (audio? #t)
              (camera? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       %portal-screencast-dbus-rules
                       '(;; Chromium appends its PID, always 2 in the sandbox.
                         "--own=org.freedesktop.StatusNotifierItem-2-1"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.kde.StatusNotifierWatcher"
                         ;; Sessions stay readable by the unsandboxed slack.
                         "--talk=org.freedesktop.secrets"))))))

(define-public spotifast-sandboxed
  (sandboxed spotifast
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/spotifast" ".local/state/spotifast"
                                ".cache/spotifast" "$XDG_RUNTIME_DIR/spotifast"))
              (network? #t)
              (wayland? #t)
              (gpu? #t)
              (audio? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=org.mpris.MediaPlayer2.spotifast"
                         ;; ksni appends its PID, always 2 in the sandbox.
                         "--own=org.kde.StatusNotifierItem-2-1"
                         "--talk=org.kde.StatusNotifierWatcher"
                         "--talk=org.freedesktop.secrets"))))))

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

(define-public telegram-desktop-sandboxed
  (sandboxed telegram-desktop
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".local/share/TelegramDesktop" "Downloads"))
              (network? #t)
              (wayland? #t)
              (gpu? #t)
              (audio? #t)
              (camera? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       %portal-screencast-dbus-rules
                       '("--own=org.telegram.desktop"
                         ;; Qt appends its PID, always 2 in the sandbox.
                         "--own=org.kde.StatusNotifierItem-2-1"
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
              (gpu? #t)
              (dbus-rules %portal-open-dbus-rules))))
