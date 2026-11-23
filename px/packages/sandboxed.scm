;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages sandboxed)
  #:use-module ((gnu packages audio) #:select (audacity))
  #:use-module (gnu packages bittorrent)
  #:use-module ((gnu packages chromium) #:select (ungoogled-chromium))
  #:use-module (gnu packages ebook)
  #:use-module (gnu packages fediverse)
  #:use-module (gnu packages gimp)
  #:use-module ((gnu packages gnome) #:select (fractal loupe rhythmbox))
  #:use-module (gnu packages gnome-circle)
  #:use-module (gnu packages inkscape)
  #:use-module ((gnu packages kde-graphics) #:select (okular))
  #:use-module ((gnu packages libreoffice) #:select (libreoffice))
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages telegram)
  #:use-module (gnu packages video)
  #:use-module ((gnu packages web) #:select (kiwix-desktop))
  #:use-module ((nongnu packages chrome) #:select (google-chrome-stable))
  #:use-module (nongnu packages messaging)
  #:use-module ((nongnu packages mozilla) #:select (firefox))
  #:use-module (nongnu packages password-utils)
  #:use-module (nongnu packages productivity)
  #:use-module (px packages desktop-tools)
  #:use-module (px packages document)
  #:use-module (px packages gnuzilla)
  #:use-module (px packages graphics)
  #:use-module (px packages maths)
  #:use-module (px packages music)
  #:use-module (px sandbox))

(define-public amberol-sandboxed
  (sandboxed amberol
             (sandbox-policy
              (read-only-paths '("Music" ".cache/fontconfig"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=io.bassi.Amberol"
                         "--own=org.mpris.MediaPlayer2.io.bassi.Amberol"))))))

(define-public anytype-sandboxed
  (sandboxed anytype
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/anytype" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '(;; Chromium appends its PID, always 2 in the sandbox.
                         "--own=org.freedesktop.StatusNotifierItem-2-1"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.kde.StatusNotifierWatcher"
                         "--talk=org.freedesktop.secrets"))))))

(define-public audacity-sandboxed
  (sandboxed audacity
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/audacity" ".local/share/audacity"
                                ".local/state/audacity" "Music"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public bitwarden-desktop-sandboxed
  (sandboxed bitwarden-desktop
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/Bitwarden" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '(;; Chromium appends its PID, always 2 in the sandbox.
                         "--own=org.freedesktop.StatusNotifierItem-2-1"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.kde.StatusNotifierWatcher"
                         "--talk=org.freedesktop.secrets"))))))

(define-public calibre-sandboxed
  (sandboxed calibre
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/calibre" ".cache/calibre"
                                "Calibre Library" "Downloads"))
              (file-arguments? #t)
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--talk=org.freedesktop.Notifications"))))))

(define-public celluloid-sandboxed
  (sandboxed celluloid
             (sandbox-policy
              (read-only-paths '(".config/mpv" ".cache/fontconfig"))
              (writable-paths '(".config/celluloid" ".cache/mpv"
                                ".local/state/mpv"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=io.github.celluloid_player.Celluloid"
                         "--own=org.mpris.MediaPlayer2.io.github.celluloid_player.Celluloid"))))))

(define-public darktable-sandboxed
  (sandboxed darktable
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/darktable" ".cache/darktable"
                                "Pictures"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public dino-sandboxed
  (sandboxed dino
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".local/share/dino" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (camera? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=im.dino.Dino"
                         "--talk=org.freedesktop.Notifications"))))))

(define-public element-desktop-sandboxed
  (sandboxed element-desktop
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/Element" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
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

(define-public firefox-sandboxed
  (sandboxed firefox
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              ;; Firefox keeps profiles here whenever the directory exists.
              (writable-paths '(".mozilla/firefox" ".cache/mozilla" "Downloads"))
              (file-arguments? #t)
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (camera? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       %portal-screencast-dbus-rules
                       '(;; Remote control by later launches, to open links.
                         "--own=org.mozilla.firefox.*"
                         "--own=org.mpris.MediaPlayer2.firefox.*"
                         "--talk=org.freedesktop.Notifications"))))))

(define-public fractal-sandboxed
  (sandboxed fractal
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".local/share/fractal" ".cache/fractal"
                                "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=org.gnome.Fractal"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.freedesktop.secrets"))))))

(define-public gajim-sandboxed
  (sandboxed gajim
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/gajim" ".local/share/gajim"
                                ".cache/gajim" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=org.gajim.Gajim"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.freedesktop.secrets"))))))

(define-public fragments-sandboxed
  (sandboxed fragments
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/fragments" ".cache/fragments" "Downloads"))
              (file-arguments? #t)
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=de.haeckerfelix.Fragments"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.freedesktop.secrets"))))))

(define-public gimp-sandboxed
  (sandboxed gimp
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/GIMP" ".cache/gimp" "Pictures"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=org.gimp.GIMP.*"))))))

(define-public google-chrome-stable-sandboxed
  (sandboxed google-chrome-stable
             (sandbox-policy
              ;; Managed policies are read from /etc/opt/chrome.
              (read-only-paths '("/etc/opt/chrome" ".cache/fontconfig"))
              (writable-paths '(".config/google-chrome" ".cache/google-chrome"
                                "Downloads"))
              (file-arguments? #t)
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (camera? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       %portal-screencast-dbus-rules
                       '(;; Chromium appends its PID, always 2 in the sandbox.
                         "--own=org.mpris.MediaPlayer2.chromium.instance2"
                         "--talk=org.freedesktop.Notifications"
                         ;; Passwords and cookies are encrypted with a key kept here.
                         "--talk=org.freedesktop.secrets"))))))

(define-public inkscape-sandboxed
  (sandboxed inkscape
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/inkscape" ".cache/inkscape" "Pictures"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=org.inkscape.Inkscape"))))))

(define-public kiwix-desktop-sandboxed
  (sandboxed kiwix-desktop
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              ;; Downloaded archives go to the data directory.
              (writable-paths '(".config/kiwix-desktop" ".local/share/kiwix-desktop"))
              (file-arguments? #t)
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (user-namespaces? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public komikku-sandboxed
  (sandboxed komikku
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".local/share/komikku" ".cache/komikku"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=info.febvre.Komikku"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.freedesktop.secrets"))))))

(define-public libreoffice-sandboxed
  (sandboxed libreoffice
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/libreoffice" "Documents"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public loupe-sandboxed
  (sandboxed loupe
             (sandbox-policy
              (read-only-paths '("Pictures" ".cache/fontconfig"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              ;; glycin runs its image loaders in their own sandbox.
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=org.gnome.Loupe"))))))

(define-public mpv-sandboxed
  (sandboxed mpv
             (sandbox-policy
              (read-only-paths '(".config/mpv" ".cache/fontconfig"))
              (writable-paths '(".cache/mpv" ".local/state/mpv"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t))))

(define-public nheko-sandboxed
  (sandboxed nheko
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/nheko" ".local/share/nheko"
                                ".cache/nheko" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (camera? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       %portal-screencast-dbus-rules
                       '(;; Qt appends its PID, always 2 in the sandbox.
                         "--own=org.kde.StatusNotifierItem-2-1"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.kde.StatusNotifierWatcher"
                         "--talk=org.freedesktop.secrets"))))))

(define-public obsidian-sandboxed
  (sandboxed obsidian
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              ;; Obsidian's default vault name.
              (writable-paths '(".config/obsidian" "Obsidian Vault"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (user-namespaces? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public okular-sandboxed
  (sandboxed okular
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".local/share/okular"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '(;; KDE appends its PID, always 2 in the sandbox.
                         "--own=org.kde.okular-2"))))))

(define-public papers-sandboxed
  (sandboxed papers
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public qbittorrent-sandboxed
  (sandboxed qbittorrent
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/qBittorrent" ".local/share/qBittorrent"
                                ".cache/qBittorrent" "Downloads"))
              (file-arguments? #t)
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '(;; Qt appends its PID, always 2 in the sandbox.
                         "--own=org.kde.StatusNotifierItem-2-1"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.kde.StatusNotifierWatcher"))))))

(define-public qalculate-gtk-sandboxed
  (sandboxed qalculate-gtk
             (sandbox-policy
              (read-only-paths '(".config/gtk-3.0/settings.ini"
                                 ".cache/fontconfig"))
              (writable-paths '(".config/qalculate" ".local/share/qalculate"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=io.github.Qalculate"))))))

(define-public rawtherapee-sandboxed
  (sandboxed rawtherapee
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              ;; Processing profiles are saved next to the pictures.
              (writable-paths '(".config/RawTherapee" ".cache/RawTherapee"
                                "Pictures"))
              (file-arguments? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public rhythmbox-sandboxed
  (sandboxed rhythmbox
             (sandbox-policy
              (read-only-paths '("Music" ".cache/fontconfig"))
              (writable-paths '(".local/share/rhythmbox" ".cache/rhythmbox"
                                "Podcasts"))
              (file-arguments? #t)
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=org.gnome.Rhythmbox3"
                         "--own=org.mpris.MediaPlayer2.rhythmbox"
                         "--talk=org.freedesktop.Notifications"))))))

(define-public signal-desktop-sandboxed
  (sandboxed signal-desktop
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/Signal" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
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
              (x11? #t)
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
              (x11? #t)
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
              (x11? #t)
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
              (x11? #t)
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

(define-public thunderbird-sandboxed
  (sandboxed thunderbird
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".thunderbird" ".cache/thunderbird" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=org.mozilla.thunderbird.*"
                         "--talk=org.freedesktop.Notifications"))))))

(define-public tidal-hifi-sandboxed
  (sandboxed tidal-hifi
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/tidal-hifi"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
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

(define-public transmission-sandboxed
  (sandboxed transmission
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/transmission" "Downloads"))
              (file-arguments? #t)
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=com.transmissionbt.Transmission"
                         "--talk=org.freedesktop.Notifications"))))
             #:output "gui"))

(define-public tuba-sandboxed
  (sandboxed tuba
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".cache/dev.geopjr.Tuba" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '("--own=dev.geopjr.Tuba"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.freedesktop.secrets"))))))

(define-public ungoogled-chromium-sandboxed
  (sandboxed ungoogled-chromium
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/chromium" ".cache/chromium" "Downloads"))
              (file-arguments? #t)
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (camera? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       %portal-screencast-dbus-rules
                       '(;; Chromium appends its PID, always 2 in the sandbox.
                         "--own=org.mpris.MediaPlayer2.chromium.instance2"
                         "--talk=org.freedesktop.Notifications"
                         ;; Passwords and cookies are encrypted with a key kept here.
                         "--talk=org.freedesktop.secrets"))))))

(define-public vitrine-sandboxed
  (sandboxed vitrine
             (sandbox-policy
              (read-only-paths '(".config/vitrine" "Pictures"))
              (writable-paths '(".local/share/vitrine" ".cache/vitrine"))
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules %portal-open-dbus-rules))))

(define-public zotero-sandboxed
  (sandboxed zotero
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".zotero" ".cache/zotero" "Zotero" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public zulip-desktop-sandboxed
  (sandboxed zulip-desktop
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (writable-paths '(".config/Zulip" "Downloads"))
              (network? #t)
              (wayland? #t)
              (x11? #t)
              (gpu? #t)
              (audio? #t)
              (user-namespaces? #t)
              (dbus-rules
               (append %portal-settings-dbus-rules
                       '(;; Chromium appends its PID, always 2 in the sandbox.
                         "--own=org.freedesktop.StatusNotifierItem-2-1"
                         "--talk=org.freedesktop.Notifications"
                         "--talk=org.kde.StatusNotifierWatcher"))))))
