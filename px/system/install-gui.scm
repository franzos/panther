;; PantherX disk image configuration file (GUI installer variant)
;;
;; Same as install.scm but adds the Wayland stack needed to run the iced
;; graphical installer (guix-install-gui) under cage:
;;   - cage          kiosk Wayland compositor (sole fullscreen client)
;;   - seatd         seat management for cage on a bare TTY
;;   - mesa          GLES2 renderer for cage/wlroots
;;   - wayland       libwayland-client/-cursor (dlopen'd by the iced binary)
;;   - libxkbcommon  keymap handling (dlopen'd by the iced binary)
;;   - foot          terminal, for debugging under cage
;;
;; On boot, tty1 runs a chooser (%px-install-chooser): pick the graphical
;; installer (launched under cage) or drop to a shell for the CLI installer.
;;
;; Generate a bootable image with:
;; $ guix system image --image-type=iso9660 px/system/install-gui.scm

(define-module (px system install-gui)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux)
  #:use-module (px packages package-management)
  #:use-module (px packages setup)
  #:use-module (px system common)
  #:export (px-installation-gui-os))

(define %issue
  ;; Greeting.
  "
\x1b[1;37mThis is a PantherX OS installation image (GUI).\x1b[0m
\x1b[1;37mVisit wiki.pantherx.org for more information.\x1b[0m

\x1b[1;33mThe installer chooser starts on the main console (tty1).\x1b[0m

")

(define %px-install-chooser
  ;; tty1 login program: let the user pick the graphical installer (launched
  ;; under cage) or drop to a shell for the CLI installer (guix-install).
  (program-file
   "px-install-chooser"
   #~(begin
       (use-modules (ice-9 rdelim))
       (let ((cage #$(file-append cage "/bin/cage"))
             (seatd-launch #$(file-append seatd "/bin/seatd-launch"))
             (gui #$(file-append guix-install-gui "/bin/guix-install-gui"))
             (openvt #$(file-append kbd "/bin/openvt"))
             (chvt #$(file-append kbd "/bin/chvt"))
             (pkill #$(file-append procps "/bin/pkill"))
             (bash #$(file-append bash "/bin/bash")))
         (define (launch-gui)
           ;; Clear any stale seat left by a previous (hard-killed) run, then
           ;; host the iced installer as cage's sole fullscreen client.
           (system* pkill "-f" "seatd-launch")
           (system* pkill "seatd")
           (false-if-exception (delete-file "/run/seatd.sock"))
           (false-if-exception (mkdir "/run/user" #o755))
           (false-if-exception (mkdir "/run/user/0" #o700))
           (setenv "XDG_RUNTIME_DIR" "/run/user/0")
           ;; kmscon owns DRM master on tty1 (this VT), so cage can't become
           ;; master here.  Launch it on a fresh VT instead: openvt -s switches
           ;; to it (seatd then hands cage the master), -w waits for cage to
           ;; exit; afterwards switch back to the menu on tty1.
           ;;
           ;; Run through `bash -lc` so the installer's children (lsblk, parted,
           ;; cryptsetup, guix ...) inherit the same login PATH/env the CLI path
           ;; gets -- the chooser itself starts with a near-empty PATH.
           (system* openvt "-s" "-w" "--" bash "-lc"
                    (string-append "exec " seatd-launch " -- " cage " -- " gui))
           (system* chvt "1"))
         (let loop ()
           (display "\n  PantherX OS Installer\n\n")
           (display "    1) Graphical installer (recommended)\n")
           (display "    2) Command line  (drops to a shell; run: guix-install)\n\n")
           (display "  Choice [1]: ")
           (force-output)
           (let ((line (read-line)))
             (if (and (string? line)
                      (string=? (string-trim-both line) "2"))
                 (begin
                   (display "\n  Install from the command line with:  guix-install\n")
                   (display "  (Wi-Fi: wpa_supplicant / wpa_cli; editor: nvim)\n\n")
                   (force-output)
                   (system* bash "-l"))
                 (launch-gui))
             (loop)))))))

(define px-installation-gui-os
  (operating-system
    (inherit installation-os)
    (host-name "panther")
    (kernel linux-lts)
    (firmware (list linux-firmware))

    (issue %issue)

    (packages (cons* guix-install                    ;; CLI installer
                     guix-install-gui                ;; graphical (iced) installer
                     wpa-supplicant                  ;; Wi-Fi connection via CLI
                     libimobiledevice                ;; iPhone USB tethering
                     neovim                          ;; Editing
                     cage                            ;; Wayland kiosk compositor
                     seatd                           ;; Seat management
                     mesa                            ;; cage/wlroots GLES2 renderer
                     wayland                         ;; libwayland-client/-cursor
                     libxkbcommon                    ;; keymap handling
                     foot                            ;; debug terminal under cage
                     (operating-system-packages installation-os)))
    (services
     (modify-services (operating-system-user-services installation-os)
       ;; Replace the default installer launch on tty1 with our GUI/CLI chooser.
       (kmscon-service-type
        config => (kmscon-configuration
                   (inherit config)
                   (login-program %px-install-chooser)))
       (guix-service-type
        config => (guix-configuration
                   (inherit config)
                   (guix (guix-for-channels %pantherx-default-channels))
                   (authorized-keys
                    (cons* %gofranz-substitute-server-key
                           %nonguix-substitute-server-key
                           %guix-moe-substitute-server-key
                           %default-authorized-guix-keys))
                   (substitute-urls
                    (cons* %gofranz-substitute-server-url
                           %nonguix-mirror-substitute-server-url
                           %guix-moe-substitute-server-url
                           %default-substitute-urls))
                   (channels %pantherx-default-channels)))))))

px-installation-gui-os
