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
         (define keymap-file "/run/guix-install-keymap")
         (define relaunch-file "/run/guix-install-relaunch")
         (define (read-trimmed path)
           (false-if-exception
            (call-with-input-file path
              (lambda (port) (string-trim-both (read-line port))))))
         (define (launch-gui)
           (let loop ()
             ;; Clear any stale seat left by a previous (hard-killed) run.
             (system* pkill "-f" "seatd-launch")
             (system* pkill "seatd")
             (false-if-exception (delete-file "/run/seatd.sock"))
             (false-if-exception (mkdir "/run/user" #o755))
             (false-if-exception (mkdir "/run/user/0" #o700))
             (setenv "XDG_RUNTIME_DIR" "/run/user/0")
             ;; Apply the layout the GUI last requested (cage reads
             ;; XKB_DEFAULT_LAYOUT only at startup, so it must be set before cage).
             (let ((layout (read-trimmed keymap-file)))
               (when (and (string? layout) (not (string=? layout "")))
                 (setenv "XKB_DEFAULT_LAYOUT" layout)))
             ;; Fresh VT (kmscon owns DRM master on tty1). bash -lc so the
             ;; installer's children inherit the login PATH/env.
             (system* openvt "-s" "-w" "--" bash "-lc"
                      (string-append "exec " seatd-launch " -- " cage " -- " gui))
             ;; If the GUI asked for a relaunch (keyboard change), consume the
             ;; marker and loop; otherwise return to the chooser menu.
             (if (file-exists? relaunch-file)
                 (begin
                   (false-if-exception (delete-file relaunch-file))
                   (loop))
                 (system* chvt "1"))))
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
                    (append %all-substitute-server-keys
                            %default-authorized-guix-keys))
                   (substitute-urls
                    (append %all-substitute-server-urls
                            %default-substitute-urls))
                   (channels %pantherx-default-channels)))))))

px-installation-gui-os
