;; PantherX disk image configuration file
;;
;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system image --image-type=iso9660 px/system/install.scm
;; Using Guix time-machine
;; $ guix time-machine --channels=/path/to/channels.scm -- system disk-image -t iso9660 px/system/install.scm
;; Flash to USB stick (e.g. /dev/sdb) with:
;; $ dd if=/path/to/disk-image of=/dev/sdb bs=4M status=progress oflag=sync

(define-module (px system install)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages libusb)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux)
  #:use-module (px packages setup)
  #:use-module (px system common)
  #:export (px-installation-os))

(define %issue
  ;; Greeting.
  "
\x1b[1;37mThis is a PantherX OS installation image.\x1b[0m
\x1b[1;37mVisit wiki.pantherx.org for more information.\x1b[0m

\x1b[1;33mStart with: px-install run\x1b[0m

")

(define px-installation-os
  (operating-system
    (inherit installation-os)
    (host-name "panther")
    (kernel linux-lts)
    (firmware (list linux-firmware))

    (issue %issue)

    (packages (cons* px-install
                     wpa-supplicant                  ;; Wi-Fi connection via CLI
                     libimobiledevice                ;; iPhone USB tethering
                     neovim                          ;; Editing
                     (operating-system-packages installation-os)))
    (services
     (modify-services (operating-system-user-services installation-os)
       (guix-service-type
        config => (guix-configuration
                   (inherit config)
                   (guix (guix-for-channels %pantherx-default-channels))
                   (authorized-keys
                    (cons* %gofranz-substitute-server-key
                           %nonguix-substitute-server-key
                           %default-authorized-guix-keys))
                   (substitute-urls
                    (cons* %gofranz-substitute-server-url
                           %nonguix-substitute-server-url
                           %default-substitute-urls))
                   (channels %pantherx-default-channels)))))))

px-installation-os
