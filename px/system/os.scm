;;; Package Repository for GNU Guix
;;; Copyright © 2021-2026 Franz Geffke <mail@gofranz.com>

(define-module (px system os)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dbus)                   ;; polkit-service-type
  #:use-module (gnu services sound)                  ;; pulseaudio-service-type alsa-service-type
  #:use-module (gnu services xorg)                   ;; gdm-service-type
  #:use-module (gnu services sddm)                   ;; sddm-service-type
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages admin)                  ;; wpa-supplicant
  #:use-module (gnu packages libusb)                 ;; libimobiledevice
  #:use-module (gnu packages vim)                    ;; neovim
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (srfi srfi-1)                         ;; remove
  #:use-module (nongnu packages linux)
  #:use-module (px system common)
  #:use-module (px packages libguix)                ;; libguix-polkit
  #:use-module ((px packages package-management)
                #:select (guix-gui))               ;; desktop package-management frontend

  #:export (%os-base-services
            %os-desktop-services
            %os-desktop-services-minimal
            %os-base-packages
            %os-desktop-packages
            %os-base))

(define %os-base-services
  (modify-services %base-services
    (guix-service-type config =>
      (guix-configuration
        (inherit config)
        (guix (guix-for-channels %pantherx-default-channels))
        (authorized-keys
        (append %all-substitute-server-keys
                %default-authorized-guix-keys))
        (substitute-urls
        (append %all-substitute-server-urls
                %default-substitute-urls))
        (channels %pantherx-default-channels)))))

(define %os-desktop-services
  (cons (simple-service 'libguix-polkit
                        polkit-service-type
                        (list libguix-polkit))
        (modify-services %desktop-services
          (guix-service-type config =>
            (guix-configuration
              (inherit config)
              (guix (guix-for-channels %pantherx-default-channels))
              (authorized-keys
              (append %all-substitute-server-keys
                      %default-authorized-guix-keys))
              (substitute-urls
              (append %all-substitute-server-urls
                      %default-substitute-urls))
              (channels %pantherx-default-channels))))))

;; Desktop services without login/display managers and audio.
;; Use this when bringing your own (greetd, pipewire, etc.).
(define %os-desktop-services-minimal
  (remove (lambda (service)
            (memq (service-kind service)
                  (list login-service-type
                        agetty-service-type
                        mingetty-service-type
                        gdm-service-type
                        sddm-service-type
                        pulseaudio-service-type
                        alsa-service-type)))
          %os-desktop-services))

(define %os-base-packages
  (cons* wpa-supplicant
         libimobiledevice
         neovim
         %base-packages))

;; Base packages plus the graphical Guix frontend, for desktop systems.
(define %os-desktop-packages
  (cons* guix-gui
         %os-base-packages))

(define %os-base
  (operating-system
    (host-name "panther")
    (timezone "Europe/Berlin")
    (locale "en_US.UTF-8")
    (kernel linux)
    (firmware (list linux-firmware))

    (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets '("/dev/vda"))))

    (file-systems (cons (file-system
                        (device (file-system-label "my-root"))
                        (mount-point "/")
                        (type "ext4"))
                        %base-file-systems))

    (users (cons (user-account
                  (name "panther")
                  (comment "Default account")
                  (group "users")
                  (password (crypt "pantherx" "$6$abc"))
                  (supplementary-groups '("wheel" "audio" "video")))
                 %base-user-accounts))

    (services %os-base-services)
    (packages %os-base-packages)))
