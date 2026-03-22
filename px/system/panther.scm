;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2021-2026 Franz Geffke <mail@gofranz.com>

(define-module (px system panther)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)                ;; udev-service-type
  #:use-module (gnu packages networking)             ;; blueman
  #:use-module (gnu packages security-token)         ;; pam-u2f libu2f-host libu2f-server libfido2 yubikey-personalization
  #:use-module (guix gexp)
  #:use-module (px system os)
  #:use-module (px services security-token)          ;; nitro-key-udev-rule

  #:export (%panther-desktop-services
            %panther-desktop-services-minimal
            %panther-desktop-packages

            %panther-os
            %panther-desktop-os))

;;; Deprecated: use (px system os) instead.
;;; This module is kept for backward compatibility.
(define %panther-services-udev
  (list (simple-service 'custom-udev-rules udev-service-type
                        (list libu2f-host))

         ;; Adding plugdev group once should suffice
         (udev-rules-service 'nitro %nitro-key-udev-rule #:groups '("plugdev"))
         (udev-rules-service 'fido2 libfido2)
         (udev-rules-service 'yubikey yubikey-personalization)
         (udev-rules-service 'coinkite %coinkite-udev-rule)
         (udev-rules-service 'ledger %ledger-udev-rule)))

;; Deprecated: use %os-desktop-services from (px system os)
(define %panther-desktop-services
  (append %panther-services-udev
         %os-desktop-services))

;; Deprecated: use %os-desktop-services-minimal from (px system os)
(define %panther-desktop-services-minimal
  %os-desktop-services-minimal)

;; Deprecated: use %os-base-packages from (px system os)
(define %panther-desktop-packages
  (cons* pam-u2f
         libu2f-host
         libu2f-server
         blueman
         %os-base-packages))

;; Deprecated: use %os-base from (px system os)
(define %panther-os
  (operating-system
    (inherit %os-base)
    (services %os-base-services)
    (packages %os-base-packages)))

;; Deprecated: use %os-base with %os-desktop-services from (px system os)
(define %panther-desktop-os
  (operating-system
    (inherit %os-base)
    (services %panther-desktop-services)
    (packages %panther-desktop-packages)))
