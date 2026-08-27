;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px services apparmor)
  #:use-module (px packages apparmor)
  #:use-module (gnu services)
  #:use-module ((gnu services base) #:select (file-system-service-type))
  #:use-module (gnu services shepherd)
  #:use-module (gnu system file-systems)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (apparmor-configuration
            apparmor-configuration?
            apparmor-configuration-package
            apparmor-configuration-profiles
            %apparmor-securityfs-file-system
            apparmor-service-type))

;;;
;;; AppArmor - mount securityfs and load profiles.
;;;
;;; Defaults to px's apparmor over the 4.1.2 in Guix; a profile denying a unix
;;; socket by path needs network_v9 policy, which 4.1.2 can't emit.
;;;

(define-record-type* <apparmor-configuration>
  apparmor-configuration make-apparmor-configuration
  apparmor-configuration?
  (package
   apparmor-configuration-package
   (default apparmor))
  (profiles
   apparmor-configuration-profiles
   (default '())))                      ;list of file-like objects

(define %apparmor-securityfs-mount-point "/sys/kernel/security")

(define %apparmor-securityfs-file-system
  ;; mount-may-fail? because it's often already mounted.
  (file-system
    (type "securityfs")
    (device "none")
    (mount-point %apparmor-securityfs-mount-point)
    (check? #f)
    (create-mount-point? #t)
    (mount-may-fail? #t)))

(define %apparmor-securityfs-requirement
  ;; Copied from 'file-system->shepherd-service-name' in (gnu services base),
  ;; which doesn't export it.
  (symbol-append 'file-system-
                 (string->symbol %apparmor-securityfs-mount-point)))

(define (apparmor-shepherd-services config)
  (match-record config <apparmor-configuration>
    (package profiles)
    (list
     (shepherd-service
      (documentation "Load AppArmor profiles into the kernel.")
      (provision '(apparmor))
      (requirement (list %apparmor-securityfs-requirement))
      (one-shot? #t)
      (respawn? #f)
      ;; A one-shot never reaches 'running', so every reconfigure starts it
      ;; again - that's what reloads the profiles.
      (start
       (if (null? profiles)
           #~(lambda _ #t)
           #~(lambda _
               (zero? (apply system*
                             #$(file-append package "/sbin/apparmor_parser")
                             "-r"       ;replace, don't fail on already loaded
                             ;; The parser warns about a parser.conf we don't
                             ;; ship; there is no flag to just skip it.
                             "--config-file=/dev/null"
                             ;; Where abstractions/ and tunables/ resolve from.
                             "-b" #$(file-append package "/etc/apparmor.d")
                             (list #$@profiles))))))))))

(define apparmor-service-type
  (service-type
   (name 'apparmor)
   (description "Mount securityfs and load AppArmor profiles into the kernel
at boot and on every reconfigure.  Profiles are given as file-like objects and
are loaded from the store.  Also makes the @command{aa-*} administration tools
available; note they live in @file{sbin}, not @file{bin}.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             apparmor-shepherd-services)
          (service-extension file-system-service-type
                             (const (list %apparmor-securityfs-file-system)))
          (service-extension profile-service-type
                             (compose list apparmor-configuration-package))))
   (default-value (apparmor-configuration))))
