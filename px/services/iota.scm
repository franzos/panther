;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px services iota)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module ((px packages iota) #:prefix pkg:)
  #:use-module (srfi srfi-1)

  #:export (iota-node-configuration
            iota-node-configuration?
            iota-node-configuration-package
            iota-node-configuration-config-file
            iota-node-configuration-user
            iota-node-configuration-group
            iota-node-configuration-data-directory
            iota-node-configuration-log-file
            iota-node-configuration-log-level
            iota-node-service-type))

;;
;; IOTA Node SERVICE
;;

(define-record-type* <iota-node-configuration>
  iota-node-configuration make-iota-node-configuration
  iota-node-configuration?
  (package
   iota-node-configuration-package
   (default pkg:iota))
  (config-file
   iota-node-configuration-config-file)
  (user
   iota-node-configuration-user
   (default "iota"))
  (group
   iota-node-configuration-group
   (default "iota"))
  (data-directory
   iota-node-configuration-data-directory
   (default "/var/lib/iota"))
  (log-file
   iota-node-configuration-log-file
   (default "/var/log/iota-node.log"))
  (log-level
   iota-node-configuration-log-level
   (default "info,iota_core=debug,consensus=debug,jsonrpsee=error")))

(define (iota-node-accounts config)
  (match-record config <iota-node-configuration> (user group data-directory)
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "IOTA node daemon user")
           (home-directory data-directory)
           (shell (file-append shadow "/sbin/nologin"))))))

(define (iota-node-activation config)
  (match-record config <iota-node-configuration> (user data-directory log-file)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let ((user (getpwnam #$user)))
            (mkdir-p #$data-directory)
            (chown #$data-directory (passwd:uid user) (passwd:gid user))
            (chmod #$data-directory #o750)
            (mkdir-p (dirname #$log-file)))))))

(define (iota-node-shepherd-service config)
  (match-record config <iota-node-configuration>
    (package config-file user group data-directory log-file log-level)
    (list (shepherd-service
           (documentation "Run the IOTA node daemon")
           (provision '(iota-node))
           (requirement '(networking user-processes))
           (start #~(make-forkexec-constructor
                     (list #$(file-append package "/bin/iota-node")
                           "--config-path" #$config-file)
                     #:user #$user
                     #:group #$group
                     #:log-file #$log-file
                     #:environment-variables
                     (cons* "RUST_BACKTRACE=1"
                            (string-append "RUST_LOG=" #$log-level)
                            (string-append "HOME=" #$data-directory)
                            "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                            "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                            (default-environment-variables))))
           (stop #~(make-kill-destructor #:grace-period 10))))))

(define (iota-node-log-rotations config)
  (list (iota-node-configuration-log-file config)))

(define iota-node-service-type
  (service-type
   (name 'iota-node)
   (description "Run and configure the IOTA node daemon.")
   (extensions
    (list (service-extension activation-service-type
                             iota-node-activation)
          (service-extension account-service-type
                             iota-node-accounts)
          (service-extension shepherd-root-service-type
                             iota-node-shepherd-service)
          (service-extension log-rotation-service-type
                             iota-node-log-rotations)))
   (default-value #f)))
