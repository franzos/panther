;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px services mail)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (px packages mail)
  #:use-module (srfi srfi-1)

  #:export (bichon-configuration
            bichon-configuration?
            bichon-configuration-package
            bichon-configuration-root-dir
            bichon-configuration-encrypt-password-file
            bichon-configuration-http-port
            bichon-configuration-bind-ip
            bichon-configuration-public-url
            bichon-configuration-base-url
            bichon-configuration-log-level
            bichon-configuration-enable-smtp?
            bichon-configuration-smtp-port
            bichon-configuration-index-dir
            bichon-configuration-data-dir
            bichon-configuration-extra-env
            bichon-configuration-user
            bichon-configuration-group
            bichon-environment-variables
            bichon-service-type))

;;
;; Bichon email archiver SERVICE
;;
;; Bichon has no config file: every setting is a BICHON_* environment
;; variable.  One shared record + serializer feeds both the system and home
;; services.
;;

(define-record-type* <bichon-configuration>
  bichon-configuration make-bichon-configuration
  bichon-configuration?
  (package
   bichon-configuration-package
   (default bichon))
  (root-dir
   bichon-configuration-root-dir
   (default "/var/lib/bichon"))
  (encrypt-password-file
   bichon-configuration-encrypt-password-file)
  (http-port
   bichon-configuration-http-port
   (default 15630))
  (bind-ip
   bichon-configuration-bind-ip
   (default #f))
  (public-url
   bichon-configuration-public-url
   (default #f))
  (base-url
   bichon-configuration-base-url
   (default #f))
  (log-level
   bichon-configuration-log-level
   (default "info"))
  (enable-smtp?
   bichon-configuration-enable-smtp?
   (default #f))
  (smtp-port
   bichon-configuration-smtp-port
   (default 2525))
  (index-dir
   bichon-configuration-index-dir
   (default #f))
  (data-dir
   bichon-configuration-data-dir
   (default #f))
  (extra-env
   bichon-configuration-extra-env
   (default '()))
  (user
   bichon-configuration-user
   (default "bichon"))
  (group
   bichon-configuration-group
   (default "bichon")))

(define (bichon-environment-variables config)
  "Serialize CONFIG into a list of \"KEY=VALUE\" strings suitable for
make-forkexec-constructor's #:environment-variables.  Maybe fields that are #f
are omitted; extra-env (an alist of (string . string)) is appended verbatim."
  (define (maybe key value)
    (if value
        (list (string-append key "=" value))
        '()))
  (append
   (list (string-append "BICHON_ROOT_DIR="
                        (bichon-configuration-root-dir config))
         (string-append "BICHON_ENCRYPT_PASSWORD_FILE="
                        (bichon-configuration-encrypt-password-file config))
         (string-append "BICHON_HTTP_PORT="
                        (number->string
                         (bichon-configuration-http-port config)))
         (string-append "BICHON_LOG_LEVEL="
                        (bichon-configuration-log-level config))
         (string-append "BICHON_ENABLE_SMTP="
                        (if (bichon-configuration-enable-smtp? config)
                            "true" "false"))
         (string-append "BICHON_SMTP_PORT="
                        (number->string
                         (bichon-configuration-smtp-port config))))
   (maybe "BICHON_BIND_IP" (bichon-configuration-bind-ip config))
   (maybe "BICHON_PUBLIC_URL" (bichon-configuration-public-url config))
   (maybe "BICHON_BASE_URL" (bichon-configuration-base-url config))
   (maybe "BICHON_INDEX_DIR" (bichon-configuration-index-dir config))
   (maybe "BICHON_DATA_DIR" (bichon-configuration-data-dir config))
   (map (lambda (pair)
          (string-append (car pair) "=" (cdr pair)))
        (bichon-configuration-extra-env config))))

(define (bichon-accounts config)
  (match-record config <bichon-configuration> (user group root-dir)
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "Bichon email archiver daemon user")
           (home-directory root-dir)
           (shell (file-append shadow "/sbin/nologin"))))))

(define (bichon-activation config)
  (match-record config <bichon-configuration>
    (user root-dir encrypt-password-file)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let ((user (getpwnam #$user)))
            (mkdir-p #$root-dir)
            (chown #$root-dir (passwd:uid user) (passwd:gid user))
            (chmod #$root-dir #o750))
          (unless (file-exists? #$encrypt-password-file)
            (format #t "warning: bichon encrypt password file ~s is missing~%"
                    #$encrypt-password-file))))))

(define (bichon-shepherd-service config)
  (match-record config <bichon-configuration> (package user group root-dir)
    (list (shepherd-service
           (documentation "Run the Bichon email archiver daemon")
           (provision '(bichon))
           (requirement '(networking user-processes))
           (start #~(make-forkexec-constructor
                     (list #$(file-append package "/bin/bichon-server"))
                     #:user #$user
                     #:group #$group
                     #:log-file "/var/log/bichon.log"
                     #:environment-variables
                     (append (list #$@(bichon-environment-variables config)
                                   (string-append "HOME=" #$root-dir)
                                   "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                                   "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")
                             (default-environment-variables))))
           (stop #~(make-kill-destructor))))))

(define (bichon-log-rotations config)
  (list "/var/log/bichon.log"))

(define bichon-service-type
  (service-type
   (name 'bichon)
   (description "Run and configure the Bichon email archiver daemon.")
   (extensions
    (list (service-extension activation-service-type
                             bichon-activation)
          (service-extension account-service-type
                             bichon-accounts)
          (service-extension shepherd-root-service-type
                             bichon-shepherd-service)
          (service-extension log-rotation-service-type
                             bichon-log-rotations)))
   ;; No default-value: encrypt-password-file is required.
   (default-value #f)))
