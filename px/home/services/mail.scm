;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px home services mail)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (px services mail)
  #:use-module (srfi srfi-1)

  #:export (home-bichon-service-type))

;;
;; Bichon email archiver HOME SERVICE
;;
;; Reuses the shared bichon-configuration record and serializer from
;; (px services mail). No system user/group; data and logs live under XDG
;; directories, and the daemon binds localhost by default.
;;

;; The shared record's root-dir defaults to the system path; for home use we
;; resolve it under $XDG_DATA_HOME instead, unless the user set an explicit
;; value.
(define %system-default-root-dir "/var/lib/bichon")

(define (bichon-home-explicit-root config)
  "The user-set root-dir, or #f when left at the shared system default."
  (let ((root (bichon-configuration-root-dir config)))
    (and (not (string=? root %system-default-root-dir)) root)))

(define (bichon-home-root-gexp explicit-root)
  "Gexp evaluating at runtime to EXPLICIT-ROOT when set, else
$XDG_DATA_HOME/bichon (fallback ~/.local/share/bichon)."
  #~(or #$explicit-root
        (string-append
         (or (getenv "XDG_DATA_HOME")
             (string-append (getenv "HOME") "/.local/share"))
         "/bichon")))

(define %bichon-home-log-dir-gexp
  #~(or (getenv "XDG_LOG_HOME")
        (string-append (getenv "HOME") "/.local/var/log")))

(define (bichon-home-env-without-root config)
  "Serialized environment minus BICHON_ROOT_DIR, so the home service can
substitute an XDG-based default computed at start time."
  (remove (lambda (entry)
            (string-prefix? "BICHON_ROOT_DIR=" entry))
          (bichon-environment-variables config)))

(define (home-bichon-activation config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$(bichon-home-root-gexp (bichon-home-explicit-root config)))
        (mkdir-p #$%bichon-home-log-dir-gexp))))

(define (home-bichon-shepherd-service config)
  (let ((explicit-root (bichon-home-explicit-root config))
        ;; Personal daemon: bind localhost unless the user set bind-ip.
        (bind-ip-default
         (if (bichon-configuration-bind-ip config)
             '()
             '("BICHON_BIND_IP=127.0.0.1"))))
    (list
     (shepherd-service
      (documentation "Run the Bichon email archiver daemon")
      (provision '(bichon))
      (requirement '())
      (start
       #~(make-forkexec-constructor
          (list #$(file-append (bichon-configuration-package config)
                               "/bin/bichon-server"))
          #:log-file (string-append #$%bichon-home-log-dir-gexp "/bichon.log")
          #:environment-variables
          (let* ((root #$(bichon-home-root-gexp explicit-root))
                 (ssl-cert-file (getenv "SSL_CERT_FILE"))
                 (ssl-cert-dir (getenv "SSL_CERT_DIR"))
                 ;; make-forkexec-constructor does not inherit the user's
                 ;; environment, so propagate CA cert config when present.
                 (ssl-vars
                  (append
                   (if ssl-cert-file
                       (list (string-append "SSL_CERT_FILE=" ssl-cert-file))
                       '())
                   (if ssl-cert-dir
                       (list (string-append "SSL_CERT_DIR=" ssl-cert-dir))
                       '()))))
            (append (list (string-append "BICHON_ROOT_DIR=" root)
                          #$@(bichon-home-env-without-root config)
                          #$@bind-ip-default)
                    ssl-vars
                    (default-environment-variables)))))
      (stop #~(make-kill-destructor))))))

(define home-bichon-service-type
  (service-type
   (name 'home-bichon)
   (description "Run and configure the Bichon email archiver as a user daemon.")
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-bichon-shepherd-service)
          (service-extension home-activation-service-type
                             home-bichon-activation)
          (service-extension home-profile-service-type
                             (lambda (config)
                               (list (bichon-configuration-package config))))))
   ;; No default-value: encrypt-password-file is required.
   (default-value #f)))
