;;; Package Repository for GNU Guix
;;; Copyright © 2021-2026 Franz Geffke <mail@gofranz.com>

(define-module (px services ntp)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ntp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (chrony-service-configuration
            chrony-service-configuration?
            chrony-service-type))

;;
;; Chrony SERVICE
;;

(define %default-chrony-config
  ;; NTS (RFC 8915) authenticates time packets via TLS, preventing on-path
  ;; attackers from forging NTP responses.  Requires outbound TCP/4460 in
  ;; addition to the usual UDP/123.  There is no NTS pool (TLS certificates
  ;; break the pooling model), so sources are listed individually.
  "server time.cloudflare.com iburst nts
server nts.netnod.se iburst nts
server ptbtime1.ptb.de iburst nts
server ptbtime2.ptb.de iburst nts
server ntppool1.time.nl iburst nts
driftfile /var/lib/chrony/drift
ntsdumpdir /var/lib/chrony
makestep 1.0 3
rtcsync
")

(define-record-type* <chrony-service-configuration>
                     chrony-service-configuration
                     make-chrony-service-configuration
  chrony-service-configuration?
  (package chrony-service-configuration-package
           (default chrony))
  (config  chrony-service-configuration-config
           (default %default-chrony-config)))

(define %chrony-accounts
  (list (user-group
         (name "chrony")
         (system? #t))
        (user-account
         (name "chrony")
         (group "chrony")
         (system? #t)
         (comment "chrony daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (chrony-service-activation cfg)
  "Create /var/lib/chrony owned by the chrony user so chronyd can write its
drift file."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let ((pw  (getpw "chrony"))
              (dir "/var/lib/chrony"))
          (mkdir-p dir)
          (chown dir (passwd:uid pw) (passwd:gid pw))
          (chmod dir #o755)))))

(define (chrony-shepherd-service cfg)
  (match-record cfg <chrony-service-configuration>
    (package config)
    (let ((chrony.conf (plain-file "chrony.conf" config)))
      (list (shepherd-service
             (provision '(chrony))
             (documentation "Run chronyd, the NTP daemon.")
             (requirement '(user-processes networking))
             (actions (list (shepherd-configuration-action chrony.conf)))
             (start #~(make-forkexec-constructor
                       (list #$(file-append package "/sbin/chronyd")
                             "-n" "-u" "chrony" "-f" #$chrony.conf)
                       #:log-file "/var/log/chrony.log"))
             (stop #~(make-kill-destructor)))))))

(define chrony-service-type
  (service-type
   (name 'chrony)
   (description "Run @command{chronyd}, the Network Time Protocol (NTP)
daemon from the @uref{https://chrony-project.org, Chrony project}.  The
daemon will keep the system clock synchronized with the configured
servers.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             chrony-shepherd-service)
          (service-extension account-service-type
                             (const %chrony-accounts))
          (service-extension activation-service-type
                             chrony-service-activation)))
   (default-value (chrony-service-configuration))))
