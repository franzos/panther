;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>
;;; Copyright © 2023, 2025 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (px services networking)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ntp)
  #:use-module (gnu packages screen)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (px packages networking)
  #:use-module (srfi srfi-1)

  #:export (chrony-service-configuration
            chrony-service-type
            nebula-configuration
            nebula-configuration-package
            nebula-configuration-provision
            nebula-configuration-config-path
            %default-nebula-configuration
            nebula-service-type
            tailscale-configuration
            tailscale-service-type

            mullvad-daemon-service-type
            mullvad-daemon-shepherd-services
            mullvad-daemon-user
            mullvad-daemon-group
            mullvad-daemon-configuration
            mullvad-daemon-configuration?
            mullvad-daemon-configuration-fields
            mullvad-daemon-configuration-mullvad-vpn-desktop
            mullvad-daemon-configuration-user
            mullvad-daemon-configuration-group
            mullvad-daemon-configuration-supplementary-groups
            mullvad-daemon-configuration-cachedir
            mullvad-daemon-configuration-configdir
            mullvad-daemon-configuration-datadir
            mullvad-daemon-configuration-logdir))

;;
;; Chrony SERVICE
;;

(define-record-type* <chrony-service-configuration>
                     chrony-service-configuration
                     make-chrony-service-configuration
  chrony-service-configuration?
  (package
    chrony-service-configuration-package
    (default chrony))
  (user chrony-service-configuration-user
        (default "root"))
  (config chrony-service-configuration-config
          (default "server 0.pool.ntp.org iburst
server 1.pool.ntp.org iburst
server 2.pool.ntp.org iburst
server 3.pool.ntp.org iburst
driftfile /var/lib/chrony/drift
makestep 1.0 3
rtcsync
logdir /var/log/chrony")))

(define (chrony-service-config-file config)
  "Return the chorny configuration file corresponding to CONFIG."
  (computed-file "chrony.conf"
                 (with-imported-modules '((guix build utils))
                                        #~(begin
                                            (use-modules (guix build utils))
                                            (call-with-output-file #$output
                                              (lambda (port)
                                                (format port
                                                        #$config)))))))

(define chrony-shepherd-service
  (match-lambda
    (($ <chrony-service-configuration> package user config)
     (list (shepherd-service (provision '(chrony))
                             (documentation "Run chrony as a daemon")
                             (requirement '(networking))
                             (start #~(make-forkexec-constructor (list (string-append #$package
                                                                        "/sbin/chronyd")
                                                                       "-n"
                                                                       "-u"
                                                                       #$user
                                                                       "-f"
                                                                       #$(chrony-service-config-file
                                                                          config))))
                             (stop #~(make-kill-destructor)))))))

(define chrony-service-type
  (service-type (name 'chrony)
                (description "Chrony service")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   chrony-shepherd-service)))
                (default-value (chrony-service-configuration))))

;;
;; Nebula SERVICE
;;

(define-record-type* <nebula-configuration> nebula-configuration
                     make-nebula-configuration
  nebula-configuration?
  (package
    nebula-configuration-package
    (default nebula))
  (provision nebula-configuration-provision)
  (config-path nebula-configuration-config-path))

(define nebula-profile-packages
  (lambda (configurations)
    (fold (lambda (config prv)
            (let ((pkg (nebula-configuration-package config)))
              (if (memq pkg prv) prv
                  (cons pkg prv))))
          '() configurations)))

(define (nebula-shepherd-service config)
  (match config
    (($ <nebula-configuration> package provision config-path)
     (let ((log-path (string-append "/var/log/"
                                    (symbol->string (car provision)) ".log")))
       (shepherd-service (provision provision)
                         (documentation
                          "Run configured instance of nebula on system start")
                         (requirement '(networking user-processes))
                         (start #~(make-forkexec-constructor (list (string-append #$package
                                                                    "/bin/nebula")
                                                                   "-config"
                                                                   #$config-path)
                                                             #:log-file #$log-path
                                                             #:environment-variables
                                                             (cons*
                                                              "HOME=/root"
                                                              "XDG_DATA_HOME=/root/.local/share"
                                                              "XDG_CONFIG_HOME=/root/.config"
                                                              "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                                                              "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                                                              (default-environment-variables))))
                         (stop #~(make-kill-destructor)))))))

(define (nebula-shepherd-services configurations)
  (map nebula-shepherd-service configurations))

(define %default-nebula-configuration
  (nebula-configuration (provision '(nebula))
                        (config-path "/etc/nebula/config.yml")))

(define %nebula-log-rotations
  (list "/var/log/nebula.log"))

(define nebula-service-type
  (service-type (name 'nebula)
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   nebula-shepherd-services)
                                  (service-extension profile-service-type
                                                     nebula-profile-packages)
                                  (service-extension log-rotation-service-type
                                                     (const
                                                      %nebula-log-rotations))))
                (default-value (list %default-nebula-configuration))
                (description
                 "Run configured instance of nebula on system start")))

;;
;; Tailscale SERVICE
;;

(define-public (tailscale-configuration) '())

(define (tailscale-shepherd-service config)
  (list (shepherd-service
         (documentation "Run the tailscale daemon")
         (provision '(tailscaled tailscale))
         (requirement '(user-processes))
         (start
          #~(lambda _
              (fork+exec-command (list #$(file-append tailscaled "/bin/tailscaled")))))
         (stop #~(make-kill-destructor)))))

(define-public tailscale-service-type
  (service-type
   (name 'tailscale)
   (extensions
    (list (service-extension shepherd-root-service-type tailscale-shepherd-service)))
   (default-value (tailscale-configuration))
   (description "Run and connect to tailscale")))

;;
;; Mullvad VPN daemon
;;

(define %default-mullvad-daemon-supplementary-groups
  '("netdev" "users"))

(define-maybe/no-serialization string)

(define-configuration/no-serialization mullvad-daemon-configuration
  (mullvad-vpn-desktop
   (package mullvad-vpn-desktop)
   "The mullvad-vpn-desktop package that will be installed in the system profile.")
  (user
   (maybe-string)
   "The user that will be used to run @command{mullvad-daemon}.  When unset the
root user will be used.")
  (group
   (maybe-string)
   "The group that will be used to run @command{mullvad-daemon}.  When unset the
root group will be used.")
  (supplementary-groups
   (list-of-strings '())
   "A list of supplementary groups that will be created and to which the
configured @code{user} will be added.  The @code{netdev} and @code{users}
are always appended to this fields value.  This field is ignored when the
user is root.")
  (configdir
   (string "/etc/mullvad-vpn")
   "The directory where mullvad-daemon will write its configs.")
  (cachedir
   (string "/var/cache/mullvad-vpn")
   "The directory where mullvad-daemon will write cache data.")
  (datadir
   (string "/var/lib/mullvad-vpn")
   "The directory where mullvad-daemon will write state.")
  (logdir
   (string "/var/log/mullvad-vpn")
   "The directory where mullvad-daemon will write logs."))

(define (mullvad-daemon-user config)
  (define user (mullvad-daemon-configuration-user config))
  (if (maybe-value-set? user)
      user
      "root"))

(define (mullvad-daemon-group config)
  (define user (mullvad-daemon-configuration-user config))
  (define group (mullvad-daemon-configuration-group config))
  (if (and (maybe-value-set? user)
           (maybe-value-set? group))
      group
      "root"))

(define (mullvad-daemon-accounts config)
  (let ((user (mullvad-daemon-user config))
        (group (mullvad-daemon-group config))
        (supplementary-groups
         (mullvad-daemon-configuration-supplementary-groups config)))
    (if (string=? user "root")
        '()
        (append
         (map (lambda (name)
                (user-group (name name) (system? #t)))
              supplementary-groups)
         (list (user-group (name group) (system? #t))
               (user-account
                (name user)
                (group group)
                (system? #t)
                (comment "Mullvad's daemon user account")
                (home-directory "/var/empty")
                (shell "/run/current-system/profile/sbin/nologin")
                (supplementary-groups
                 (append supplementary-groups
                         %default-mullvad-daemon-supplementary-groups))))))))

(define (mullvad-daemon-activation config)
  (let ((user (mullvad-daemon-user config))
        (configdir (mullvad-daemon-configuration-configdir config))
        (cachedir (mullvad-daemon-configuration-cachedir config))
        (datadir (mullvad-daemon-configuration-datadir config))
        (logdir (mullvad-daemon-configuration-logdir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpwnam #$user))
               (uid (passwd:uid user))
               (gid (passwd:gid user))
               (dirs (list #$configdir #$cachedir #$datadir #$logdir)))
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    dirs)))))

(define (mullvad-daemon-shepherd-services config)
  (let* ((user (mullvad-daemon-user config))
         (group (mullvad-daemon-group config))
         (package
          (mullvad-daemon-configuration-mullvad-vpn-desktop config))
         (command (file-append package "/bin/mullvad-daemon")))
    (list
     (shepherd-service (provision '(mullvad-early-boot-blocking))
                       (respawn? #f)
                       (one-shot? #t)
                       (documentation "Mullvad early boot network blocker")
                       (start
                        #~(make-forkexec-constructor
                           (list #$command "--initialize-early-boot-firewall")
                           #:user #$user
                           #:group #$group))
                       (stop
                        #~(make-kill-destructor)))
     (shepherd-service (provision '(mullvad-daemon))
                       (requirement '(mullvad-early-boot-blocking
                                      networking))
                       (respawn? #t)
                       (documentation "Mullvad VPN daemon")
                       (start
                        #~(make-forkexec-constructor
                           (list #$command "-v" "--disable-stdout-timestamps")
                           #:user #$user
                           #:group #$group))
                       (stop
                        #~(make-kill-destructor))))))

(define mullvad-daemon-service-type
  (service-type (name 'mullvad-daemon)
                (extensions
                 (list (service-extension profile-service-type
                                          (lambda (config)
                                            (list
                                             (mullvad-daemon-configuration-mullvad-vpn-desktop config))))
                       (service-extension account-service-type
                                          mullvad-daemon-accounts)
                       (service-extension activation-service-type
                                          mullvad-daemon-activation)
                       (service-extension shepherd-root-service-type
                                          mullvad-daemon-shepherd-services)))
                (default-value (mullvad-daemon-configuration))
                (description
                 "This service provides a way to run Mullvad's daemon as Shepherd services.")))
