;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>
;;;
;;; Based on (gnu services admin) from GNU Guix:
;;; Copyright © 2016-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024 Gabriel Wicki <gabriel@erlikon.ch>
;;;
;;; This is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.

(define-module (px services unattended-upgrade)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (unattended-upgrade-configuration
            unattended-upgrade-configuration?
            unattended-upgrade-operating-system-file
            unattended-upgrade-operating-system-expression
            unattended-upgrade-configuration-schedule
            unattended-upgrade-configuration-channels
            unattended-upgrade-configuration-reboot?
            unattended-upgrade-configuration-services-to-restart
            unattended-upgrade-system-expiration
            unattended-upgrade-maximum-duration
            unattended-upgrade-configuration-system-load-paths
            unattended-upgrade-configuration-skip-on-battery?
            unattended-upgrade-configuration-log-file
            unattended-upgrade-service-type))

;;;
;;; Unattended upgrade with battery awareness.
;;;
;;; Drop-in replacement for (gnu services admin) unattended-upgrade-service-type
;;; with an additional skip-on-battery? field.
;;;

(define-record-type* <unattended-upgrade-configuration>
  unattended-upgrade-configuration make-unattended-upgrade-configuration
  unattended-upgrade-configuration?
  (operating-system-file unattended-upgrade-operating-system-file
                         (default "/run/current-system/configuration.scm"))
  (operating-system-expression unattended-upgrade-operating-system-expression
                               (default #f))
  (schedule             unattended-upgrade-configuration-schedule
                        (default "30 01 * * 0"))
  (channels             unattended-upgrade-configuration-channels
                        (default #~%default-channels))
  (reboot?              unattended-upgrade-configuration-reboot?
                        (default #f))
  (services-to-restart  unattended-upgrade-configuration-services-to-restart
                        (default '(unattended-upgrade)))
  (system-expiration    unattended-upgrade-system-expiration
                        (default (* 3 30 24 3600)))
  (maximum-duration     unattended-upgrade-maximum-duration
                        (default 3600))
  ;; Additional -L flags passed to 'guix system reconfigure'.  Needed when
  ;; the operating-system-file imports modules that live outside channels
  ;; (e.g. a local (common) module).  Note: Guix only stores the top-level
  ;; configuration file in the store — imported modules are resolved from
  ;; these paths at upgrade time, not from a stored snapshot.
  (system-load-paths    unattended-upgrade-configuration-system-load-paths
                        (default '()))
  (skip-on-battery?     unattended-upgrade-configuration-skip-on-battery?
                        (default #f))
  (log-file             unattended-upgrade-configuration-log-file
                        (default %unattended-upgrade-log-file)))

(define %unattended-upgrade-log-file
  "/var/log/unattended-upgrade.log")

(define (unattended-upgrade-shepherd-services config)
  (define channels
    (scheme-file "channels.scm"
                 (unattended-upgrade-configuration-channels config)))

  (define log
    (unattended-upgrade-configuration-log-file config))

  (define schedule
    (unattended-upgrade-configuration-schedule config))

  (define services
    (unattended-upgrade-configuration-services-to-restart config))

  (define reboot?
    (unattended-upgrade-configuration-reboot? config))

  (define expiration
    (unattended-upgrade-system-expiration config))

  (define skip-on-battery?
    (unattended-upgrade-configuration-skip-on-battery? config))

  (define config-file
    (unattended-upgrade-operating-system-file config))

  (define expression
    (unattended-upgrade-operating-system-expression config))

  (define load-paths
    (unattended-upgrade-configuration-system-load-paths config))

  (define arguments
    (if expression
        #~(list "-e" (object->string '#$expression))
        #~(append #$(append-map (lambda (p) (list "-L" p)) load-paths)
                  (list #$config-file))))

  (define code
    (with-imported-modules (source-module-closure '((guix build utils)
                                                    (gnu services herd)))
      #~(begin
          (use-modules (guix build utils)
                       (gnu services herd)
                       (ice-9 rdelim)
                       (srfi srfi-34))

          (setvbuf (current-output-port) 'line)
          (setvbuf (current-error-port) 'line)

          ;; Battery check: skip upgrade when on battery power.
          ;; Reads /sys/class/power_supply/*/type to find AC adapters ("Mains")
          ;; and checks their /online status.  Proceeds if no battery info is
          ;; available (e.g. desktops) or if any AC adapter is online.
          (when #$skip-on-battery?
            (let ((base "/sys/class/power_supply"))
              (when (file-exists? base)
                (let ((dir (opendir base)))
                  (let loop ((entry (readdir dir))
                             (has-mains? #f)
                             (mains-online? #f))
                    (if (eof-object? entry)
                        (begin
                          (closedir dir)
                          (when (and has-mains? (not mains-online?))
                            (format #t "skipping upgrade: running on battery~%")
                            (exit 0)))
                        (if (member entry '("." ".."))
                            (loop (readdir dir) has-mains? mains-online?)
                            (let* ((type-file
                                    (string-append base "/" entry "/type"))
                                   (online-file
                                    (string-append base "/" entry "/online"))
                                   (is-mains?
                                    (and (file-exists? type-file)
                                         (string=?
                                          (string-trim-right
                                           (call-with-input-file type-file
                                             read-line))
                                          "Mains")))
                                   (is-online?
                                    (and is-mains?
                                         (file-exists? online-file)
                                         (string=?
                                          (string-trim-right
                                           (call-with-input-file online-file
                                             read-line))
                                          "1"))))
                              (loop (readdir dir)
                                    (or has-mains? is-mains?)
                                    (or mains-online? is-online?))))))))))

          ;; 'guix time-machine' needs X.509 certificates to authenticate the
          ;; Git host.
          (setenv "SSL_CERT_DIR"
                  #$(file-append nss-certs "/etc/ssl/certs"))

          (format #t "starting upgrade...~%")
          (guard (c ((invoke-error? c)
                     (report-invoke-error c)
                     (exit 1)))
            (apply invoke #$(file-append guix "/bin/guix")
                   "time-machine" "-C" #$channels
                   "--" "system" "reconfigure" #$arguments)

            ;; 'guix system delete-generations' fails when there's no
            ;; matching generation.  Thus, catch 'invoke-error?'.
            (guard (c ((invoke-error? c)
                       (report-invoke-error c)))
              (invoke #$(file-append guix "/bin/guix")
                      "system" "delete-generations"
                      #$(string-append (number->string expiration)
                                       "s")))

            (unless #$reboot?
              ;; Rebooting effectively restarts services anyway and execution
              ;; would be halted here if mcron is restarted.
              (format #t "restarting services...~%")
              (for-each restart-service '#$services))

            ;; XXX: If this service has been restarted, this is not reached.
            (format #t "upgrade complete~%")

            ;; Stopping the root shepherd service triggers a reboot.
            (when #$reboot?
              (format #t "rebooting system~%")
              (force-output) ;ensure the entire log is written.
              (stop-service 'root))))))

  (define upgrade
    (program-file "unattended-upgrade" code))

  (list (shepherd-service
         (provision '(unattended-upgrade))
         (requirement '(user-processes networking))
         (modules '((shepherd service timer)))
         (start #~(make-timer-constructor
                   #$(if (string? schedule)
                         #~(cron-string->calendar-event #$schedule)
                         schedule)
                   (command '(#$upgrade))

                   #:log-file #$log

                   ;; Make sure the upgrade doesn't take too long.
                   #:max-duration
                   #$(unattended-upgrade-maximum-duration config)

                   ;; Wait for the previous attempt to terminate before trying
                   ;; again.
                   #:wait-for-termination? #t))
         (stop #~(make-timer-destructor))
         (actions (list shepherd-trigger-action)))))

(define unattended-upgrade-service-type
  (service-type
   (name 'unattended-upgrade)
   (extensions
    (list (service-extension shepherd-root-service-type
                             unattended-upgrade-shepherd-services)))
   (description
    "Periodically upgrade the system from the current configuration.
Extends the upstream service with @code{skip-on-battery?} support.")
   (default-value (unattended-upgrade-configuration))))
