;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>
;;;
;;; This is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.

(define-module (px home services unattended-upgrade)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:export (home-unattended-upgrade-configuration
            home-unattended-upgrade-configuration?
            home-unattended-upgrade-configuration-config-file
            home-unattended-upgrade-configuration-channels
            home-unattended-upgrade-configuration-schedule
            home-unattended-upgrade-configuration-system-expiration
            home-unattended-upgrade-configuration-maximum-duration
            home-unattended-upgrade-configuration-skip-on-battery?
            home-unattended-upgrade-configuration-log-file
            home-unattended-upgrade-service-type))

;;;
;;; Unattended home upgrade with battery awareness.
;;;
;;; Periodically runs: guix pull -C <channels> && guix home reconfigure <config>
;;;

(define-record-type* <home-unattended-upgrade-configuration>
  home-unattended-upgrade-configuration
  make-home-unattended-upgrade-configuration
  home-unattended-upgrade-configuration?
  (config-file      home-unattended-upgrade-configuration-config-file)
  (channels         home-unattended-upgrade-configuration-channels
                    (default #~%default-channels))
  (schedule         home-unattended-upgrade-configuration-schedule
                    (default "0 19 * * *"))
  (system-expiration home-unattended-upgrade-configuration-system-expiration
                    (default (* 3 30 24 3600)))
  (maximum-duration home-unattended-upgrade-configuration-maximum-duration
                    (default 3600))
  (skip-on-battery? home-unattended-upgrade-configuration-skip-on-battery?
                    (default #f))
  (log-file         home-unattended-upgrade-configuration-log-file
                    (default #f)))

(define (home-unattended-upgrade-shepherd-services config)
  (define channels
    (scheme-file "home-channels.scm"
                 (home-unattended-upgrade-configuration-channels config)))

  (define log
    (or (home-unattended-upgrade-configuration-log-file config)
        (string-append (getenv "HOME")
                       "/.local/state/unattended-home-upgrade.log")))

  (define schedule
    (home-unattended-upgrade-configuration-schedule config))

  (define config-file
    (home-unattended-upgrade-configuration-config-file config))

  (define expiration
    (home-unattended-upgrade-configuration-system-expiration config))

  (define skip-on-battery?
    (home-unattended-upgrade-configuration-skip-on-battery? config))

  (define code
    (with-imported-modules (source-module-closure '((guix build utils)))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 rdelim)
                       (srfi srfi-34))

          (setvbuf (current-output-port) 'line)
          (setvbuf (current-error-port) 'line)

          ;; Battery check: skip upgrade when on battery power.
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

          (setenv "SSL_CERT_DIR"
                  #$(file-append nss-certs "/etc/ssl/certs"))

          (define %updated-guix
            (string-append (getenv "HOME")
                           "/.config/guix/current/bin/guix"))

          (format #t "starting home upgrade...~%")
          (guard (c ((invoke-error? c)
                     (report-invoke-error c)
                     (exit 1)))
            ;; Step 1: Update guix with specified channels
            (invoke #$(file-append guix "/bin/guix")
                    "pull" "-C" #$channels)

            ;; Step 2: Reconfigure home with the freshly pulled guix
            (invoke %updated-guix
                    "home" "reconfigure" #$config-file)

            ;; Step 3: Clean up old generations
            (guard (c ((invoke-error? c)
                       (report-invoke-error c)))
              (invoke %updated-guix
                      "home" "delete-generations"
                      #$(string-append (number->string expiration)
                                       "s")))

            (format #t "home upgrade complete~%")))))

  (define upgrade
    (program-file "unattended-home-upgrade" code))

  (list (shepherd-service
         (provision '(unattended-home-upgrade))
         (documentation "Periodically upgrade the home environment.")
         (modules '((shepherd service timer)))
         (start #~(make-timer-constructor
                   (cron-string->calendar-event #$schedule)
                   (command '(#$upgrade))
                   #:log-file #$log
                   #:max-duration
                   #$(home-unattended-upgrade-configuration-maximum-duration
                      config)
                   #:wait-for-termination? #t))
         (stop #~(make-timer-destructor))
         (actions (list shepherd-trigger-action)))))

(define home-unattended-upgrade-service-type
  (service-type
   (name 'home-unattended-upgrade)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-unattended-upgrade-shepherd-services)))
   (description
    "Periodically upgrade the home environment by running
@command{guix pull} followed by @command{guix home reconfigure}.
Supports @code{skip-on-battery?} to avoid upgrades on battery power.")))
