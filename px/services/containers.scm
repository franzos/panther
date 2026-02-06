;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px services containers)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (ice-9 match)
  #:use-module (px packages containers)
  #:export (home-podman-healthcheckd-configuration
            home-podman-healthcheckd-configuration?
            home-podman-healthcheckd-package
            home-podman-healthcheckd-log-level
            home-podman-healthcheckd-service-type))

(define-record-type* <home-podman-healthcheckd-configuration>
  home-podman-healthcheckd-configuration
  make-home-podman-healthcheckd-configuration
  home-podman-healthcheckd-configuration?
  (package   home-podman-healthcheckd-package    ;file-like
             (default podman-healthcheckd))
  (log-level home-podman-healthcheckd-log-level  ;string
             (default "info")))

(define (home-podman-healthcheckd-shepherd-services config)
  "Return a <shepherd-service> for podman-healthcheckd."
  (match-record config <home-podman-healthcheckd-configuration>
    (package log-level)
    (let ((command #~(list #$(file-append package "/bin/podman-healthcheckd")))
          (log-file #~(string-append %user-log-dir
                                     "/podman-healthcheckd.log")))
      (list (shepherd-service
             (documentation "Run podman-healthcheckd daemon.")
             (provision '(podman-healthcheckd))
             (modules '((shepherd support)))
             (start #~(make-forkexec-constructor
                       #$command
                       #:log-file #$log-file
                       #:environment-variables
                       (cons (string-append "RUST_LOG=" #$log-level)
                             (default-environment-variables))))
             (stop #~(make-kill-destructor)))))))

(define home-podman-healthcheckd-service-type
  (service-type
   (name 'home-podman-healthcheckd)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-podman-healthcheckd-shepherd-services)
          (service-extension home-profile-service-type
                             (lambda (config)
                               (list (home-podman-healthcheckd-package config))))))
   (default-value (home-podman-healthcheckd-configuration))
   (description
    "Install and configure @command{podman-healthcheckd} as a Shepherd service.")))
