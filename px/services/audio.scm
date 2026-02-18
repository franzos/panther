;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px services audio)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (px packages audio)  ; for rtkit package
  #:export (rtkit-daemon-service-type))

;;;
;;; RealtimeKit (rtkit)
;;;

(define %rtkit-user
  (user-account
   (name "rtkit")
   (group "rtkit")
   (system? #t)
   (comment "RealtimeKit daemon user")
   (home-directory "/var/empty")
   (shell (file-append shadow "/sbin/nologin"))))

(define %rtkit-group
  (user-group
   (name "rtkit")
   (system? #t)))

(define (rtkit-daemon-shepherd-service _)
  (list (shepherd-service
         (documentation "Run the RealtimeKit daemon.")
         (requirement '(dbus-system))
         (provision '(rtkit-daemon))
         (start #~(make-forkexec-constructor
                   (list #$(file-append rtkit "/libexec/rtkit-daemon"))
                   #:user "rtkit"
                   #:group "rtkit"))
         (stop #~(make-kill-destructor)))))

(define (rtkit-daemon-dbus-service _)
  (list rtkit))

(define (rtkit-daemon-accounts _)
  (list %rtkit-user %rtkit-group))

(define rtkit-daemon-service-type
  (service-type
   (name 'rtkit-daemon)
   (description "Run RealtimeKit, a D-Bus system service that changes the
scheduling policy of user processes/threads to real-time on request.")
   (extensions
    (list (service-extension shepherd-root-service-type
                            rtkit-daemon-shepherd-service)
          (service-extension dbus-root-service-type
                            rtkit-daemon-dbus-service)
          (service-extension account-service-type
                            rtkit-daemon-accounts)))
   (default-value #f)))
