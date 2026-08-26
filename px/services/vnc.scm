;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px services vnc)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (wayvnc-pam-configuration
            wayvnc-pam-configuration?
            wayvnc-pam-service-type))

;;; Commentary:
;;;
;;; wayvnc calls pam_start with the service name "wayvnc" but ships no policy
;;; file of its own, so without this service /etc/pam.d/wayvnc does not exist
;;; and enable_pam cannot work.
;;;
;;; Code:

(define-configuration/no-serialization wayvnc-pam-configuration
  (users
   list-of-strings
   "Accounts allowed to authenticate against wayvnc.  This field is mandatory:
wayvnc hands the client-supplied user name straight to PAM without comparing
it to anything, so without a restriction here any account on the machine can
authenticate and drive the session wayvnc is sharing.

Listing more than one account grants all of them that power over each other's
sessions, because @file{/etc/pam.d/wayvnc} is system-wide and PAM cannot
express \"the user this wayvnc runs as\".  Prefer a single name.")

  (deny
   (integer 3)
   "Consecutive failures before the account is locked out.")

  (fail-interval
   (integer 900)
   "Window, in seconds, over which consecutive failures are counted.")

  (unlock-time
   (integer 600)
   "Seconds before a locked-out account is allowed to try again.")

  (faillock-directory
   (string "/var/lib/wayvnc/faillock")
   "Directory holding the per-user failure tallies.  Pointing this at the
@code{/var/run/faillock} default would silently disable lockout: wayvnc runs
unprivileged and cannot create a tally there, and @code{pam_faillock} reads a
missing tally as \"no failures so far\"."))

(define (wayvnc-pam-service config)
  (match-record config <wayvnc-pam-configuration>
    (users deny fail-interval unlock-time faillock-directory)
    (when (null? users)
      (error "wayvnc-pam-configuration: 'users' must name at least one account"))
    (let ((faillock-options
           (list "silent"
                 ;; wayvnc calls PAM synchronously from its event loop, so the
                 ;; two second delay both modules register on failure would
                 ;; freeze capture and input for everyone already connected.
                 ;; libpam applies the largest delay any module asks for, so
                 ;; every one of them has to opt out.
                 "nodelay"
                 (string-append "dir=" faillock-directory)
                 (string-append "deny=" (number->string deny))
                 (string-append "fail_interval=" (number->string fail-interval))
                 (string-append "unlock_time=" (number->string unlock-time)))))
      (list
       (pam-service
        (name "wayvnc")
        (auth
         (list (pam-entry
                ;; requisite, not required: a name that can never log in should
                ;; not reach pam_unix at all, so its password is never checked
                ;; and it leaves no tally behind.
                (control "requisite")
                (module "pam_succeed_if.so")
                (arguments
                 (list "quiet" "user" "in" (string-join users ":"))))
               (pam-entry
                (control "required")
                (module "pam_faillock.so")
                (arguments (cons "preauth" faillock-options)))
               (pam-entry
                (control "[success=1 default=bad]")
                (module "pam_unix.so")
                (arguments '("nodelay")))
               (pam-entry
                (control "[default=die]")
                (module "pam_faillock.so")
                (arguments (cons "authfail" faillock-options)))
               (pam-entry
                (control "sufficient")
                (module "pam_faillock.so")
                (arguments (cons "authsucc" faillock-options)))
               (pam-entry
                (control "required")
                (module "pam_deny.so"))))
        (account
         (list (pam-entry
                (control "required")
                (module "pam_unix.so")))))))))

(define (wayvnc-pam-activation config)
  (match-record config <wayvnc-pam-configuration> (users faillock-directory)
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$faillock-directory)
        (chmod #$faillock-directory #o755)
        ;; pam_faillock opens the tally with O_CREAT and no ownership check, so
        ;; a directory the users could create in would let any of them plant a
        ;; tally for another and either clear it mid-attack or lock them out.
        ;; Pre-create each one instead and leave the directory to root.  Group
        ;; root and mode 0660 are what the module itself expects: it forces
        ;; 0660 on any tally that is not group-writable, so handing it the
        ;; user's own group would open the tally to everyone in that group.
        (for-each (lambda (user)
                    (let ((file (string-append #$faillock-directory "/" user))
                          (pw (false-if-exception (getpwnam user))))
                      (when pw
                        (unless (file-exists? file)
                          (close-port (open-output-file file)))
                        (chown file (passwd:uid pw) 0)
                        (chmod file #o660))))
                  '#$users))))

(define wayvnc-pam-service-type
  (service-type
   (name 'wayvnc-pam)
   (extensions
    (list (service-extension pam-root-service-type
                             wayvnc-pam-service)
          (service-extension activation-service-type
                             wayvnc-pam-activation)))
   (description
    "Install the PAM policy that @command{wayvnc} authenticates against when
@code{enable_pam} is set, restricting logins to named accounts and locking an
account out after repeated failures.")))
