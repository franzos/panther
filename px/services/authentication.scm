;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px services authentication)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages security-token)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:export (pam-u2f-configuration
            pam-u2f-configuration?
            pam-u2f-configuration-pam-u2f
            pam-u2f-configuration-mappings
            pam-u2f-configuration-mappings-file
            pam-u2f-configuration-pam-services
            pam-u2f-configuration-control
            pam-u2f-configuration-relying-party
            pam-u2f-configuration-user-presence?
            pam-u2f-configuration-cue?
            pam-u2f-configuration-extra-options
            pam-u2f-service-type

            pam-fprintd-configuration
            pam-fprintd-configuration?
            pam-fprintd-configuration-fprintd
            pam-fprintd-configuration-pam-services
            pam-fprintd-configuration-control
            pam-fprintd-service-type))

;;;
;;; Both services prepend one auth entry to a set of PAM services, so a key
;;; touch or a fingerprint satisfies the stack and anything else falls through
;;; to the modules that were already there.
;;;

(define (pam-auth-extension service-names entry)
  (pam-extension
   (transformer
    (lambda (pam)
      (if (member (pam-service-name pam) service-names)
          (pam-service
           (inherit pam)
           (auth (cons entry (pam-service-auth pam))))
          pam)))))

(define-maybe/no-serialization string)


;;;
;;; pam_u2f
;;;

(define-configuration/no-serialization pam-u2f-configuration
  (pam-u2f
   (package pam-u2f)
   "The pam-u2f package, which also provides @command{pamu2fcfg}.")
  (mappings
   (file-like)
   "File mapping user names to their FIDO credentials, one line per user as
written by @command{pamu2fcfg}.  Public key material only.  It is copied out
of the store at activation (see @code{mappings-file}): pam_u2f rejects an
authfile owned by anyone but root or the authenticating user, and store files
are owned by the build user when guix-daemon runs unprivileged.  That
rejection is silent under @code{sufficient}, the stack just falls through to
the next module.")
  (mappings-file
   (string "/etc/u2f_mappings")
   "Where the mappings are installed, root-owned with mode 0400, and what
pam_u2f is told as @code{authfile}.  Must be absolute: a relative path is
resolved against the user's home directory, and makes pam_u2f drop privileges
to read it.")
  (pam-services
   (list-of-strings '("sudo"))
   "PAM services whose auth stack gets the entry.")
  (control
   (string "sufficient")
   "PAM control flag.  @code{sufficient} makes a touch enough on its own and
leaves the password as the fallback; @code{required} demands both.")
  (relying-party
   maybe-string
   "Relying party identifier the credentials were registered against, passed
as @code{origin}.  Unset means pam_u2f's default, @code{pam://<hostname>},
which stops matching if the host is renamed; register with
@command{pamu2fcfg -o <origin>} and set the same value here to make the
mapping portable across hosts.")
  (user-presence?
   (boolean #t)
   "Require a touch on the authenticator (@code{userpresence=1}).")
  (cue?
   (boolean #t)
   "Print a prompt asking for the touch (@code{cue}).")
  (extra-options
   (list-of-strings '())
   "Further pam_u2f options, appended verbatim."))

(define (pam-u2f-arguments config)
  (match-record config <pam-u2f-configuration>
    (mappings-file relying-party user-presence? cue? extra-options)
    (append
     (list (string-append "authfile=" mappings-file))
     (if (maybe-value-set? relying-party)
         (list (string-append "origin=" relying-party))
         '())
     (list (string-append "userpresence=" (if user-presence? "1" "0")))
     (if cue? '("cue") '())
     extra-options)))

(define (pam-u2f-pam-services config)
  (list (pam-auth-extension
         (pam-u2f-configuration-pam-services config)
         (pam-entry
          (control (pam-u2f-configuration-control config))
          (module (file-append (pam-u2f-configuration-pam-u2f config)
                               "/lib/security/pam_u2f.so"))
          (arguments (pam-u2f-arguments config))))))

(define (pam-u2f-activation config)
  (let ((target (pam-u2f-configuration-mappings-file config)))
    #~(begin
        (false-if-exception (delete-file #$target))
        (copy-file #$(pam-u2f-configuration-mappings config) #$target)
        (chown #$target 0 0)
        (chmod #$target #o400))))

(define pam-u2f-service-type
  (service-type
   (name 'pam-u2f)
   (extensions
    (list (service-extension pam-root-service-type pam-u2f-pam-services)
          (service-extension activation-service-type pam-u2f-activation)
          (service-extension profile-service-type
                             (lambda (config)
                               (list (pam-u2f-configuration-pam-u2f config))))))
   (description
    "Authenticate with a FIDO2/U2F authenticator through pam_u2f, by default
as a sufficient method for @command{sudo}, and install the user-to-credential
mappings outside the store.")))


;;;
;;; pam_fprintd
;;;

(define-configuration/no-serialization pam-fprintd-configuration
  (fprintd
   (package fprintd)
   "The fprintd package, which also provides @command{fprintd-enroll}.  The
daemon itself comes from @code{fprintd-service-type} in
@code{(gnu services authentication)}, which must be present as well.")
  (pam-services
   (list-of-strings '("login" "greetd"))
   "PAM services whose auth stack gets the entry.  Screen lockers such as
swaylock have their own service name; add it here to unlock by fingerprint.")
  (control
   (string "sufficient")
   "PAM control flag."))

(define (pam-fprintd-pam-services config)
  (list (pam-auth-extension
         (pam-fprintd-configuration-pam-services config)
         (pam-entry
          (control (pam-fprintd-configuration-control config))
          (module (file-append (pam-fprintd-configuration-fprintd config)
                               "/lib/security/pam_fprintd.so"))))))

(define pam-fprintd-service-type
  (service-type
   (name 'pam-fprintd)
   (extensions
    (list (service-extension pam-root-service-type pam-fprintd-pam-services)
          (service-extension profile-service-type
                             (lambda (config)
                               (list (pam-fprintd-configuration-fprintd config))))))
   (default-value (pam-fprintd-configuration))
   (description
    "Authenticate with an enrolled fingerprint through pam_fprintd, by default
as a sufficient method for @code{login} and @code{greetd}.")))
