;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px home services wayvnc)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages tls)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (px packages vnc)
  #:export (home-wayvnc-configuration
            home-wayvnc-configuration?
            home-wayvnc-service-type))

;;;
;;; wayvnc - VNC server for wlroots-based Wayland compositors.
;;;
;;; The credentials live outside the store: it is world-readable, so a private
;;; key or a password placed there would be readable by every user on the
;;; machine.  They are generated on first activation instead, into a directory
;;; only the owner can enter, and the config file that points at them is
;;; written alongside them.
;;;

(define-maybe/no-serialization string)

(define (wayvnc-tls? value)
  (match value
    ('generate #t)
    (#f #t)
    (((? string?) . (? string?)) #t)
    (_ #f)))

(define (wayvnc-rsa-key? value)
  (or (eq? value 'generate)
      (eq? value #f)
      (string? value)))

(define-configuration/no-serialization home-wayvnc-configuration
  (wayvnc
   (package wayvnc-0.10)
   "The wayvnc package to use.")

  (openssl
   (package openssl)
   "The openssl package used to generate credentials.")

  (address
   (string "localhost")
   "Address to listen on.  Anything other than a loopback or overlay-network
address puts the session on the open network.")

  (port
   (integer 5900)
   "Port to listen on.")

  (enable-auth?
   (boolean #t)
   "Require clients to authenticate.")

  (enable-pam?
   (boolean #t)
   "Authenticate against PAM rather than a fixed password.  This needs
@code{wayvnc-pam-service-type} from @code{(px services vnc)} on the system
side, because wayvnc ships no PAM policy of its own and @file{/etc/pam.d} is
not something a home environment can write to.")

  (username
   (maybe-string)
   "User name clients must supply.  Ignored when @code{enable-pam?} is set,
since PAM resolves the name itself.")

  (password-file
   (maybe-string)
   "Path to a file whose first line is the password clients must supply.  The
password is copied into the wayvnc config file at activation time.  Ignored
when @code{enable-pam?} is set.")

  (tls
   (wayvnc-tls 'generate)
   "@code{'generate} to create a self-signed certificate on first activation,
a @code{(key-file . certificate-file)} pair to use your own, or @code{#f} to
run without TLS.  An existing generated certificate is never overwritten.")

  (tls-common-name
   (string "localhost")
   "Common name of the generated certificate.")

  (tls-subject-alt-names
   (list-of-strings '("DNS:localhost" "IP:127.0.0.1"))
   "Subject alternative names of the generated certificate.  Clients that
check the certificate need the address they connect to listed here.")

  (rsa-key
   (wayvnc-rsa-key 'generate)
   "@code{'generate} to create an RSA key on first activation, a path to use
your own, or @code{#f} for neither.  Leaving this unset does not disable
RSA-AES: neatvnc offers that security type regardless and falls back to a key
generated afresh on every start, which defeats the trust-on-first-use check
and makes clients warn about a changed host key each time.")

  (credentials-directory
   (string ".local/state/wayvnc")
   "Directory, relative to the home directory, holding the generated
credentials and the config file assembled from this configuration.")

  (desktop?
   (boolean #f)
   "Capture every output rather than a single one.")

  (output
   (maybe-string)
   "Name of the output to capture.")

  (seat
   (maybe-string)
   "Name of the seat to use for input.")

  (desktop-name
   (maybe-string)
   "Desktop name reported to clients.")

  (max-fps
   (integer 30)
   "Frame rate limit.")

  (gpu?
   (boolean #f)
   "Enable the features that need a GPU.")

  (render-cursor?
   (boolean #f)
   "Render the cursor into the frame instead of sending it separately.")

  (disable-input?
   (boolean #f)
   "Refuse all remote input, making the session view-only.")

  (detached?
   (boolean #f)
   "Start without a compositor and wait for @command{wayvncctl attach}.  Useful
when wayvnc starts before the Wayland session is up.")

  (log-level
   (string "warning")
   "One of @code{error}, @code{warning}, @code{info}, @code{debug},
@code{trace} or @code{quiet}.")

  (extra-config
   (list-of-strings '())
   "Additional lines appended verbatim to the wayvnc config file.")

  (extra-options
   (list-of-strings '())
   "Additional command line options passed to wayvnc.")

  (environment-variables
   (list-of-strings '())
   "@code{\"NAME=value\"} strings added to the environment wayvnc runs in.  The
Shepherd hands the service the environment it inherited at login, which has no
@env{WAYLAND_DISPLAY}; without one wayvnc falls back to @file{wayland-0} under
@env{XDG_RUNTIME_DIR}, so pin @code{\"WAYLAND_DISPLAY=wayland-1\"} here if your
compositor takes a different socket.")

  (auto-start?
   (boolean #f)
   "Start with the home Shepherd.  Off by default because wayvnc needs
@env{WAYLAND_DISPLAY} to point at a running compositor, which is usually not
true yet at login; start it from the compositor instead, or set
@code{detached?} and attach later with @command{wayvncctl attach}.

Reconfiguring rewrites the config file but wayvnc cannot reload it, so a
running server keeps the old settings until it is restarted."))

(define (home-wayvnc-config-file config)
  #~(string-append (getenv "HOME") "/"
                   #$(home-wayvnc-configuration-credentials-directory config)
                   "/config"))

(define (home-wayvnc-activation config)
  (match-record config <home-wayvnc-configuration>
    (openssl address port enable-auth? enable-pam? username password-file
     tls tls-common-name tls-subject-alt-names rsa-key credentials-directory
     extra-config)
    ;; wayvnc's own sanity check rejects this at startup; catching it here
    ;; turns a service that dies on first launch into a build error.
    (when (and enable-auth?
               (not enable-pam?)
               (not (maybe-value-set? password-file)))
      (error "home-wayvnc-configuration: authentication needs either \
'enable-pam?' or 'password-file'"))
    (with-imported-modules (source-module-closure
                            '((gnu build activation)
                              (guix build utils)))
      #~(begin
          (use-modules (gnu build activation)
                       (guix build utils)
                       (ice-9 rdelim))

          (let* ((home (getenv "HOME"))
                 (dir (string-append home "/" #$credentials-directory))
                 (tls-key #$(match tls
                              ('generate #~(string-append dir "/tls_key.pem"))
                              (#f #f)
                              ((key . _) key)))
                 (tls-cert #$(match tls
                               ('generate #~(string-append dir "/tls_cert.pem"))
                               (#f #f)
                               ((_ . cert) cert)))
                 (rsa #$(match rsa-key
                          ('generate #~(string-append dir "/rsa_key.pem"))
                          (#f #f)
                          (path path)))
                 (password #$(if (and (not enable-pam?)
                                      (maybe-value-set? password-file))
                                 #~(begin
                                     (unless (file-exists? #$password-file)
                                       (error "wayvnc: no such password file"
                                              #$password-file))
                                     (let ((line (call-with-input-file
                                                     #$password-file
                                                   read-line)))
                                       (when (eof-object? line)
                                         (error "wayvnc: password file is empty"
                                                #$password-file))
                                       line))
                                 #f))
                 (config-file (string-append dir "/config")))

            (mkdir-p/perms dir (getpw (getuid)) #o700)

            #$@(if (eq? tls 'generate)
                   #~((unless (file-exists? tls-key)
                        (invoke #$(file-append openssl "/bin/openssl")
                                "req" "-x509" "-newkey" "ec"
                                "-pkeyopt" "ec_paramgen_curve:secp384r1"
                                "-sha384" "-days" "3650" "-nodes"
                                "-keyout" tls-key "-out" tls-cert
                                "-subj" (string-append "/CN="
                                                       #$tls-common-name)
                                "-addext"
                                (string-append
                                 "subjectAltName="
                                 #$(string-join tls-subject-alt-names ",")))
                        (chmod tls-key #o600)
                        (chmod tls-cert #o644)))
                   #~())

            ;; neatvnc accepts PKCS#1 only, which is what -traditional selects;
            ;; plain 'genrsa' emits PKCS#8 on OpenSSL 3 and fails to load.
            #$@(if (eq? rsa-key 'generate)
                   #~((unless (file-exists? rsa)
                        (invoke #$(file-append openssl "/bin/openssl")
                                "genrsa" "-traditional" "-out" rsa "2048")
                        (chmod rsa #o600)))
                   #~())

            (umask #o077)
            (call-with-output-file config-file
              (lambda (port)
                (for-each (lambda (line)
                            (display line port)
                            (newline port))
                          (append
                           (list (string-append "address=" #$address)
                                 (string-append "port="
                                                (number->string #$port)))
                           (if #$enable-auth? '("enable_auth=true") '())
                           (if #$enable-pam? '("enable_pam=true") '())
                           #$(if (and (not enable-pam?)
                                      (maybe-value-set? username))
                                 #~(list (string-append "username=" #$username))
                                 #~'())
                           (if password
                               (list (string-append "password=" password))
                               '())
                           (if tls-key
                               (list (string-append "private_key_file="
                                                    tls-key)
                                     (string-append "certificate_file="
                                                    tls-cert))
                               '())
                           (if rsa
                               (list (string-append "rsa_private_key_file="
                                                    rsa))
                               '())
                           (list #$@extra-config)))))
            (chmod config-file #o600))))))

(define (home-wayvnc-shepherd-service config)
  (match-record config <home-wayvnc-configuration>
    (wayvnc desktop? output seat desktop-name max-fps gpu? render-cursor?
     disable-input? detached? log-level extra-options environment-variables
     auto-start?)
    (let ((flags (append (if desktop? '("--desktop") '())
                         (if gpu? '("--gpu") '())
                         (if render-cursor? '("--render-cursor") '())
                         (if disable-input? '("--disable-input") '())
                         (if detached? '("--detached") '())
                         (if (maybe-value-set? output)
                             (list "--output" output)
                             '())
                         (if (maybe-value-set? seat)
                             (list "--seat" seat)
                             '())
                         (if (maybe-value-set? desktop-name)
                             (list "--name" desktop-name)
                             '())
                         extra-options)))
      (list
       (shepherd-service
        (documentation "Run wayvnc, a VNC server for the Wayland session.")
        (provision '(wayvnc))
        (requirement '())
        (modules '((shepherd support)))      ;for '%user-log-dir'
        (start #~(make-forkexec-constructor
                  (list #$(file-append wayvnc "/bin/wayvnc")
                        "--config" #$(home-wayvnc-config-file config)
                        "--log-level" #$log-level
                        "--max-fps" #$(number->string max-fps)
                        #$@flags)
                  #:environment-variables
                  (append (list #$@environment-variables) (environ))
                  #:log-file (string-append %user-log-dir "/wayvnc.log")))
        (stop #~(make-kill-destructor))
        (auto-start? auto-start?))))))

(define (home-wayvnc-profile config)
  (list (home-wayvnc-configuration-wayvnc config)))

(define home-wayvnc-service-type
  (service-type
   (name 'home-wayvnc)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-wayvnc-shepherd-service)
          (service-extension home-activation-service-type
                             home-wayvnc-activation)
          (service-extension home-profile-service-type
                             home-wayvnc-profile)))
   (default-value (home-wayvnc-configuration))
   (description
    "Share the Wayland session over VNC with @command{wayvnc}.  Credentials
are generated on first activation into a directory only the owner can enter,
and the running server is controlled with @command{wayvncctl}.")))
