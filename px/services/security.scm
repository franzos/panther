;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px services security)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gawk)
  #:use-module ((gnu packages linux)
                #:select (util-linux procps kmod iproute nftables))
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (aide-configuration
            aide-configuration?
            aide-configuration-aide
            aide-configuration-rules
            aide-configuration-schedule
            aide-configuration-state-directory
            aide-configuration-log-file
            aide-service-type
            %default-aide-rules

            lynis-configuration
            lynis-configuration?
            lynis-configuration-lynis
            lynis-configuration-schedule
            lynis-configuration-state-directory
            lynis-configuration-log-file
            lynis-service-type))

;;;
;;; Shared: both tools are shell scripts driven by a Shepherd timer. Lynis
;;; exits fatal without awk, find, ps or sed and silently skips tests when
;;; sysctl or nft are missing, so the PATH is explicit rather than inherited.
;;;

(define %security-path
  #~(string-join
     (list #$(file-append coreutils "/bin")
           #$(file-append bash "/bin")
           #$(file-append grep "/bin")
           #$(file-append sed "/bin")
           #$(file-append gawk "/bin")
           #$(file-append findutils "/bin")
           #$(file-append diffutils "/bin")
           #$(file-append git "/bin")
           #$(file-append procps "/bin")
           #$(file-append procps "/sbin")
           #$(file-append kmod "/bin")
           #$(file-append openssl "/bin")
           #$(file-append lsof "/bin")
           #$(file-append util-linux "/bin")
           #$(file-append util-linux "/sbin")
           #$(file-append inetutils "/bin")
           #$(file-append nftables "/sbin")
           #$(file-append iproute "/sbin"))
     ":"))

;; A program that sets PATH and ENVIRONMENT (an alist of name and value,
;; values may be file-likes) and then runs SCRIPT through bash.
(define (security-program name tool script environment)
  (program-file name
    #~(begin
        (setenv "PATH"
                (string-append #$(file-append tool "/bin") ":" #$%security-path))
        #$@(map (match-lambda
                  ((variable . value)
                   #~(setenv #$variable #$value)))
                environment)
        (apply execl #$(file-append bash "/bin/bash") "bash" #$script
               (cdr (command-line))))))

(define (timer-service name documentation schedule program)
  (shepherd-service
   (provision (list name))
   (requirement '(user-processes))
   (modules '((shepherd service timer)))
   (documentation documentation)
   (start #~(make-timer-constructor
             (cron-string->calendar-event #$schedule)
             (command (list #$program))
             #:wait-for-termination? #t))
   (stop #~(make-timer-destructor))
   (actions (list shepherd-trigger-action))))


;;;
;;; AIDE
;;;

(define %default-aide-rules
  (local-file "aux-files/aide.rules"))

(define-configuration/no-serialization aide-configuration
  (aide
   (package aide)
   "The AIDE package to use.")
  (rules
   (file-like %default-aide-rules)
   "File holding the rule groups and the rules, in AIDE's configuration
syntax.  It is included from a generated @file{aide.conf} that sets
@code{database_in}, @code{database_out} and @code{report_url}, so the file
should not set those itself.  The default watches @file{/etc}, @file{/boot}
and every user's @file{.ssh} and @file{.gnupg}, and drops the store symlinks
under @file{/etc} that change on every reconfigure.")
  (schedule
   (string "0 3 * * *")
   "When to run the check, in cron syntax.  @command{herd trigger aide} runs
it immediately.")
  (state-directory
   (string "/var/lib/aide")
   "Where the baseline database lives.  The directory is a git repository, so
every baseline the host ever accepted stays on record.")
  (log-file
   (string "/var/log/aide.log")
   "Where check results and diffs are appended."))

(define (aide-config-file config)
  (let ((state (aide-configuration-state-directory config)))
    (mixed-text-file "aide.conf"
      "database_in=file:" state "/aide.db\n"
      "database_out=file:" state "/aide.db.new\n"
      "report_url=stdout\n"
      "@@include " (aide-configuration-rules config) "\n")))

(define (aide-environment config)
  `(("AIDE_CONF" . ,(aide-config-file config))
    ("AIDE_STATE" . ,(aide-configuration-state-directory config))
    ("AIDE_LOG" . ,(aide-configuration-log-file config))))

(define (aide-check-program config)
  (security-program "aide-check" (aide-configuration-aide config)
                    (local-file "aux-files/aide-check")
                    (aide-environment config)))

(define (aide-accept-program config)
  (security-program "aide-accept" (aide-configuration-aide config)
                    (local-file "aux-files/aide-accept")
                    (aide-environment config)))

;; Puts `aide-accept' on PATH: the baseline is never updated by the timer, a
;; human reviews the diff and accepts it.
(define (aide-accept-package config)
  (package
    (name "aide-accept")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:builder
           #~(begin
               (mkdir #$output)
               (mkdir (string-append #$output "/bin"))
               (symlink #$(aide-accept-program config)
                        (string-append #$output "/bin/aide-accept")))))
    (home-page "https://aide.github.io/")
    (synopsis "Accept the current filesystem state as the AIDE baseline")
    (description "Rebuilds the AIDE database from the current filesystem
state and commits it to the baseline repository.")
    (license license:gpl3+)))

(define (aide-shepherd-services config)
  (list (timer-service 'aide "Run the AIDE file integrity check."
                       (aide-configuration-schedule config)
                       (aide-check-program config))))

(define aide-service-type
  (service-type
   (name 'aide)
   (extensions
    (list (service-extension shepherd-root-service-type
                             aide-shepherd-services)
          (service-extension profile-service-type
                             (lambda (config)
                               (list (aide-accept-package config))))
          (service-extension log-rotation-service-type
                             (lambda (config)
                               (list (aide-configuration-log-file config))))))
   (default-value (aide-configuration))
   (description
    "Run AIDE on a schedule, keep its baseline in a git repository, log every
diff, and provide @command{aide-accept} to adopt a reviewed state as the new
baseline.")))


;;;
;;; Lynis
;;;

(define-configuration/no-serialization lynis-configuration
  (lynis
   (package lynis)
   "The Lynis package to use.")
  (schedule
   (string "30 3 * * 1")
   "When to run the audit, in cron syntax.  @command{herd trigger lynis} runs
it immediately.")
  (state-directory
   (string "/var/lib/lynis")
   "Where the current and previous machine-readable reports and Lynis's own
log are kept.")
  (log-file
   (string "/var/log/lynis.log")
   "Where the summary and the delta against the previous run are appended."))

(define (lynis-audit-program config)
  (security-program "lynis-audit" (lynis-configuration-lynis config)
                    (local-file "aux-files/lynis-audit")
                    `(("LYNIS_STATE" . ,(lynis-configuration-state-directory config))
                      ("LYNIS_LOG" . ,(lynis-configuration-log-file config)))))

(define (lynis-shepherd-services config)
  (list (timer-service 'lynis "Run the Lynis system audit."
                       (lynis-configuration-schedule config)
                       (lynis-audit-program config))))

(define lynis-service-type
  (service-type
   (name 'lynis)
   (extensions
    (list (service-extension shepherd-root-service-type
                             lynis-shepherd-services)
          (service-extension log-rotation-service-type
                             (lambda (config)
                               (list (lynis-configuration-log-file config))))))
   (default-value (lynis-configuration))
   (description
    "Run a Lynis system audit on a schedule and log the hardening index,
the warnings and suggestions, and the delta against the previous run.")))
