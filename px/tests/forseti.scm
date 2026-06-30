;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

;; Marionette VM system test for the Forseti identity stack. Boots a Guix
;; System with `forseti-stack-services' (dev? #t), Postgres on unix-socket peer
;; auth with per-daemon roles, and asserts the whole chain comes up and wires
;; together: the role one-shot + both Ory migrations complete, the three
;; daemons run, Hydra's discovery issuer matches base-url, the admin API
;; answers, both health endpoints are ok, and a real OAuth2 authorize request
;; redirects to Forseti's login URL.
;;
;; Run it (KVM if /dev/kvm exists, else TCG):
;;
;;   guix build -L /home/franz/git/panther -L /home/franz/git/nonguix \
;;     -e '(@ (px tests forseti) %test-forseti-stack)'

(define-module (px tests forseti)
  #:use-module (px services forseti)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:export (%test-forseti-stack))

(define %base-url "https://id.example.com")

;; Test secrets baked into the store (fine for a test). Kratos cipher MUST be
;; exactly 32 bytes; system/cookie secrets >= 16 bytes.
(define %kratos-env
  (plain-file "kratos.env"
              (string-append
               "SECRETS_COOKIE=test-cookie-secret-0123456789\n"
               "SECRETS_CIPHER=0123456789abcdef0123456789abcdef\n")))

(define %hydra-env
  (plain-file "hydra.env"
              "SECRETS_SYSTEM=test-system-secret-0123456789\n"))

(define %forseti-env
  ;; Cookie secret needs >= 32 bytes for the HMAC key. The audit webhook token
  ;; is required (figment nests with a double underscore).
  (plain-file "forseti.env"
              (string-append
               "FORSETI_AUDIT__WEBHOOK_TOKEN=test-audit-token-0123456789\n"
               ;; Decoded to >= 32 bytes: 64 hex chars = 32 bytes.
               "FORSETI_SECURITY__COOKIE_SECRET="
               "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\n")))

(define %forseti-stack-os
  ;; forseti-stack-services returns a LIST, so splice it into the service list
  ;; rather than passing it to the simple-operating-system macro (which wants
  ;; individual services).
  (operating-system
    (inherit %simple-os)
    (services
     (append
      (list (service dhcpcd-service-type)
            ;; Operator-supplied env-files; the start wrappers source these.
            (extra-special-file "/etc/forseti/kratos.env" %kratos-env)
            (extra-special-file "/etc/forseti/hydra.env" %hydra-env)
            (extra-special-file "/etc/forseti/forseti.env" %forseti-env))
      (forseti-stack-services
       (forseti-stack-configuration
        (base-url %base-url)
        (dev? #t)
        ;; No webhook so kratos.yml does not reference an extra token.
        (audit-webhook? #f)))
      %base-services))))

(define (run-forseti-stack-test)
  (define os
    (marionette-operating-system
     %forseti-stack-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     ;; Postgres + 3 Go daemons need headroom.
     (memory-size 2560)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (srfi srfi-13)
                       (gnu build marionette)
                       (ice-9 popen)
                       (ice-9 textual-ports))

          (define marionette
            (make-marionette (list #$vm)))

          ;; Run a shell command in the guest, return (exit . stdout) with
          ;; stderr merged in.
          (define (guest command-string)
            (marionette-eval
             `(begin
                (use-modules (ice-9 popen) (ice-9 textual-ports))
                (let* ((port (open-pipe ,command-string OPEN_READ))
                       (out (get-string-all port))
                       (st (close-pipe port)))
                  (cons (status:exit-val st) out)))
             marionette))

          (define curl #$(file-append (@ (gnu packages curl) curl) "/bin/curl"))

          ;; Poll an HTTP endpoint until curl gets a response or the cap is hit.
          (define (wait-for-http url tries)
            (let loop ((i tries))
              (let ((r (guest (string-append curl " -s -o /dev/null -w %{http_code} "
                                             url))))
                (cond
                 ((and (eqv? 0 (car r))
                       (not (string=? "000" (string-trim-right (cdr r)))))
                  #t)
                 ((> i 0) (sleep 1) (loop (- i 1)))
                 (else #f)))))

          ;; Read herd status for a service; #t when it parses as a record.
          (define (herd-running? svc)
            (marionette-eval
             `(begin
                (use-modules (gnu services herd) (srfi srfi-1))
                (let ((s (find (lambda (s)
                                 (memq ',svc (live-service-provision s)))
                               (current-services))))
                  (and s (live-service-running s) #t)))
             marionette))

          (test-runner-current (system-test-runner #$output))
          (test-begin "forseti-stack")

          ;; --- (1) postgres + role + migrations -------------------------------
          (test-assert "postgres service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'postgres))
             marionette))

          ;; postgres-roles is a one-shot: starting it runs the role/db creation
          ;; (CREATE ROLE kratos/hydra/forseti + CREATE DATABASE) and exits 0.
          (test-assert "postgres-roles one-shot completes"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'postgres-roles))
             marionette))

          ;; The migrate one-shots require postgres-roles; starting them runs the
          ;; Ory `migrate sql' against the peer-auth socket and exits 0.
          (test-assert "kratos-migrate one-shot exits 0"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'kratos-migrate))
             marionette))

          (test-assert "hydra-migrate one-shot exits 0"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'hydra-migrate))
             marionette))

          ;; Ory tables landed in the peer-auth databases. Run psql as the
          ;; `postgres' superuser (peer auth) in a FORKED child so the
          ;; marionette REPL keeps its own (root) identity: a setuid in the REPL
          ;; itself is irreversible and would break every later root operation.
          ;; Run psql in a forked child as the `postgres' superuser (peer auth),
          ;; capturing stdout+stderr. Returns the output string.
          (define (psql db sql)
            (marionette-eval
             `(begin
                (use-modules (ice-9 textual-ports))
                (let ((out "/tmp/psql-out")
                      (pw (getpwnam "postgres")))
                  (let ((pid (primitive-fork)))
                    (if (zero? pid)
                        (dynamic-wind
                          (const #t)
                          (lambda ()
                            (let ((fd (open-fdes out (logior O_WRONLY O_CREAT O_TRUNC)
                                                 #o644)))
                              (dup2 fd 1) (dup2 fd 2))
                            (setgid (passwd:gid pw))
                            (setuid (passwd:uid pw))
                            (execl ,#$(file-append
                                       (@ (gnu packages databases) postgresql)
                                       "/bin/psql")
                                   "psql" "-tA" "-d" ,db "-c" ,sql))
                          (lambda () (primitive-exit 1)))
                        (begin
                          (waitpid pid)
                          (call-with-input-file out get-string-all))))))
             marionette))

          ;; Poll for a table: the first forked psql after boot can race the
          ;; socket warm-up, and a migration may still be settling, so retry.
          (define (table-exists? db table)
            (let loop ((i 10))
              (cond
               ((string-contains
                 (psql db (string-append
                           "SELECT 1 FROM information_schema.tables "
                           "WHERE table_schema='public' AND table_name='" table "'"))
                 "1")
                #t)
               ((> i 0) (sleep 1) (loop (- i 1)))
               (else #f))))

          ;; Concrete proof that the peer-auth migration path writes to the
          ;; per-daemon Postgres database: hydra-migrate (no --dev) populated
          ;; the hydra DB over the unix socket as the `hydra' role. (The
          ;; kratos-migrate one-shot success + kratos health are asserted
          ;; separately; under kratos v26 + --dev its Postgres database is not
          ;; left populated the way hydra's is.)
          (test-assert "hydra schema present in hydra db"
            (table-exists? "hydra" "hydra_client"))

          ;; --- (2) the three daemons are running ------------------------------
          ;; Ask shepherd to start each serve daemon. start-service returns #f
          ;; for an already-running (auto-started) service, so we do not assert
          ;; on its return; readiness is proven by the port waits + herd status
          ;; below. Dump each daemon log to the console for diagnostics.
          (define (try-start svc)
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (start-service ',svc))
             marionette))

          ;; Read a daemon log tail as root and surface it host-side via pk so
          ;; failures are diagnosable in the captured SRFI-64 test log.
          (define (dump-log label path)
            (pk label
                (marionette-eval
                 `(begin
                    (use-modules (ice-9 textual-ports))
                    (when (file-exists? ,path)
                      (false-if-exception (chmod ,path #o644)))
                    (if (file-exists? ,path)
                        (let ((s (false-if-exception
                                  (call-with-input-file ,path get-string-all))))
                          (cond
                           ((not (string? s)) "<unreadable>")
                           ((> (string-length s) 4000)
                            (substring s (- (string-length s) 4000)))
                           (else s)))
                        "<no log file>"))
                 marionette)))

          (try-start 'hydra)
          (try-start 'kratos)
          (try-start 'forseti)
          (sleep 5)
          (dump-log 'hydra-log "/var/log/hydra.log")
          (dump-log 'kratos-log "/var/log/kratos.log")
          (dump-log 'forseti-log "/var/log/forseti.log")

          (test-assert "wait for hydra public port 4444"
            (wait-for-tcp-port 4444 marionette))
          (test-assert "wait for hydra admin port 4445"
            (wait-for-tcp-port 4445 marionette))
          (test-assert "wait for kratos public port 4433"
            (wait-for-tcp-port 4433 marionette))
          (test-assert "wait for forseti public port 3000"
            (wait-for-tcp-port 3000 marionette))

          (test-assert "herd reports hydra running" (herd-running? 'hydra))
          (test-assert "herd reports kratos running" (herd-running? 'kratos))
          (test-assert "herd reports forseti running" (herd-running? 'forseti))

          ;; --- (3) hydra discovery issuer == base-url -------------------------
          (test-assert "wait for hydra discovery"
            (wait-for-http
             "http://localhost:4444/.well-known/openid-configuration" 30))

          (test-equal "openid-configuration issuer matches base-url"
            (string-append #$%base-url "/hydra")
            (let* ((r (guest (string-append
                              curl " -s "
                              "http://localhost:4444/.well-known/openid-configuration")))
                   (body (cdr r))
                   ;; tiny field extractor: find "issuer":"<value>".
                   (needle "\"issuer\":\"")
                   (i (string-contains body needle)))
              (and i
                   (let* ((start (+ i (string-length needle)))
                          (end (string-index body #\" start)))
                     (substring body start end)))))

          ;; --- (4) hydra admin clients endpoint answers 200 -------------------
          (test-equal "hydra admin /admin/clients is 200"
            "200"
            (string-trim-right
             (cdr (guest (string-append
                          curl " -s -o /dev/null -w %{http_code} "
                          "http://localhost:4445/admin/clients")))))

          ;; --- (5) health endpoints -------------------------------------------
          (test-assert "kratos /health/ready reports ok"
            (string-contains
             (cdr (guest (string-append curl " -s "
                                        "http://localhost:4433/health/ready")))
             "\"status\":\"ok\""))

          (test-assert "forseti /healthz returns ok"
            (string-contains
             (cdr (guest (string-append curl " -s "
                                        "http://localhost:3000/healthz")))
             "ok"))

          ;; --- (6) wiring proof: authorize redirects to Forseti login ---------
          ;; Register an OAuth2 client via the hydra admin API, then drive an
          ;; authorize request and assert the 302 Location points at the
          ;; Forseti login URL. A full token exchange would require automating
          ;; the Kratos login UI and is OUT OF SCOPE; we assert redirect wiring
          ;; only.
          (define client-id
            (let* ((r (guest (string-append
                              curl " -s -X POST "
                              "http://localhost:4445/admin/clients "
                              "-H 'Content-Type: application/json' "
                              "-d '{\"client_name\":\"test\","
                              "\"grant_types\":[\"authorization_code\"],"
                              "\"response_types\":[\"code\"],"
                              "\"scope\":\"openid\","
                              "\"redirect_uris\":[\"https://app.example.com/callback\"],"
                              "\"token_endpoint_auth_method\":\"none\"}'")))
                   (body (cdr r))
                   (needle "\"client_id\":\"")
                   (i (string-contains body needle)))
              (and i
                   (let* ((start (+ i (string-length needle)))
                          (end (string-index body #\" start)))
                     (substring body start end)))))

          (test-assert "client registration returned a client_id"
            (and (string? client-id) (> (string-length client-id) 0)))

          (test-assert "authorize redirects to the Forseti login URL"
            (let* ((url (string-append
                         "http://localhost:4444/oauth2/auth"
                         "?client_id=" client-id
                         "&response_type=code&scope=openid"
                         "&redirect_uri=https://app.example.com/callback"
                         "&state=teststate123"))
                   ;; -D- dumps response headers; do not follow redirects.
                   (r (guest (string-append curl " -s -o /dev/null -D- "
                                            "'" url "'")))
                   (headers (cdr r)))
              (and (or (string-contains headers " 302")
                       (string-contains headers " 303"))
                   (string-contains headers (string-append #$%base-url
                                                           "/oauth/login")))))

          (test-end))))

  (gexp->derivation "forseti-stack-test" test))

(define %test-forseti-stack
  (system-test
   (name "forseti-stack")
   (description
    "Boot a Guix System running the Forseti identity stack (Ory Kratos, Ory
Hydra, Forseti) on Postgres unix-socket peer authentication with per-daemon
roles, in dev mode, then assert the role + migration one-shots complete, the
three daemons run, Hydra's discovery issuer matches base-url, the admin API
answers, both health endpoints are ok, and an OAuth2 authorize request
redirects to the Forseti login URL.")
   (value (run-forseti-stack-test))))
