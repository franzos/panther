;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

;; Forseti identity stack: Ory Kratos (identity), Ory Hydra (OAuth2/OIDC),
;; and Forseti (the user-facing portal that drives both).
;;
;; Deployment shape: all three daemons serve PLAINTEXT HTTP on loopback. A
;; TLS-terminating reverse proxy fronts the single public origin `base-url`
;; and routes browser traffic to Forseti's public port (3000). Hydra public
;; (4444) and Kratos public (4433) are reached server-to-server; the admin
;; ports (4445 / 4434) and Forseti's internal machine-to-machine listener
;; (8081) MUST NOT be exposed to the public interface.
;;
;; Secrets never go into the world-readable store. Each service reads its
;; DSN, system/cookie/cipher secrets, the audit bearer token, and any SMTP
;; password from an operator-supplied environment-file under
;; `secrets-directory` (default /etc/forseti, root-owned 0600). The shepherd
;; start wrapper sources that file before exec. Non-secret env (URLs, log
;; level) is passed directly via #:environment-variables.
;;
;; The Kratos identity schema is embedded in the generated kratos.yml as a
;; base64 data: URL so no separate schema file has to be deployed.

(define-module (px services forseti)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services databases)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module ((px packages authentication) #:prefix pkg:)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)

  #:export (ory-kratos-configuration
            ory-kratos-configuration?
            ory-kratos-configuration-package
            ory-kratos-configuration-config-file
            ory-kratos-configuration-environment-variables
            ory-kratos-configuration-environment-file
            ory-kratos-configuration-user
            ory-kratos-configuration-group
            ory-kratos-configuration-log-file
            ory-kratos-configuration-state-directory
            ory-kratos-configuration-requirement
            ory-kratos-configuration-auto-migrate?
            ory-kratos-configuration-dev?
            ory-kratos-service-type

            ory-hydra-configuration
            ory-hydra-configuration?
            ory-hydra-configuration-package
            ory-hydra-configuration-config-file
            ory-hydra-configuration-environment-variables
            ory-hydra-configuration-environment-file
            ory-hydra-configuration-user
            ory-hydra-configuration-group
            ory-hydra-configuration-log-file
            ory-hydra-configuration-state-directory
            ory-hydra-configuration-requirement
            ory-hydra-configuration-auto-migrate?
            ory-hydra-configuration-dev?
            ory-hydra-service-type

            forseti-configuration
            forseti-configuration?
            forseti-configuration-package
            forseti-configuration-config-file
            forseti-configuration-environment-variables
            forseti-configuration-environment-file
            forseti-configuration-user
            forseti-configuration-group
            forseti-configuration-log-file
            forseti-configuration-state-directory
            forseti-configuration-requirement
            forseti-service-type

            forseti-stack-configuration
            forseti-stack-configuration?
            forseti-stack-configuration-base-url
            forseti-stack-configuration-kratos
            forseti-stack-configuration-hydra
            forseti-stack-configuration-forseti
            forseti-stack-configuration-manage-postgres?
            forseti-stack-configuration-postgres-host
            forseti-stack-configuration-secrets-directory
            forseti-stack-configuration-smtp-connection-uri
            forseti-stack-configuration-enable-dynamic-client-registration?
            forseti-stack-configuration-audit-webhook?
            forseti-stack-configuration-dev?
            forseti-stack-configuration-kratos-log-file
            forseti-stack-configuration-hydra-log-file
            forseti-stack-configuration-forseti-log-file
            forseti-stack-services))

;;
;; Fixed ports for the stack. Local plaintext only.
;;

(define %hydra-public 4444)
(define %hydra-admin 4445)
(define %kratos-public 4433)
(define %kratos-admin 4434)
(define %forseti-public 3000)
(define %forseti-internal 8081)             ;NEVER expose

;;
;; A small shepherd start wrapper: source the env-file (if present) so
;; secrets stay out of the store, optionally cd into a state directory, then
;; exec the daemon. Returns a file-like.
;;

(define* (env-exec-wrapper name env-file command #:key (workdir #f))
  (program-file
   name
   #~(begin
       (let* ((args (cdr (command-line)))
              (env-file #$env-file)
              (sh "/run/current-system/profile/bin/bash"))
         #$@(if workdir
                (list #~(begin (chdir #$workdir)))
                '())
         ;; `set -a` exports everything the env-file defines; secrets stay in
         ;; the root-owned 0600 file, never in the store.
         (if (and env-file (file-exists? env-file))
             (apply execl sh sh "-c"
                    (string-append "set -a; . " env-file
                                   "; set +a; exec \"$@\"")
                    "--" #$@command args)
             (apply execl #$(car command) #$(car command)
                    (append (list #$@(cdr command)) args)))))))

;;
;; ORY KRATOS
;;

(define-record-type* <ory-kratos-configuration>
  ory-kratos-configuration make-ory-kratos-configuration
  ory-kratos-configuration?
  (package
   ory-kratos-configuration-package
   (default pkg:ory-kratos))
  (config-file
   ory-kratos-configuration-config-file)
  (environment-variables
   ory-kratos-configuration-environment-variables
   (default '()))
  (environment-file
   ory-kratos-configuration-environment-file
   (default "/etc/forseti/kratos.env"))
  (user
   ory-kratos-configuration-user
   (default "ory"))
  (group
   ory-kratos-configuration-group
   (default "ory"))
  (log-file
   ory-kratos-configuration-log-file
   (default "/var/log/kratos.log"))
  (state-directory
   ory-kratos-configuration-state-directory
   (default "/var/lib/ory-kratos"))
  (requirement
   ory-kratos-configuration-requirement
   (default '(postgres)))
  (auto-migrate?
   ory-kratos-configuration-auto-migrate?
   (default #t))
  ;; Pass --dev to the serve command (NOT migrate) so Hydra/Kratos relax the
  ;; https-issuer-over-http boot check; production uses TLS-termination config.
  (dev?
   ory-kratos-configuration-dev?
   (default #f)))

(define-record-type* <ory-hydra-configuration>
  ory-hydra-configuration make-ory-hydra-configuration
  ory-hydra-configuration?
  (package
   ory-hydra-configuration-package
   (default pkg:ory-hydra))
  (config-file
   ory-hydra-configuration-config-file)
  (environment-variables
   ory-hydra-configuration-environment-variables
   (default '()))
  (environment-file
   ory-hydra-configuration-environment-file
   (default "/etc/forseti/hydra.env"))
  (user
   ory-hydra-configuration-user
   (default "ory"))
  (group
   ory-hydra-configuration-group
   (default "ory"))
  (log-file
   ory-hydra-configuration-log-file
   (default "/var/log/hydra.log"))
  (state-directory
   ory-hydra-configuration-state-directory
   (default "/var/lib/ory-hydra"))
  (requirement
   ory-hydra-configuration-requirement
   (default '(postgres)))
  (auto-migrate?
   ory-hydra-configuration-auto-migrate?
   (default #t))
  ;; Pass --dev to the serve command (NOT migrate); see kratos above.
  (dev?
   ory-hydra-configuration-dev?
   (default #f)))

(define-record-type* <forseti-configuration>
  forseti-configuration make-forseti-configuration
  forseti-configuration?
  (package
   forseti-configuration-package
   (default pkg:forseti))
  (config-file
   forseti-configuration-config-file)
  (environment-variables
   forseti-configuration-environment-variables
   (default '()))
  (environment-file
   forseti-configuration-environment-file
   (default "/etc/forseti/forseti.env"))
  (user
   forseti-configuration-user
   (default "forseti"))
  (group
   forseti-configuration-group
   (default "forseti"))
  (log-file
   forseti-configuration-log-file
   (default "/var/log/forseti.log"))
  (state-directory
   forseti-configuration-state-directory
   (default "/var/lib/forseti"))
  (requirement
   forseti-configuration-requirement
   (default '(hydra kratos postgres))))

;;
;; Shared account / activation builders. Both Ory daemons share the "ory"
;; user; Forseti runs as "forseti".
;;

(define (ory-system-account user group home)
  (list (user-group
         (name group)
         (system? #t))
        (user-account
         (name user)
         (group group)
         (system? #t)
         (comment "Ory/Forseti daemon user")
         (home-directory home)
         (shell (file-append shadow "/sbin/nologin")))))

(define (state+log-activation user state-directory log-file)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let ((pw (getpwnam #$user)))
          (mkdir-p #$state-directory)
          (chown #$state-directory (passwd:uid pw) (passwd:gid pw))
          (chmod #$state-directory #o750)
          (mkdir-p (dirname #$log-file))))))

;;
;; Kratos shepherd services: a one-shot `migrate sql` and the long-running
;; `serve`.
;;

(define (ory-kratos-shepherd-service config)
  (match-record config <ory-kratos-configuration>
    (package config-file environment-variables environment-file
     user group log-file state-directory requirement auto-migrate? dev?)
    (let ((kratos (file-append package "/bin/kratos"))
          (non-secret-env
           #~(list #$@(map (lambda (pair)
                             (string-append (car pair) "=" (cdr pair)))
                           environment-variables)))
          (migrate-prov '(kratos-migrate)))
      (list
       (shepherd-service
        (documentation "Run the Ory Kratos database migration (one-shot).")
        (provision migrate-prov)
        (requirement (append requirement '(user-processes)))
        (one-shot? #t)
        (start #~(make-forkexec-constructor
                  (list #$(env-exec-wrapper
                           "kratos-migrate-wrapper" environment-file
                           (list kratos "migrate" "sql" "-e" "--yes"
                                 "-c" config-file)))
                  #:user #$user
                  #:group #$group
                  #:log-file #$log-file
                  #:environment-variables
                  (append #$non-secret-env (default-environment-variables))))
        (stop #~(make-kill-destructor)))
       (shepherd-service
        (documentation "Run the Ory Kratos identity server.")
        (provision '(kratos))
        (requirement (append requirement
                             (if auto-migrate? migrate-prov '())
                             '(user-processes)))
        (start #~(make-forkexec-constructor
                  (list #$(env-exec-wrapper
                           "kratos-serve-wrapper" environment-file
                           (append (list kratos "serve" "-c" config-file)
                                   (if dev? '("--dev") '()))))
                  #:user #$user
                  #:group #$group
                  #:log-file #$log-file
                  #:environment-variables
                  (append #$non-secret-env (default-environment-variables))))
        (stop #~(make-kill-destructor #:grace-period 10)))))))

(define (ory-kratos-accounts config)
  (match-record config <ory-kratos-configuration> (user group state-directory)
    (ory-system-account user group state-directory)))

(define (ory-kratos-activation config)
  (match-record config <ory-kratos-configuration> (user state-directory log-file)
    (state+log-activation user state-directory log-file)))

(define (ory-kratos-log-rotations config)
  (list (ory-kratos-configuration-log-file config)))

(define ory-kratos-service-type
  (service-type
   (name 'ory-kratos)
   (description "Run and configure the Ory Kratos identity server.")
   (extensions
    (list (service-extension activation-service-type
                             ory-kratos-activation)
          (service-extension account-service-type
                             ory-kratos-accounts)
          (service-extension shepherd-root-service-type
                             ory-kratos-shepherd-service)
          (service-extension log-rotation-service-type
                             ory-kratos-log-rotations)))
   (default-value #f)))

;;
;; Hydra shepherd services.
;;

(define (ory-hydra-shepherd-service config)
  (match-record config <ory-hydra-configuration>
    (package config-file environment-variables environment-file
     user group log-file state-directory requirement auto-migrate? dev?)
    (let ((hydra (file-append package "/bin/hydra"))
          (non-secret-env
           #~(list #$@(map (lambda (pair)
                             (string-append (car pair) "=" (cdr pair)))
                           environment-variables)))
          (migrate-prov '(hydra-migrate)))
      (list
       (shepherd-service
        (documentation "Run the Ory Hydra database migration (one-shot).")
        (provision migrate-prov)
        (requirement (append requirement '(user-processes)))
        (one-shot? #t)
        (start #~(make-forkexec-constructor
                  (list #$(env-exec-wrapper
                           "hydra-migrate-wrapper" environment-file
                           (list hydra "migrate" "sql" "-e" "--yes"
                                 "-c" config-file)))
                  #:user #$user
                  #:group #$group
                  #:log-file #$log-file
                  #:environment-variables
                  (append #$non-secret-env (default-environment-variables))))
        (stop #~(make-kill-destructor)))
       (shepherd-service
        (documentation "Run the Ory Hydra OAuth2/OIDC server.")
        (provision '(hydra))
        (requirement (append requirement
                             (if auto-migrate? migrate-prov '())
                             '(user-processes)))
        (start #~(make-forkexec-constructor
                  (list #$(env-exec-wrapper
                           "hydra-serve-wrapper" environment-file
                           (append (list hydra "serve" "all" "-c" config-file)
                                   (if dev? '("--dev") '()))))
                  #:user #$user
                  #:group #$group
                  #:log-file #$log-file
                  #:environment-variables
                  (append #$non-secret-env (default-environment-variables))))
        (stop #~(make-kill-destructor #:grace-period 10)))))))

(define (ory-hydra-accounts config)
  (match-record config <ory-hydra-configuration> (user group state-directory)
    (ory-system-account user group state-directory)))

(define (ory-hydra-activation config)
  (match-record config <ory-hydra-configuration> (user state-directory log-file)
    (state+log-activation user state-directory log-file)))

(define (ory-hydra-log-rotations config)
  (list (ory-hydra-configuration-log-file config)))

(define ory-hydra-service-type
  (service-type
   (name 'ory-hydra)
   (description "Run and configure the Ory Hydra OAuth2/OIDC server.")
   (extensions
    (list (service-extension activation-service-type
                             ory-hydra-activation)
          (service-extension account-service-type
                             ory-hydra-accounts)
          (service-extension shepherd-root-service-type
                             ory-hydra-shepherd-service)
          (service-extension log-rotation-service-type
                             ory-hydra-log-rotations)))
   (default-value #f)))

;;
;; Forseti. The binary discovers its config via FORSETI_CONFIG_PATH (the env
;; var baked into the v0.1.x binary) and writes state
;; (the default sqlite db, the webhook signing key under data/) relative to
;; the working directory, so the wrapper cd's into the state directory. The
;; static/ assets are embedded in the binary (served at /static/* regardless
;; of cwd), so no asset directory has to be present at runtime.
;;

(define (forseti-shepherd-service config)
  (match-record config <forseti-configuration>
    (package config-file environment-variables environment-file
     user group log-file state-directory requirement)
    (let ((forseti (file-append package "/bin/forseti"))
          (non-secret-env
           #~(list #$@(map (lambda (pair)
                             (string-append (car pair) "=" (cdr pair)))
                           environment-variables))))
      (list
       (shepherd-service
        (documentation "Run the Forseti identity portal.")
        (provision '(forseti))
        (requirement (append requirement '(user-processes)))
        (start #~(make-forkexec-constructor
                  (list #$(env-exec-wrapper
                           "forseti-serve-wrapper" environment-file
                           (list forseti)
                           #:workdir state-directory))
                  #:user #$user
                  #:group #$group
                  #:log-file #$log-file
                  #:environment-variables
                  (cons* (string-append "FORSETI_CONFIG_PATH=" #$config-file)
                         (append #$non-secret-env
                                 (default-environment-variables)))))
        (stop #~(make-kill-destructor #:grace-period 10)))))))

(define (forseti-accounts config)
  (match-record config <forseti-configuration> (user group state-directory)
    (ory-system-account user group state-directory)))

(define (forseti-activation config)
  (match-record config <forseti-configuration> (user state-directory log-file)
    (state+log-activation user state-directory log-file)))

(define (forseti-log-rotations config)
  (list (forseti-configuration-log-file config)))

(define forseti-service-type
  (service-type
   (name 'forseti)
   (description "Run and configure the Forseti identity portal.")
   (extensions
    (list (service-extension activation-service-type
                             forseti-activation)
          (service-extension account-service-type
                             forseti-accounts)
          (service-extension shepherd-root-service-type
                             forseti-shepherd-service)
          (service-extension log-rotation-service-type
                             forseti-log-rotations)))
   (default-value #f)))

;;
;; THE FORSETI STACK META
;;
;; Wires the three daemons from a single public `base-url`. Generates
;; production-shaped config files as file-likes; the operator may override
;; any of the three via the stack record fields.
;;

(define-record-type* <forseti-stack-configuration>
  forseti-stack-configuration make-forseti-stack-configuration
  forseti-stack-configuration?
  (base-url
   forseti-stack-configuration-base-url)              ;required public https origin
  (kratos
   forseti-stack-configuration-kratos
   (default #f))                                       ;file-like override or #f
  (hydra
   forseti-stack-configuration-hydra
   (default #f))
  (forseti
   forseti-stack-configuration-forseti
   (default #f))
  (manage-postgres?
   forseti-stack-configuration-manage-postgres?
   (default #t))
  (postgres-host
   forseti-stack-configuration-postgres-host
   (default "localhost"))
  (secrets-directory
   forseti-stack-configuration-secrets-directory
   (default "/etc/forseti"))
  (smtp-connection-uri
   forseti-stack-configuration-smtp-connection-uri
   (default #f))
  (enable-dynamic-client-registration?
   forseti-stack-configuration-enable-dynamic-client-registration?
   (default #f))
  (audit-webhook?
   forseti-stack-configuration-audit-webhook?
   (default #t))
  ;; Run the Ory daemons with --dev (relaxes the https-issuer-over-http boot
  ;; refusal). Use #f in production behind a TLS-terminating reverse proxy.
  (dev?
   forseti-stack-configuration-dev?
   (default #f))
  (kratos-log-file
   forseti-stack-configuration-kratos-log-file
   (default "/var/log/kratos.log"))
  (hydra-log-file
   forseti-stack-configuration-hydra-log-file
   (default "/var/log/hydra.log"))
  (forseti-log-file
   forseti-stack-configuration-forseti-log-file
   (default "/var/log/forseti.log")))

;; Minimal Kratos identity schema embedded as a data: URL so no schema file
;; has to be deployed alongside the config.
(define %kratos-identity-schema
  "{\"$id\":\"https://schemas.ory.sh/presets/kratos/identity.email.schema.json\",\"$schema\":\"http://json-schema.org/draft-07/schema#\",\"title\":\"Person\",\"type\":\"object\",\"properties\":{\"traits\":{\"type\":\"object\",\"properties\":{\"email\":{\"type\":\"string\",\"format\":\"email\",\"title\":\"E-Mail\",\"ory.sh/kratos\":{\"credentials\":{\"password\":{\"identifier\":true},\"webauthn\":{\"identifier\":true},\"passkey\":{\"display_name\":true},\"code\":{\"identifier\":true,\"via\":\"email\"}},\"recovery\":{\"via\":\"email\"},\"verification\":{\"via\":\"email\"}}}},\"required\":[\"email\"],\"additionalProperties\":false}}}")

(define (kratos-config base-url smtp-uri audit?)
  ;; Production-shaped Kratos config. DSN and `secrets.*` come from env
  ;; (DSN / SECRETS_COOKIE / SECRETS_CIPHER) so they stay out of the store.
  ;; Public/admin base URLs are loopback; the proxy fronts base-url.
  (plain-file
   "kratos.yml"
   (string-append
    "serve:\n"
    "  public:\n"
    ;; Shape 1: Kratos public mounts under /kratos; the proxy strips the prefix.
    "    base_url: " base-url "/kratos\n"
    "    cors:\n"
    "      enabled: true\n"
    "      allowed_origins:\n"
    "        - " base-url "\n"
    "      allowed_methods: [POST, GET, PUT, PATCH, DELETE]\n"
    "      allowed_headers: [Authorization, Cookie, Content-Type]\n"
    "      exposed_headers: [Content-Type, Set-Cookie]\n"
    "  admin:\n"
    "    base_url: http://127.0.0.1:" (number->string %kratos-admin) "/\n"
    "session:\n"
    "  whoami:\n"
    "    required_aal: highest_available\n"
    "selfservice:\n"
    "  default_browser_return_url: " base-url "/\n"
    "  allowed_return_urls:\n"
    "    - " base-url "\n"
    "  methods:\n"
    "    password:\n"
    "      enabled: true\n"
    "    code:\n"
    "      enabled: true\n"
    "      config:\n"
    "        lifespan: 15m\n"
    "    totp:\n"
    "      enabled: true\n"
    "      config:\n"
    "        issuer: forseti\n"
    "    lookup_secret:\n"
    "      enabled: true\n"
    "  flows:\n"
    "    error:\n"
    "      ui_url: " base-url "/error\n"
    "    settings:\n"
    "      ui_url: " base-url "/settings\n"
    "      privileged_session_max_age: 15m\n"
    "      required_aal: highest_available\n"
    "    recovery:\n"
    "      enabled: true\n"
    "      ui_url: " base-url "/recovery\n"
    "    verification:\n"
    "      enabled: true\n"
    "      ui_url: " base-url "/verification\n"
    "    logout:\n"
    "      after:\n"
    "        default_browser_return_url: " base-url "/login\n"
    "    login:\n"
    "      ui_url: " base-url "/login\n"
    "      lifespan: 10m\n"
    (if audit?
        (string-append
         "      after:\n"
         "        password:\n"
         "          hooks:\n"
         "            - hook: web_hook\n"
         "              config:\n"
         "                url: http://127.0.0.1:" (number->string %forseti-internal)
         "/internal/audit/kratos?action=auth.login\n"
         "                method: POST\n"
         "                body: base64://e30=\n"
         "                response:\n"
         "                  ignore: true\n"
         "                auth:\n"
         "                  type: api_key\n"
         "                  config:\n"
         "                    name: Authorization\n"
         "                    value: \"Bearer ${FORSETI_AUDIT_TOKEN}\"\n"
         "                    in: header\n")
        "")
    "    registration:\n"
    "      lifespan: 10m\n"
    "      ui_url: " base-url "/registration\n"
    "      after:\n"
    "        password:\n"
    "          hooks:\n"
    "            - hook: session\n"
    "log:\n"
    "  level: info\n"
    "  format: json\n"
    "  leak_sensitive_values: false\n"
    "ciphers:\n"
    "  algorithm: xchacha20-poly1305\n"
    "hashers:\n"
    "  algorithm: bcrypt\n"
    "  bcrypt:\n"
    "    cost: 12\n"
    "identity:\n"
    "  default_schema_id: default\n"
    "  schemas:\n"
    "    - id: default\n"
    "      url: base64://"
    (base64-encode %kratos-identity-schema) "\n"
    (if smtp-uri
        (string-append
         "courier:\n"
         "  smtp:\n"
         "    connection_uri: " smtp-uri "\n")
        ""))))

(define (hydra-config base-url dcr?)
  ;; Issuer + login/consent/logout/device URLs all point at the public origin.
  ;; DSN and `secrets.system` come from env (DSN / SECRETS_SYSTEM).
  (plain-file
   "hydra.yml"
   (string-append
    "serve:\n"
    "  cookies:\n"
    "    secure: true\n"
    "    same_site_mode: Lax\n"
    "  public:\n"
    "    host: 127.0.0.1\n"
    "    port: " (number->string %hydra-public) "\n"
    ;; Trust a loopback TLS-terminating reverse proxy: Hydra serves plain http
    ;; but accepts the https issuer when X-Forwarded-Proto comes from these CIDRs
    ;; (key path verified against ory.com/docs/hydra/configuration).
    "    tls:\n"
    "      allow_termination_from:\n"
    "        - 127.0.0.1/32\n"
    "        - ::1/128\n"
    "  admin:\n"
    "    host: 127.0.0.1\n"
    "    port: " (number->string %hydra-admin) "\n"
    "urls:\n"
    "  self:\n"
    ;; Shape 1 (path-prefixed): issuer and public carry the /hydra prefix so
    ;; discovery advertises base-url/hydra/... endpoints; the reverse proxy
    ;; strips the prefix before Hydra (which does not honour subpath mounting).
    ;; consent/login/logout/device and the DCR url stay at root: Forseti serves
    ;; those.
    "    issuer: " base-url "/hydra\n"
    "    public: " base-url "/hydra\n"
    "  consent: " base-url "/oauth/consent\n"
    "  login:   " base-url "/oauth/login\n"
    "  logout:  " base-url "/oauth/logout\n"
    "  device:\n"
    "    verification: " base-url "/oauth/device\n"
    "    success:      " base-url "/oauth/device/done\n"
    "oidc:\n"
    "  subject_identifiers:\n"
    ;; Hydra v26 rejects `pairwise' in supported_types unless a pairwise salt
    ;; is also configured; `public' alone is the valid default here.
    "    supported_types: [public]\n"
    (if dcr?
        (string-append
         "  dynamic_client_registration:\n"
         "    enabled: true\n"
         "    default_scope:\n"
         "      - openid\n"
         "      - offline\n"
         "      - offline_access\n"
         "webfinger:\n"
         "  oidc_discovery:\n"
         "    client_registration_url: " base-url "/oauth2/register\n")
        "")
    "oauth2:\n"
    "  expose_internal_errors: false\n"
    "  pkce:\n"
    "    enforced_for_public_clients: true\n"
    "strategies:\n"
    "  access_token: jwt\n"
    "ttl:\n"
    "  access_token: 5m\n"
    "  device_user_code: 10m\n")))

(define (forseti-config base-url smtp-uri audit?)
  ;; Forseti TOML. Secrets (audit token, cookie secret, db DSN, SMTP password)
  ;; come from FORSETI_* env overrides supplied by the env-file, not here.
  (plain-file
   "config.toml"
   (string-append
    "[kratos]\n"
    "public_url = \"http://127.0.0.1:" (number->string %kratos-public) "\"\n"
    "admin_url  = \"http://127.0.0.1:" (number->string %kratos-admin) "\"\n"
    "[hydra]\n"
    "public_url = \"http://127.0.0.1:" (number->string %hydra-public) "\"\n"
    "admin_url  = \"http://127.0.0.1:" (number->string %hydra-admin) "\"\n"
    "[self]\n"
    "url = \"" base-url "\"\n"
    "[brand]\n"
    "name          = \"Forseti\"\n"
    "support_email = \"support@example.com\"\n"
    "[internal]\n"
    "bind = \"127.0.0.1:" (number->string %forseti-internal) "\"\n"
    "[posix]\n"
    ;; Must equal Hydra's issuer (Shape 1: the /hydra-prefixed origin).
    "hydra_issuer = \"" base-url "/hydra\"\n"
    ;; [audit].webhook_token and [security].cookie_secret come from the
    ;; env-file (FORSETI_AUDIT__WEBHOOK_TOKEN / FORSETI_SECURITY__COOKIE_SECRET).
    (if smtp-uri
        (string-append
         "[smtp]\n"
         "enabled = true\n")
        ""))))

(define (forseti-stack-services config)
  (match-record config <forseti-stack-configuration>
    (base-url kratos hydra forseti manage-postgres? postgres-host
     secrets-directory smtp-connection-uri enable-dynamic-client-registration?
     audit-webhook? dev? kratos-log-file hydra-log-file forseti-log-file)
    (let* ((kratos-file (or kratos
                            (kratos-config base-url smtp-connection-uri
                                           audit-webhook?)))
           (hydra-file (or hydra (hydra-config
                                  base-url
                                  enable-dynamic-client-registration?)))
           (forseti-file (or forseti
                             (forseti-config base-url smtp-connection-uri
                                             audit-webhook?)))
           (envf (lambda (name)
                   (string-append secrets-directory "/" name)))
           ;; Unix-socket peer auth: the daemon runs as the OS user whose name
           ;; equals the PG role, so the DSN carries no password and is NOT a
           ;; secret. The explicit `user@' makes the driver connect as that role
           ;; rather than guessing from the environment. The pool-tuning params
           ;; (max_conns/max_idle_conns) are intentionally omitted: with them in
           ;; the socket DSN, kratos migrate did not persist to the kratos
           ;; database (verified in the VM test), while the plain form does.
           (kratos-dsn
            "postgres://kratos@/kratos?host=/var/run/postgresql&sslmode=disable")
           (hydra-dsn
            "postgres://hydra@/hydra?host=/var/run/postgresql&sslmode=disable"))
      (append
       (if manage-postgres?
           (list
            (service postgresql-service-type
                     (postgresql-configuration
                      (postgresql postgresql)))
            (service postgresql-role-service-type
                     (postgresql-role-configuration
                      (roles
                       (list (postgresql-role
                              (name "kratos")
                              (create-database? #t))
                             (postgresql-role
                              (name "hydra")
                              (create-database? #t))
                             ;; Forseti uses sqlite under its state dir unless
                             ;; FORSETI_DATABASE__URL is supplied; the role/db
                             ;; is created anyway for future use.
                             (postgresql-role
                              (name "forseti")
                              (create-database? #t)))))))
           '())
       (list
        (service ory-kratos-service-type
                 (ory-kratos-configuration
                  (config-file kratos-file)
                  (environment-variables `(("DSN" . ,kratos-dsn)))
                  (environment-file (envf "kratos.env"))
                  (user "kratos")
                  (group "kratos")
                  (log-file kratos-log-file)
                  (dev? dev?)
                  (requirement '(postgres-roles))))
        (service ory-hydra-service-type
                 (ory-hydra-configuration
                  (config-file hydra-file)
                  (environment-variables `(("DSN" . ,hydra-dsn)))
                  (environment-file (envf "hydra.env"))
                  (user "hydra")
                  (group "hydra")
                  (log-file hydra-log-file)
                  (dev? dev?)
                  (requirement '(postgres-roles))))
        (service forseti-service-type
                 (forseti-configuration
                  (config-file forseti-file)
                  (environment-file (envf "forseti.env"))
                  (user "forseti")
                  (group "forseti")
                  (log-file forseti-log-file)
                  (requirement '(hydra kratos postgres-roles)))))))))

;; Tiny base64 encoder for embedding the identity schema as a data URL.
(define (base64-encode str)
  (let* ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (bytes (string->utf8 str))
         (len (bytevector-length bytes))
         (out (open-output-string)))
    (let loop ((i 0))
      (when (< i len)
        (let* ((b0 (bytevector-u8-ref bytes i))
               (b1 (if (< (+ i 1) len) (bytevector-u8-ref bytes (+ i 1)) 0))
               (b2 (if (< (+ i 2) len) (bytevector-u8-ref bytes (+ i 2)) 0))
               (n (logior (ash b0 16) (ash b1 8) b2)))
          (write-char (string-ref alphabet (logand (ash n -18) 63)) out)
          (write-char (string-ref alphabet (logand (ash n -12) 63)) out)
          (write-char (if (< (+ i 1) len)
                          (string-ref alphabet (logand (ash n -6) 63))
                          #\=)
                      out)
          (write-char (if (< (+ i 2) len)
                          (string-ref alphabet (logand n 63))
                          #\=)
                      out))
        (loop (+ i 3))))
    (get-output-string out)))
