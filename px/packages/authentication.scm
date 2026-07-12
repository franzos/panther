;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages authentication)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages java)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (px self))

(define-public keycloak
  (package
    (name "keycloak")
    (version "26.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/keycloak/keycloak/releases/download/"
             version "/keycloak-" version ".tar.gz"))
       (sha256
        (base32 "12hizskzhj3db9zdxy23gd41ar7bbc0nszbgsmbhz0p4l45dywgp"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "libexec/keycloak/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-launchers
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out #$output)
                     (home (string-append out "/libexec/keycloak"))
                     (bin (string-append out "/bin"))
                     (java (search-input-file inputs "/bin/java"))
                     (java-home (dirname (dirname java))))
                ;; The launchers resolve their install dir via readlink -f
                ;; on $0, so exec'ing them by absolute path keeps that intact.
                (for-each delete-file
                          (find-files (string-append home "/bin") "\\.bat$"))
                (mkdir-p bin)
                (for-each
                 (lambda (script)
                   (let ((target (string-append home "/bin/" script))
                         (wrapper (string-append bin "/" script)))
                     (call-with-output-file wrapper
                       (lambda (port)
                         (format port "#!~a
export JAVA_HOME=\"~a\"
export PATH=\"~a/bin:$PATH\"
exec \"~a\" \"$@\"
"
                                 (search-input-file inputs "/bin/bash")
                                 java-home java-home target)))
                     (chmod wrapper #o555)))
                 '("kc.sh" "kcadm.sh" "kcreg.sh"))))))))
    (inputs (list bash-minimal openjdk21))
    (home-page "https://www.keycloak.org/")
    (synopsis "Identity and access management server")
    (description
     "Keycloak is an open source identity and access management solution
providing single sign-on, user federation, identity brokering, and social
login.  This package repackages the upstream prebuilt distribution and runs
on OpenJDK 21.")
    (license license:asl2.0)))

(define-public forseti-unix
  (package
    (name "forseti-unix")
    (version "0.1.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/forseti")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h6525x84f1inixhbxrmvfmkzrnf89w7n6yw00xi4avwdisk6mrc"))))
    (build-system cargo-build-system)
    (arguments
     (list
      ;; Tests drive the Forseti socket protocol and a wiremock HTTP server;
      ;; they belong to the integration suite, not the package build, where
      ;; there is no daemon socket and no network.
      #:tests? #f
      ;; We install hand-picked artifacts (two bins + one .so), not crate
      ;; tarballs.
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; The buildable workspace is the forseti-unix/ subdirectory; enter it
          ;; before configure so Cargo.toml, the vendor dir and target/release
          ;; all resolve relative to it.
          (add-after 'unpack 'enter-workspace
            (lambda _
              (chdir "forseti-unix")))
          ;; Build the workspace members explicitly so artifacts land
          ;; predictably regardless of cargo's default member selection.
          (replace 'build
            (lambda _
              (invoke "cargo"
                      "build"
                      "--release"
                      "--offline"
                      "--package"
                      "forseti-unixd"
                      "--package"
                      "forseti-ssh-authorizedkeys"
                      "--package"
                      "libnss_forseti"
                      "--package"
                      "pam_forseti")))
          ;; Thin-.so guard.  Both cdylibs are dlopened into other processes
          ;; (libnss_forseti into every name-lookup; pam_forseti into setuid
          ;; sshd/login pre-auth); neither must drag in tokio/reqwest/rusqlite.
          ;; A heavy dep would show up as an extra NEEDED entry.
          (add-after 'build 'check-thin-nss
            (lambda _
              (use-modules (ice-9 popen)
                           (ice-9 textual-ports))
              (define (needed-of so)
                (let* ((port (open-pipe* OPEN_READ
                                         #$(file-append patchelf
                                                        "/bin/patchelf")
                                         "--print-needed" so))
                       (out (get-string-all port)))
                  (close-pipe port)
                  out))
              (for-each (lambda (so)
                          (when (file-exists? so)
                            (let ((needed (needed-of so)))
                              (for-each (lambda (forbidden)
                                          (when (string-contains needed
                                                                 forbidden)
                                            (error (string-append
                                                    "thin-.so invariant violated: "
                                                    so " links " forbidden))))
                                        '("libssl" "libcrypto" "libsqlite")))))
                        '("target/release/libnss_forseti.so"
                          "target/release/libpam_forseti.so"))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (sbin (string-append out "/sbin"))
                     (lib (string-append out "/lib"))
                     (rel "target/release"))
                (mkdir-p bin)
                (mkdir-p sbin)
                (mkdir-p lib)
                (install-file (string-append rel "/forseti-unixd") sbin)
                (install-file (string-append rel "/forseti_ssh_authorizedkeys")
                              bin)
                ;; NSS module: glibc dlopens it by versioned soname.  build.rs
                ;; already stamps DT_SONAME=libnss_forseti.so.2 so the file
                ;; matches its soname.
                (copy-file (string-append rel "/libnss_forseti.so")
                           (string-append lib "/libnss_forseti.so.2"))
                ;; PAM module: PAM dlopens it by plain name from its module
                ;; dir.  The service references it by absolute file-append, so
                ;; lib/security is the canonical home.  cargo names the cdylib
                ;; libpam_forseti.so; drop the lib prefix for PAM.
                (let ((security (string-append lib "/security")))
                  (mkdir-p security)
                  (copy-file (string-append rel "/libpam_forseti.so")
                             (string-append security "/pam_forseti.so")))))))))
    (inputs (cons* glibc
                   ;; rusqlite's "bundled" feature does not propagate through the
                   ;; workspace build under cargo-build-system, so libsqlite3-sys
                   ;; falls back to a dynamic -lsqlite3 link; satisfy it with the
                   ;; packaged sqlite (cleaner than the bundled C copy anyway).  Only
                   ;; the daemon links it; the NSS .so and helper stay thin.
                   sqlite
                   ;; The forseti-unixd entry holds the full workspace crate closure
                   ;; (the importer keys each member to the whole lockfile graph, so
                   ;; all three lookup entries are identical); it already includes
                   ;; rust-libnss for the cdylib.
                   (px-cargo-inputs 'forseti-unixd)))
    (native-inputs (list gcc-toolchain patchelf))
    (home-page "https://git.gofranz.com/franz/forseti")
    (synopsis "Linux NSS client workspace for Forseti identity")
    (description
     "This package builds the @code{forseti-unix} client workspace: the
@command{forseti-unixd} daemon that resolves accounts and SSH keys from a
Forseti server over an authenticated HTTP API and caches them locally; a thin
@code{libnss_forseti.so.2} NSS module that answers @code{passwd} and
@code{group} lookups from the daemon's Unix socket; and the
@command{forseti_ssh_authorizedkeys} helper for the @code{AuthorizedKeysCommand}
of sshd.  The daemon uses rustls for TLS, so no system OpenSSL is linked; the
NSS module and the sshd helper stay deliberately thin because the module is
dlopened into every process that performs a name lookup, including setuid
@command{sshd} and @command{sudo}.")
    (license license:agpl3+)))

(define-public ory-hydra
  (package
    (name "ory-hydra")
    (version "26.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ory/hydra/releases/download/v" version
             "/hydra_" version "-linux_static-nosqlite_"
             (match (or (%current-system) (%current-target-system))
               ("aarch64-linux" "arm64")
               (_ "64bit"))
             ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("aarch64-linux"
            "1sc4mgccjfchsfikz1w7jdv5fd0p4g0qk2ydql91kc829fkjkjyp")
           (_
            "1afzdfzxpw7jl7i6mff1m7z98z4w678s3i2bd4i8p82yybsv82vv"))))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan `(("hydra" "bin/"))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://www.ory.sh/hydra/")
    (synopsis "OAuth 2.0 and OpenID Connect server")
    (description
     "Ory Hydra is an OAuth 2.0 and OpenID Connect provider.  It issues and
validates access, refresh, and ID tokens, and delegates the user-facing login
and consent steps to an external application over a redirect-based flow.  This
package installs the upstream statically linked release binary (Postgres,
MySQL, and CockroachDB backends; no embedded SQLite).")
    (license license:asl2.0)))

(define-public ory-kratos
  (package
    (name "ory-kratos")
    (version "26.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ory/kratos/releases/download/v" version
             "/kratos_" version "-linux_static-nosqlite_"
             (match (or (%current-system) (%current-target-system))
               ("aarch64-linux" "arm64")
               (_ "64bit"))
             ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("aarch64-linux"
            "0mrj4wxlz4a4r0ji0kw3w1dycz3i8zm9ixvl2w647dgj1v4zjshf")
           (_
            "0f97414hnxrs07zzyvy094gdchs54s3k97pacrzhw8lm4k7zgvi4"))))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan `(("kratos" "bin/"))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://www.ory.sh/kratos/")
    (synopsis "Identity and user management server")
    (description
     "Ory Kratos is an identity and user management server.  It handles
self-service login, registration, multi-factor authentication, account
recovery and verification, profile management, and social sign-in, and stores
identities in an SQL database.  This package installs the upstream statically
linked release binary (Postgres, MySQL, and CockroachDB backends; no embedded
SQLite).")
    (license license:asl2.0)))

(define-public forseti
  (package
    (name "forseti")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/franzos/forseti/releases/download/v" version
             "/forseti-"
             (match (or (%current-system) (%current-target-system))
               ("aarch64-linux" "aarch64-unknown-linux-gnu")
               (_ "x86_64-unknown-linux-gnu"))
             ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         (match (or (%current-system) (%current-target-system))
           ("aarch64-linux"
            "0i49gzcsv69x8bhfc926qmmizfapz61lqqp1ln0ajcxxqk3w13ik")
           (_
            "1sbfiwpmk8phvv5r2y0bwsdfi7a8xr3ql1kkv6mwjdwmksyvrd6a"))))))
    (build-system binary-build-system)
    (arguments
     ;; Prebuilt glibc binary: dynamically links libssl/libcrypto (OpenSSL),
     ;; libpq (Postgres backend), libgcc_s, and libc.  Askama templates are
     ;; compiled into the binary, so the only runtime asset tree is static/.
     `(#:patchelf-plan `(("forseti" ("glibc" "gcc:lib" "openssl" "postgresql")))
       #:install-plan `(("forseti" "bin/")
                        ("static" "share/forseti/")
                        ("config.example.toml" "share/forseti/"))))
    (inputs
     `(("glibc" ,glibc)
       ("gcc:lib" ,gcc "lib")
       ("openssl" ,openssl)
       ("postgresql" ,postgresql)))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://git.gofranz.com/franz/forseti")
    (synopsis "Login, consent, and account portal for the Ory stack")
    (description
     "Forseti is the user-facing bridge for an Ory Kratos and Hydra
deployment.  It implements Hydra's login, consent, and logout handlers and the
Kratos self-service flows (login, registration, recovery, verification,
multi-factor, and account settings), and adds an admin portal for OAuth2 client
and organization management.  This package installs the upstream release
binary together with its @file{static/} assets; HTML templates are compiled
into the binary.")
    (license license:agpl3+)))
