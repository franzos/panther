;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages mail)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix base32)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages rust)
  #:use-module (px packages node)
  #:use-module (px packages rust)
  #:use-module (px self))

(define-public himalaya
  (package
    (name "himalaya")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "himalaya" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ca2gmpf9hr5nl9fg2s9j2x2m7xcnc8ifccmblnqfx8h6aq24jiz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.89))
    (inputs (px-cargo-inputs 'himalaya))
    (home-page "https://pimalaya.org/")
    (synopsis "CLI to manage emails")
    (description
     "Himalaya is a command-line interface for managing emails, providing a
modern and efficient way to interact with email accounts.  It supports IMAP,
Maildir, Notmuch, SMTP, and Sendmail backends, along with OAuth 2.0
authorization for various email providers including Gmail, Outlook, and iCloud.")
    (license license:expat)))

(define bichon-version "1.5.2")

(define bichon-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/rustmailer/bichon")
          (commit bichon-version)))
    (file-name (git-file-name "bichon" bichon-version))
    (sha256
     (base32 "04h7i19ldfmafqmc9f7ai89nz1kkpcd7wy32bkk7cwp7awri9w22"))
    (snippet
        #~(begin
            (use-modules (guix build utils)
                         (ice-9 regex)
                         (ice-9 rdelim))
            ;; Set a fixed GIT_HASH since git isn't available during build
            (substitute* "crates/server/build.rs"
              (("Command::new\\(\"git\"\\)")
               "Command::new(\"echo\")")
              (("\\.args\\(&\\[\"rev-parse.*\\]\\)")
               ".arg(\"1.5.2\")"))
            ;; Replace git deps with path deps (multi-line: read/regex/write)
            (define (rewrite-file path pattern replacement)
              (let* ((content (call-with-input-file path
                                (lambda (p)
                                  (let loop ((acc ""))
                                    (let ((line (read-line p 'concat)))
                                      (if (eof-object? line)
                                          acc
                                          (loop (string-append acc line)))))))))
                (chmod path #o644)
                (call-with-output-file path
                  (lambda (port)
                    (display (regexp-substitute/global
                              #f pattern content
                              'pre replacement 'post)
                             port)))))
            (rewrite-file "crates/core/Cargo.toml"
                          "async-imap = \\{ git[^}]+\\}"
                          (string-append
                           "async-imap = { path = \"../../vendored-git/async-imap\""
                           ", default-features = false"
                           ", features = [\"runtime-tokio\", \"compress\"] }"))
            (rewrite-file "crates/cli/Cargo.toml"
                          "outlook-pst = \\{ git[^}]+\\}"
                          "outlook-pst = { path = \"../../vendored-git/outlook-pst\" }")))))

;; Build the Vite/TypeScript frontend in a fixed-output (network-enabled,
;; hash-pinned) derivation; its dist/ is embedded by RustEmbed in the server.
(define bichon-web-dist
  (computed-file
   "bichon-web-dist"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (setenv "PATH" (string-append #$node "/bin:" #$pnpm-9 "/bin:"
                                       (getenv "PATH")))
         (setenv "HOME" "/tmp")
         (setenv "SSL_CERT_DIR" (string-append #$nss-certs "/etc/ssl/certs"))
         (setenv "SSL_CERT_FILE"
                 (string-append #$nss-certs "/etc/ssl/certs/ca-certificates.crt"))
         (copy-recursively (string-append #$bichon-source "/web") "/tmp/web")
         (with-directory-excursion "/tmp/web"
           ;; Skip native postinstall scripts (@swc/core, @parcel/watcher,
           ;; esbuild); the prebuilt platform binaries are installed anyway.
           (invoke "pnpm" "install" "--frozen-lockfile" "--ignore-scripts")
           ;; Run vite directly: the build script's `tsc -b` type-check gets
           ;; killed in the sandbox and isn't needed to emit dist/.
           (invoke "node" "node_modules/vite/bin/vite.js" "build"))
         (copy-recursively "/tmp/web/dist" #$output)))
   #:options `(#:hash-algo sha256
               #:hash ,(base32 "0jkrp1z1m3fp8g4scd0gv1glml56knlyya9cn49mylslyn3yskkp")
               #:recursive? #t)))

(define-public bichon
  (package
    (name "bichon")
    (version bichon-version)
    (source bichon-source)
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.95
       #:cargo-install-paths
       '("crates/server" "crates/cli" "crates/admin")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'install-web-dist
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             ;; Prebuilt frontend for RustEmbed in the server crate
             (mkdir-p "web")
             (copy-recursively
              (assoc-ref (or native-inputs inputs) "bichon-web-dist")
              "web/dist")))
         ;; Place the two git-fetched deps under ./vendored-git/ so member
         ;; crates can reference them via path.  outlook-pst's crates/pst is
         ;; extracted as a standalone package — cargo walks up to bichon's
         ;; workspace, not outlook-pst's, so `.workspace = true` inheritances
         ;; can't resolve.  For async-imap, also strip [dev-dependencies] and
         ;; the optional async-std dep: cargo pulls dev-deps for path deps
         ;; even when not building tests.
         (add-after 'unpack-rust-crates 'vendor-git-deps
           (lambda* (#:key inputs #:allow-other-keys)
             (use-modules (ice-9 rdelim))
             (mkdir-p "vendored-git")
             (define (handle-input pair)
               (let ((name (car pair))
                     (path (cdr pair)))
                 (cond
                  ((and (string? path)
                        (string-prefix? "rust-async-imap-" name))
                   (let ((dest "vendored-git/async-imap"))
                     (unless (file-exists? dest)
                       (copy-recursively path dest))
                     ;; Drop [dev-dependencies] (cargo pulls those for path deps)
                     ;; and the optional async-std dep (not activated, but cargo
                     ;; still requires it to be resolvable in the vendor dir).
                     (let* ((toml-path (string-append dest "/Cargo.toml"))
                            (content (call-with-input-file toml-path
                                       (lambda (p)
                                         (let loop ((acc ""))
                                           (let ((line (read-line p 'concat)))
                                             (if (eof-object? line)
                                                 acc
                                                 (loop (string-append acc line))))))))
                            (idx (string-contains content "[dev-dependencies]"))
                            (trimmed (if idx (substring content 0 idx) content)))
                       (chmod toml-path #o644)
                       (call-with-output-file toml-path
                         (lambda (port) (display trimmed port))))
                     (substitute* (string-append dest "/Cargo.toml")
                       (("^async-std = .*$") "")
                       (("\"async-std\",") "")
                       (("^default = \\[\"runtime-async-std\"\\]")
                        "default = []")
                       (("^runtime-async-std = .*$") ""))))
                  ((and (string? path)
                        (string-prefix? "rust-outlook-pst-" name))
                   (let ((dest "vendored-git/outlook-pst"))
                     (unless (file-exists? dest)
                       (copy-recursively (string-append path "/crates/pst")
                                         dest))
                     (chmod (string-append dest "/Cargo.toml") #o644)
                     (call-with-output-file (string-append dest "/Cargo.toml")
                       (lambda (port)
                         (display "\
[package]
name = \"outlook-pst\"
description = \"Outlook PST Store Provider in Rust\"
version = \"1.1.0\"
authors = [\"Microsoft\"]
edition = \"2021\"
rust-version = \"1.82\"
repository = \"https://github.com/microsoft/outlook-pst-rs\"
license = \"MIT\"
keywords = [\"win32\", \"outlook\", \"mapi\"]
categories = [\"os::windows-apis\"]

[dependencies]
byteorder = \"1\"
thiserror = \"2\"
tracing = \"0.1\"
" port))))))))
             (for-each handle-input inputs))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("bichon-web-dist" ,bichon-web-dist)))
    (inputs
     (cons* openssl `(,zstd "lib") (px-cargo-inputs 'bichon)))
    (home-page "https://github.com/rustmailer/bichon")
    (synopsis "Email archiver with full-text search and web interface")
    (description
     "Bichon is a lightweight email archiver that synchronizes emails from IMAP
servers, indexes them for full-text search, and provides a REST API and web
interface.  It supports multiple accounts, tag-based organization, and email
export in EML format.")
    (license license:agpl3)))

(define-public claws-mail-theme-breeze
  (package
    (name "claws-mail-theme-breeze")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.claws-mail.org/download.php?file=themes/png/claws-mail-theme_breeze.tar.gz"))
       (sha256
        (base32 "104ak4m3s7i4d44clpn4kcq4bhjz92ybmpiw83dpg2xwc9w8k2pf"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar (assoc-ref %build-inputs "tar"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (theme-dir (string-append %output
                                      "/share/claws-mail/themes")))
                     (mkdir-p theme-dir)
                     (setenv "PATH"
                             (string-append gzip "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" source "-C"
                             theme-dir)))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)))
    (synopsis "claws-mail breeze theme")
    (home-page "https://www.claws-mail.org/themes.php")
    (description "claws-mail breeze theme")
    (license license:gpl3+)))