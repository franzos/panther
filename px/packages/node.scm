;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages node)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages node)
  #:use-module (gnu packages tls)
  #:use-module (nonguix build-system binary))

(define-public pnpm
  (package
    (name "pnpm")
    (version "11.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pnpm/pnpm/releases/download/v"
                           version "/pnpm-linux-"
                           (match (or (%current-system)
                                      (%current-target-system))
                             ("x86_64-linux" "x64")
                             ("aarch64-linux" "arm64"))
                           ".tar.gz"))
       (sha256
        (base32
         (match (or (%current-system)
                    (%current-target-system))
           ("x86_64-linux"
            "0rkn2fw3i2hziirsv7a0bhx4rkw742fbw77vbjd7rxlhpbqwj8ng")
           ("aarch64-linux"
            "02vjbscc6nad9rir4jbarjlpmpxiqhaypmfqjmh51nq0701i983x"))))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("pnpm" ("glibc" "gcc:lib")))
       ;; The `pnpm' binary loads `./dist/pnpm.mjs' relative to its own
       ;; location, so install it next to `dist/' and symlink into `bin/'.
       #:install-plan `(("pnpm" "lib/pnpm/pnpm")
                        ("dist" "lib/pnpm/dist"))
       ;; `dist/node_modules/.bin/' contains absolute symlinks pointing to the
       ;; upstream CI build path; `validate-runpath' chokes trying to read
       ;; them.  Skip it -- only the `pnpm' binary needs patching.
       #:validate-runpath? #f
       ;; `pnpm' is a Node.js Single Executable Application; `strip' moves
       ;; sections and corrupts the embedded SEA blob, so leave it alone.
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         ;; The tarball has multiple top-level entries (`pnpm' and `dist').
         ;; The standard `unpack' phase chdirs into the only subdirectory it
         ;; finds (`dist'), so step back up before patching and installing.
         (add-after 'unpack 'chdir-up
           (lambda _
             (chdir "..")))
         (add-after 'install 'symlink-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (symlink (string-append out "/lib/pnpm/pnpm")
                        (string-append bin "/pnpm"))))))))
    (inputs `(("glibc" ,glibc)
              ("gcc:lib" ,gcc "lib")))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://pnpm.io")
    (synopsis "Fast, disk space efficient package manager for nodejs")
    (description "PNPM uses a content-addressable filesystem to
store all files from all module directories on a disk")
    (license license:expat)))

(define-public pnpm-9
  (package
    (name "pnpm")
    (version "9.15.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pnpm/pnpm/releases/download/v"
                           version "/pnpm-linuxstatic-"
                           (match (or (%current-system)
                                      (%current-target-system))
                             ("x86_64-linux" "x64")
                             ("aarch64-linux" "arm64"))))
       (sha256
        (base32 "1h3zl7zmjfnkvxc6s7sr6xf4vrzjhpzpysg514yq5g6sri0jm3q3"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules ((guix build utils)))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (bin (string-append %output "/bin"))
                          (exe (string-append bin "/pnpm")))
                     (mkdir-p bin)
                     (copy-file source exe)
                     (chmod exe #o755)))))
    (home-page "https://pnpm.io")
    (synopsis "Fast, disk space efficient package manager for nodejs")
    (description "PNPM uses a content-addressable filesystem to
store all files from all module directories on a disk")
    (license license:expat)))

(define-public yarn
  (package
    (name "yarn")
    (version "1.22.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/yarnpkg/yarn/releases/download/v"
                           version "/yarn-v" version ".tar.gz"))
       (sha256
        (base32 "181nvynhhrbga3c209v8cd9psk6lqjkc1s9wyzy125lx35j889l8"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("bin/" "bin/")
         ("lib/" "lib/")
         ("package.json" "lib/yarn/")
         ("LICENSE" "share/doc/yarn/")
         ("README.md" "share/doc/yarn/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'make-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (for-each
                (lambda (file)
                  (chmod file #o755))
                (find-files bin ".*"))))))))
    (home-page "https://classic.yarnpkg.com")
    (synopsis "Fast, reliable, and secure dependency management.")
    (description "Yarn is a package manager for your code.
It allows you to use and share (e.g. JavaScript) code with
other developers from around the world. Yarn does this quickly,
securely, and reliably so you don’t ever have to worry.")
    (license license:expat)))

;; Node 24 bundles libuv 1.52.x; match it.  Get the version from
;; https://github.com/nodejs/node/blob/main/deps/uv/include/uv/version.h
(define-public libuv-for-node-24
  (package
    (inherit libuv)
    (name "libuv")
    (version "1.52.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dist.libuv.org/dist/v" version
                                  "/libuv-v" version ".tar.gz"))
              (sha256
               (base32
                "1rzrrylgqyjcnjarph3r2h5i3cxjpx7j7svr4bkc0d73wswi3mb6"))))
    (properties '((hidden? . #t)))))

;; Node 24 bundles llhttp 9.4.x.  Rebuild the generated C sources from the
;; upstream TypeScript so we can replace node's pre-generated copies.
(define-public llhttp-for-node-24
  (package
    (inherit llhttp-bootstrap)
    (version "9.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nodejs/llhttp.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "llhttp" version))
              (sha256
               (base32
                "0yb46qksyw0h14r1qp84xkk8zijfi661991288isivba61hijp2z"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Makefile"
                  (("node --import tsx bin/generate.ts")
                   "node bin/generate.js")))))))

(define-public node-24
  (package
    (inherit node-lts)
    (name "node")
    (version "24.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.gz"))
              (sha256
               (base32
                "1dvy8y51qad7gx5fhlzbfrmav5c9rlasckgxd7n3k1qxnikq0d68"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; openssl.cnf is required for build.
                  (for-each delete-file-recursively
                            (find-files "deps/openssl"
                                        (lambda (file stat)
                                          (not (string-contains file "nodejs-openssl.cnf")))))
                  ;; Remove bundled software, where possible
                  (for-each delete-file-recursively
                            '("deps/brotli"
                              "deps/cares"
                              "deps/icu-small"
                              "deps/nghttp2"
                              "deps/ngtcp2"
                              "deps/uv"
                              "deps/zlib"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments node-lts)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'delete-problematic-tests 'delete-node-24-problematic-tests
             (lambda _
               ;; setuid/setgid is denied in the build container: spawn fails
               ;; with EINVAL rather than the expected EPERM.
               (delete-file "test/parallel/test-child-process-uid-gid.js")
               ;; These fail against the older shared nghttp2 (1.58 vs node's
               ;; bundled 1.69): flow-control violations now surface as a
               ;; stream error instead of a session protocol error.
               (for-each delete-file
                         '("test/parallel/test-http2-misbehaving-flow-control.js"
                           "test/parallel/test-http2-misbehaving-flow-control-paused.js"))))
           ;; npm's bundled tar moved to a dist/{esm,commonjs} layout, so the
           ;; inherited phase's lib/write-entry.js path no longer exists.
           (replace 'ignore-number-of-hardlinks
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((dir (string-append (assoc-ref outputs "out")
                                         "/lib/node_modules/npm/node_modules"
                                         "/tar/dist")))
                 (substitute* (list (string-append dir "/commonjs/write-entry.js")
                                    (string-append dir "/esm/write-entry.js"))
                   (("this.stat.nlink > 1") "false")))))))))
    (inputs
     (modify-inputs (package-inputs node-lts)
       (replace "openssl" openssl-3.5)
       (replace "icu4c" icu4c-78)
       (replace "libuv" libuv-for-node-24)
       (replace "llhttp" llhttp-for-node-24)))
    (native-inputs
     (modify-inputs (package-native-inputs node-lts)
       (replace "openssl" openssl-3.5)
       (replace "icu4c" icu4c-78)
       (replace "libuv" libuv-for-node-24)))))
