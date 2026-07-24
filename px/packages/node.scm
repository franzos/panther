;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages node)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (nonguix build-system binary))

(define-public pnpm
  (package
    (name "pnpm")
    (version "11.17.0")
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
            "1gx08gzbjx639fqx766wi5c32zr8bjfa0nas82ap8x8gpw0xpcdx")
           ("aarch64-linux"
            "1lq4yplry8jckys8xkfidaxccmh4zjndg4ds42qgngiafkg1f3bk"))))))
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

