;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages node)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (guix packages))

(define-public pnpm
  (package
    (name "pnpm")
    (version "10.29.3")
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
        (base32 "1jzhkfs79h01syk8dqj9wfngrhwd79kwawlyn6fscrcbbx3gwnd5"))))
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

(define-public pnpm-9
  (package
    (name "pnpm")
    (version "9.15.5")
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
        (base32 "02nwfl15rhx0vjfm5za6grg1748kdlqkw6yfz21g4sgcv3sfiadb"))))
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
