;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages version-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages perl)
  #:use-module (px packages rust)
  #:use-module (px self))

(define-public gh
  (package
    (name "gh")
    (version "2.86.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cli/cli/releases/download/v"
                           version "/gh_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "1iq3h6k1q1612is2jjzhscwbiwi5za3sd8dh54icq844lbb8pc7k"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("bin/gh" "bin/")
         ("share/man" "share/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'generate-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash-comp (string-append out "/etc/bash_completion.d"))
                    (zsh-comp (string-append out "/share/zsh/site-functions"))
                    (fish-comp (string-append out "/share/fish/vendor_completions.d"))
                    (gh (string-append out "/bin/gh")))
               (mkdir-p bash-comp)
               (mkdir-p zsh-comp)
               (mkdir-p fish-comp)
               ;; Generate shell completions
               (with-output-to-file (string-append bash-comp "/gh")
                 (lambda () (invoke gh "completion" "-s" "bash")))
               (with-output-to-file (string-append zsh-comp "/_gh")
                 (lambda () (invoke gh "completion" "-s" "zsh")))
               (with-output-to-file (string-append fish-comp "/gh.fish")
                 (lambda () (invoke gh "completion" "-s" "fish")))
               #t))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://cli.github.com")
    (synopsis "GitHub command-line tool")
    (description
     "gh is GitHub on the command line.  It brings pull requests, issues, and
other GitHub concepts to the terminal next to where you are already working
with git and your code.")
    (license license:expat)))

(define-public jj-vcs
  (package
    (name "jj-vcs")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jj-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "115mhyxil71yw4icnki2id1xvpa21kla7dss4dp69jf9mr68n14w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f  ; Tests require testutils module
       #:rust ,rust-1.89))
    (native-inputs (list pkg-config))
    (inputs (cons* openssl (px-cargo-inputs 'jj-cli)))
    (home-page "https://github.com/jj-vcs/jj")
    (synopsis "Git-compatible version control system")
    (description
     "Jujutsu (jj) is a Git-compatible version control system that is both
powerful and easy to use.  It features automatic working copy management,
operation logging for easy undo, first-class conflict handling, automatic
rebasing of descendant commits, and comprehensive history rewriting tools.")
    (license license:asl2.0)))

(define-public keifu
  (package
    (name "keifu")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "keifu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pyhm35rdqyh3i7m1sgd4k19l8fjqfqn70pcbgdpa1c003a5jh0r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (native-inputs (list perl))
    (inputs (px-cargo-inputs 'keifu))
    (home-page "https://github.com/trasta298/keifu")
    (synopsis "TUI tool to visualize Git commit graphs")
    (description
     "Keifu is a terminal user interface tool that visualizes Git commit graphs
with branch genealogy.  It provides color-coded commit graphs, branch
information, file change statistics, and basic Git operations.")
    (license license:expat)))
