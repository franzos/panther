;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages ai)
  #:use-module (gnu packages bash)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix licenses))

(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://storage.googleapis.com/claude-code-dist-"
             "86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/"
             version "/linux-x64/claude"))
       (sha256
        (base32 "0i59yw3hmfcv4yka7lpjhxiplfik0gnpvdbn6c9kpkqyhxriqajf"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan
      #~'(("claude" ()))
      #:install-plan
      #~'(("claude" "bin/claude-unwrapped"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (copy-file (assoc-ref inputs "source") "claude")
              (chmod "claude" #o755)))
          (add-after 'install 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (unwrapped (string-append bin "/claude-unwrapped"))
                     (wrapper (string-append bin "/claude")))
                (call-with-output-file wrapper
                  (lambda (port)
                    (format port "#!~a
export DISABLE_AUTOUPDATER=1
export DISABLE_INSTALLATION_CHECKS=1
exec ~a \"$@\"
"
                            (search-input-file inputs "bin/bash")
                            unwrapped)))
                (chmod wrapper #o755)))))))
    (inputs
     (list bash-minimal))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/anthropics/claude-code")
    (synopsis "Claude AI assistant for the terminal")
    (description
     "Claude Code is an agentic coding tool that lives in your terminal.
It can understand your codebase, edit files, run terminal commands, and
handle entire workflows.  This package disables auto-updates.")
    (license (nonfree "https://code.claude.com/docs/en/legal-and-compliance"))))
