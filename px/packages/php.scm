;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages php)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages music)
  #:use-module (gnu packages php)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

;; TODO: Drop this override once upstream guix php works
;; lsp-plugins uses php as a build-time codegen tool, which transitively
;; blocks easyeffects. Both are rewritten below to pick up this override.
(define-public php-8.5
  (package
    (inherit (@ (gnu packages php) php-8.5))
    (arguments
     (substitute-keyword-arguments
         (package-arguments (@ (gnu packages php) php-8.5))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'prepare-tests 'delete-flaky-sandbox-tests
              (lambda _
                (for-each delete-file
                          '("ext/openssl/tests/bug74796.phpt"
                            "ext/openssl/tests/sni_server.phpt"
                            "ext/openssl/tests/sni_server_key_cert.phpt"
                            "ext/posix/tests/posix_errno_variation1.phpt"))))))))))

(define-public php php-8.5)

(define %php-rewriter
  (package-input-rewriting/spec
   `(("php" . ,(const php-8.5)))))

(define-public lsp-plugins
  (%php-rewriter (@ (gnu packages music) lsp-plugins)))

(define-public easyeffects
  (%php-rewriter (@ (gnu packages audio) easyeffects)))
