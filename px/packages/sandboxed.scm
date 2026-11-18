;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages sandboxed)
  #:use-module (px packages document)
  #:use-module (px packages graphics)
  #:use-module (px sandbox))

(define-public papers-sandboxed
  (sandboxed papers
             (sandbox-policy
              (read-only-paths '(".cache/fontconfig"))
              (file-arguments? #t)
              (wayland? #t)
              (gpu? #t)
              (dbus-rules %portal-settings-dbus-rules))))

(define-public vitrine-sandboxed
  (sandboxed vitrine
             (sandbox-policy
              (read-only-paths '(".config/vitrine" "Pictures"))
              (writable-paths '(".local/share/vitrine" ".cache/vitrine"))
              (wayland? #t)
              (gpu? #t))))
