;;; Package Repository for GNU Guix
;;; Copyright © 2021-2026 Franz Geffke <mail@gofranz.com>

(define-module (px system common)
  #:use-module (guix gexp)
  #:use-module (guix channels)

  #:export (%gofranz-substitute-server-url
            ;; %nonguix-substitute-server-url
            %nonguix-mirror-substitute-server-url
            %guix-moe-substitute-server-url
            %gofranz-substitute-server-key
            %nonguix-substitute-server-key
            %guix-moe-substitute-server-key
            %pantherx-default-channels))

(define %gofranz-substitute-server-url
  "https://substitutes.guix.gofranz.com")

;; Official nonguix substitutes - temporarily disabled due to availability
;; issues; using guix.moe mirrors instead.
;; See https://ultrarare.space/en/posts/guix-build-farm/
;; (define %nonguix-substitute-server-url
;;   "https://substitutes.nonguix.org")

;; Transparent mirror of nonguix substitutes operated by guix.moe.  Retains the
;; original nonguix signing key, so %nonguix-substitute-server-key is reused
;; below.  Marked as a testing endpoint upstream (added 2026-04-03), revisit
;; if availability of substitutes.nonguix.org is restored.
(define %nonguix-mirror-substitute-server-url
  "https://cache-test.guix.moe")

;; guix.moe build farm head node (nuporta, Finland).  Builds upstream Guix
;; substitutes signed with its own key (see %guix-moe-substitute-server-key).
(define %guix-moe-substitute-server-url
  "https://cache-fi.guix.moe")

(define %gofranz-substitute-server-key
  (plain-file "substitutes.guix.gofranz.com.pub"
   "(public-key
 (ecc
  (curve Ed25519)
  (q #0096373009D945F86C75DFE96FC2D21E2F82BA8264CB69180AA4F9D3C45BAA47#)
  )
 )
"))

(define %nonguix-substitute-server-key
  (plain-file "substitutes.nonguix.org.pub"
   "(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
  )
 )
"))

(define %guix-moe-substitute-server-key
  (plain-file "cache-fi.guix.moe.pub"
   "(public-key
 (ecc
  (curve Ed25519)
  (q #552F670D5005D7EB6ACF05284A1066E52156B51D75DE3EBD3030CD046675D543#)
  )
 )
"))

(define %pantherx-default-channels
  (append (list (channel
                  (name 'pantherx)
                  (branch "master")
                  (url "https://codeberg.org/gofranz/panther.git")
                  (introduction
                   (make-channel-introduction
                    "54b4056ac571611892c743b65f4c47dc298c49da"
                    (openpgp-fingerprint
                     "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB")))))
          %default-channels))
