;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages go)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages nss)
  #:use-module (ice-9 match)
  #:export (go-git-reference
            go-fetch-vendored))

;;
;; Go vendored fetch (for Go apps with many dependencies)
;;
;; Workaround: upstream Guix requires packaging every Go dependency
;; individually. This fetches source and runs `go mod vendor` to bundle
;; all dependencies into a single tarball.

(define-record-type* <go-git-reference>
  go-git-reference make-go-git-reference
  go-git-reference?
  (url go-git-reference-url)
  (commit go-git-reference-commit)
  (sha go-git-reference-sha256))

(define* (go-fetch-vendored uri hash-algorithm hash-value name #:key system)
  (let ((src
         (match uri
           (($ <go-git-reference> url commit sha)
            (origin
              (method git-fetch)
              (uri (git-reference
                    (url url)
                    (commit commit)))
              (sha256 sha)))))
        (name (or name "go-git-checkout")))
    (gexp->derivation
     (string-append name "-vendored.tar.gz")
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           (let ((inputs (list
                          #+go-1.25
                          #+tar
                          #+bzip2
                          #+gzip)))
             (set-path-environment-variable "PATH" '("/bin") inputs))
           (mkdir "source")
           (chdir "source")
           (copy-recursively #$src "."
                             #:keep-mtime? #t)
           (for-each (lambda (f)
                       (false-if-exception (make-file-writable f)))
                     (find-files "."))
           (setenv "GOCACHE" "/tmp/gc")
           (setenv "GOMODCACHE" "/tmp/gmc")
           (setenv "SSL_CERT_DIR" #+(file-append nss-certs "/etc/ssl/certs/"))
           (invoke "go" "mod" "vendor")
           (invoke "tar" "czvf" #$output
                   "--mtime=@0"
                   "--owner=root:0"
                   "--group=root:0"
                   "--sort=name"
                   "--hard-dereference"
                   ".")))
     #:hash hash-value
     #:hash-algo hash-algorithm)))
