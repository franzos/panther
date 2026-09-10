;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px services nss)
  #:use-module (gnu services)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages sqlite)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (nss-fhs-configuration
            nss-fhs-configuration?
            nss-fhs-configuration-nss
            nss-fhs-configuration-nspr
            nss-fhs-configuration-sqlite
            nss-fhs-configuration-directory
            nss-fhs-service-type))

;;;
;;; Expose the NSS libraries at /usr/lib/nss.
;;;
;;; Applications that load NSS through dlopen rather than by linking against it
;;; tend to hard-code the FHS locations.  Autofirma is one: it walks a fixed
;;; list of directories looking for libsoftokn3.so, and without a hit it cannot
;;; read certificates out of the Firefox or Chromium key stores.  /usr/lib/nss
;;; is the first entry on that list that a Guix system can populate.  Guix does
;;; the same thing for Dovecot, which hard-codes /usr/lib/dovecot.
;;;
;;; The lookup wants libsoftokn3.so, the NSPR libraries and libsqlite3.so in
;;; one directory, so this is a union of three packages rather than a symlink
;;; to nss alone.
;;;

(define-record-type* <nss-fhs-configuration>
  nss-fhs-configuration make-nss-fhs-configuration
  nss-fhs-configuration?
  (nss
   nss-fhs-configuration-nss
   (default nss))
  (nspr
   nss-fhs-configuration-nspr
   (default nspr))
  (sqlite
   nss-fhs-configuration-sqlite
   (default sqlite)))

(define (nss-fhs-configuration-directory config)
  "Return a directory holding the libraries CONFIG names, laid out the way an
FHS system lays out /usr/lib/nss."
  (match-record config <nss-fhs-configuration> (nss nspr sqlite)
    (computed-file
     "nss-fhs"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 ftw)
                        (ice-9 regex))

           (define (link-matching directory regexp)
             (for-each
              (lambda (file)
                (let ((destination (string-append #$output "/" file)))
                  ;; nss ships the FIPS checksums next to the modules they
                  ;; describe, so keep whichever name is seen first.
                  (unless (file-exists? destination)
                    (symlink (string-append directory "/" file) destination))))
              (scandir directory
                       (lambda (file)
                         (string-match regexp file)))))

           (mkdir #$output)
           (link-matching #$(file-append nss "/lib/nss") "\\.(so|chk)$")
           (link-matching #$(file-append nspr "/lib") "\\.so$")
           (link-matching #$(file-append sqlite "/lib") "^libsqlite3\\.so"))))))

(define (nss-fhs-activation config)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/usr/lib")
      (switch-symlinks "/usr/lib/nss"
                       #$(nss-fhs-configuration-directory config))))

(define nss-fhs-service-type
  (service-type
   (name 'nss-fhs)
   (description
    "Expose the @acronym{NSS, Network Security Services} libraries at
@file{/usr/lib/nss}, where applications that dlopen them by their FHS path can
find them.")
   (extensions
    (list (service-extension activation-service-type nss-fhs-activation)))
   (default-value (nss-fhs-configuration))))
