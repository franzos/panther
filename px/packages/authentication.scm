;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages authentication)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages java))

(define-public keycloak
  (package
    (name "keycloak")
    (version "26.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/keycloak/keycloak/releases/download/"
             version "/keycloak-" version ".tar.gz"))
       (sha256
        (base32 "0i4f47ys07j1i0dpmlycic2r0ns5as99mpqbswybi40iri2d08fv"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "libexec/keycloak/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-launchers
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out #$output)
                     (home (string-append out "/libexec/keycloak"))
                     (bin (string-append out "/bin"))
                     (java (search-input-file inputs "/bin/java"))
                     (java-home (dirname (dirname java))))
                ;; The launchers resolve their install dir via readlink -f
                ;; on $0, so exec'ing them by absolute path keeps that intact.
                (for-each delete-file
                          (find-files (string-append home "/bin") "\\.bat$"))
                (mkdir-p bin)
                (for-each
                 (lambda (script)
                   (let ((target (string-append home "/bin/" script))
                         (wrapper (string-append bin "/" script)))
                     (call-with-output-file wrapper
                       (lambda (port)
                         (format port "#!~a
export JAVA_HOME=\"~a\"
export PATH=\"~a/bin:$PATH\"
exec \"~a\" \"$@\"
"
                                 (search-input-file inputs "/bin/bash")
                                 java-home java-home target)))
                     (chmod wrapper #o555)))
                 '("kc.sh" "kcadm.sh" "kcreg.sh"))))))))
    (inputs (list bash-minimal openjdk21))
    (home-page "https://www.keycloak.org/")
    (synopsis "Identity and access management server")
    (description
     "Keycloak is an open source identity and access management solution
providing single sign-on, user federation, identity brokering, and social
login.  This package repackages the upstream prebuilt distribution and runs
on OpenJDK 21.")
    (license license:asl2.0)))
