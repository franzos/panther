;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages ci)
  #:use-module (gnu packages bash)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public gitlab-runner
  (package
    (name "gitlab-runner")
    (version "v18.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab-runner-downloads.s3.amazonaws.com/"
             version "/binaries/gitlab-runner-linux-amd64"))
       (sha256
        (base32 "0zqyzz398sra3wk7bh24hbb1zmzxb3gywqr0529l2xl677kg8hd9"))))
    (build-system trivial-build-system)
    (supported-systems '("x86_64-linux" "i686-linux")) ;; As long as we download binaries
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((src (assoc-ref %build-inputs "source"))
                          (bash (string-append (assoc-ref %build-inputs "bash")
                                               "/bin/sh"))
                          (out (assoc-ref %outputs "out"))
                          (target (string-append out "/bin/gitlab-runner")))
                     (setenv "PATH" bash)
                     (mkdir-p (string-append out "/bin"))
                     (copy-file src target)
                     (chmod target #o755) #t))))
    (home-page "https://docs.gitlab.com/runner/")
    (native-inputs `(("bash" ,bash)))
    (synopsis "GitLab Runner")
    (description "GitLab Runner is the open source project that is used 
to run your jobs and send the results back to GitLab.")
    (license license:expat)))
