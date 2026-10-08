;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages framework)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libftdi))

(define-public ectool
  (let ((commit "38f92b1f2773c30c0d911453051619c0a07773d3")
        (revision "1"))
  (package
    (name "ectool")
    (version (git-version "1.0.0" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.howett.net/DHowett/ectool")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "006ms61dpr7z1ksb426dvf1h5iszdf39kq9spl7wz8l483pvnd8m"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "src/ectool" bin)
               #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libusb libftdi))
    (home-page "https://gitlab.howett.net/DHowett/ectool")
    (synopsis "ChromeOS EC Tool")
    (description
     "ECTool is a utility for interacting with the Embedded Controller (EC) 
      in ChromeOS devices and Framework laptops.")
    (license license:expat))))