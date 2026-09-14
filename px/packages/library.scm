;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages library)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages qt)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public px-auth-library-cpp
  (package
    (name "px-auth-library-cpp")
    (version "0.0.35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/px-auth-library-cpp_"
                           version ".tgz"))
       (sha256
        (base32 "0hqhpxn31wx2m81ard1jdfzcmhcbsbjncx6y01ckn1lln8gjpliq"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f
      #:qtbase qtbase))
    (inputs (list qtbase))
    (home-page "https://www.pantherx.org/")
    (synopsis "CIBA, QR and Device Authentication")
    (description "CIBA and QR flow and device authentication library")
    (license license:expat)))

;; Qt is ABI-visible here, so consumers must link the variant matching their own Qt.
(define-public px-auth-library-cpp-qt5
  (package
    (inherit px-auth-library-cpp)
    (name "px-auth-library-cpp-qt5")
    (arguments
     (list
      #:tests? #f
      #:qtbase qtbase-5))
    (inputs (modify-inputs (package-inputs px-auth-library-cpp)
              (replace "qtbase" qtbase-5)))))
