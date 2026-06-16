;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages aidc)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages check)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages stb)
  #:use-module (guix build-system cmake))

(define-public zxing-cpp-08978e2
  (let ((revision "0")
        (commit "08978e2c6cf3befb54320c9ca8ab33f7775e6ec4"))
    (package
      (name "zxing-cpp-08978e2")
      (version (git-version "1.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nu-book/zxing-cpp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "13wzs7dhb7vv7j9i40gib4ns9bfsp4kkdx4gkgykgqqb4imvvvvp"))))
      (arguments
       ;; GCC 13+ no longer transitively includes <cstdint>, so force-include it
       ;; for the textcodec sources that use uint8_t without including it.
       '(#:tests? #f
         #:configure-flags '("-DBUILD_BLACKBOX_TESTS=OFF"
                             "-DCMAKE_CXX_FLAGS=-include cstdint")))
      (build-system cmake-build-system)
      (native-inputs (list fmt-8 googletest))
      (synopsis "C++ port of ZXing")
      (description "ZXing-CPP is a barcode scanning library.")
      (home-page "https://github.com/nu-book/zxing-cpp")
      (license license:asl2.0))))

(define-public scodes
  (let ((revision "0")
        (commit "fac97f2800db37c7f018813c16038799e6e224cd"))
    (package
      (name "scodes")
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/scytheStudio/SCodes")
               (commit commit)))
         (file-name (git-file-name name version))
         (patches (search-patches "scodes-zxing-path.patch"))
         (sha256
          (base32 "0vbbz8z04c3w370vbjq85xd9kkykcb0xm3rrkcb71k0ycrrhc183"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ;no test suite
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'zxing-path-patch
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let ((zxing-cpp-08978e2 (assoc-ref inputs
                                                  "zxing-cpp-08978e2")))
                          (chdir "src")
                          (substitute* "CMakeLists.txt"
                            (("/usr/local/include/ZXing")
                             (string-append zxing-cpp-08978e2 "/include/ZXing")))
                          (invoke "cat" "CMakeLists.txt") #t))))))
      (inputs (list qtbase-5
                    qtdeclarative-5
                    qtmultimedia-5
                    qtquickcontrols-5
                    qtquickcontrols2-5
                    stb-image
                    stb-image-write
                    zxing-cpp-08978e2))
      (synopsis "Qt & Qml wrapper for ZXing library")
      (description "Qt & Qml wrapper for ZXing library")
      (home-page "https://github.com/scytheStudio/SCodes")
      (license license:asl2.0))))
