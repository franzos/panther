;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages logging)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages check))

(define-public spdlog-fmt
  (package
    (name "spdlog")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gabime/spdlog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zgdmdgnp2y36jrlk85d4fiyjkjd6anly8pambyc3f3v6sg02zyy"))))
    (build-system cmake-build-system)
    ;; TODO run benchmark. Currently not possible, as adding
    ;; (gnu packages benchmark) forms a dependency cycle
    (arguments
     (list #:configure-flags
           #~(list "-DSPDLOG_BUILD_BENCH=OFF"
                   "-DSPDLOG_BUILD_SHARED=ON"
                   #$@(if (%current-target-system)
                          '()
                          '("-DSPDLOG_BUILD_TESTS=ON")))))
    (native-inputs (list catch2-3.5))
    (home-page "https://github.com/gabime/spdlog")
    (synopsis "Fast C++ logging library")
    (description "Spdlog is a very fast header-only/compiled C++ logging
library.")
    ;; spdlog is under Expat license, but the bundled fmt library in
    ;; "include/spdlog/fmt/bundled" is under BSD 2 clause license.
    (license (list license:expat license:bsd-2))))