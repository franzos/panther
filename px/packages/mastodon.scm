;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Fakhri Sajadi <f.sajadi@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages mastodon)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages curl))

(define-public mastodonpp
  (package
    (name "mastodonpp")
    (version "0.5.7")
    ;; Upstream's own host, schlomp.space, is gone: it answers with a 1 kB
    ;; parking page under an unrelated expired certificate, which is what the
    ;; old url-fetch was silently hashing. The GitHub mirror is the surviving
    ;; copy, and git-fetch pins a commit rather than a Gitea-generated archive
    ;; whose bytes were never stable anyway.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tastytea/mastodonpp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cr780583h2grb3cpdy06m80k5j6c3xcsy6dfbh8a4ycq6vrsavc"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("curl" ,curl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://schlomp.space/tastytea/mastodonpp")
    (synopsis "C++ library for working with Mastodon REST API")
    (description
     "Mastodonpp is a C++ wrapper for the Mastodon API.
You submit an API call and get the raw JSON that you can then transform into easy to use abstractions.")
    (license license:expat)))