;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages password-utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system pyproject)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby-xyz)
  #:use-module (gnu packages xdisorg))

(define-public keepassxc-2.8
  (package
    (inherit keepassxc)
    (version "2.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/keepassxreboot/keepassxc")
             (commit version)))
       (file-name (git-file-name "keepassxc" version))
       (sha256
        (base32 "19v1bkjz4ra7akfmz3f8skydcrz2dvnsgr52dw41xp8xqb3yi1lv"))))
    (arguments
     (substitute-keyword-arguments (package-arguments keepassxc)
       ((#:qtbase _ #f) qtbase)
       ((#:configure-flags _)
        #~(append
           (list "-DKEEPASSXC_BUILD_TYPE=Release"
                 "-DKPXC_FEATURE_UPDATES=OFF")
           #$(if (member (%current-system)
                         (package-transitive-supported-systems
                          ruby-asciidoctor/minimal))
                 #~'()
                 #~(list "-DKPXC_FEATURE_DOCS=OFF"))))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'record-clipboard-programs
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "src/cli/Utils.cpp"
                  (("QStringLiteral\\(\"xclip\"\\)")
                   (string-append "QString::fromUtf8(\""
                                  (search-input-file inputs "bin/xclip")
                                  "\")"))
                  (("QStringLiteral\\(\"wl-copy\"\\)")
                   (string-append "QString::fromUtf8(\""
                                  (search-input-file inputs "bin/wl-copy")
                                  "\")")))
                (substitute* "src/gui/osutils/nixutils/NixUtils.cpp"
                  (("QStringLiteral\\(\"wl-copy\"\\)")
                   (string-append "QString::fromUtf8(\""
                                  (search-input-file inputs "bin/wl-copy")
                                  "\")")))))))))
    (native-inputs
     (modify-inputs (package-native-inputs keepassxc)
       (prepend pkg-config)
       (replace "qttools" qttools)))
    (inputs
     (modify-inputs (package-inputs keepassxc)
       (delete "qtx11extras")
       (prepend keyutils libxkbcommon)
       (replace "qtsvg" qtsvg)
       (replace "qtwayland" qtwayland)))))

(define-public keychain
  (package
    (name "keychain")
    (version "3.0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/danielrobbins/keychain")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0raggqwy8iq3njdl31pic3qv7jsdmqa5wm4lzvg55yzfay8x7b9h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; The e2e suites drive real agents, which the build sandbox can't host.
      #:test-flags
      #~(list "--ignore-glob=tests/*e2e*.py"
              "--ignore=tests/test_distributions.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-man-page
            (lambda _
              (invoke "make" "install-man" "PYTHON=python3"
                      (string-append "PREFIX=" #$output)))))))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list procps))
    (home-page "https://github.com/danielrobbins/keychain")
    (synopsis "Manager for ssh-agent and gpg-agent with key caching")
    (description
     "Keychain starts @command{ssh-agent} and @command{gpg-agent} once per
system and lets shells and scripts reuse them, so private keys only need to be
unlocked once per login.  It is usually run from shell profiles like
@file{~/.bash_profile}, but also works with non-interactive shells.")
    (license license:gpl3)))
