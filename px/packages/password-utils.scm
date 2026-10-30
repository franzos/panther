;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages password-utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pkg-config)
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
