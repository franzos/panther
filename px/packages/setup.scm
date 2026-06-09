;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages setup)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
  #:use-module (px packages common)
  #:use-module (px self))

(define-public px-install
  (package
    (name "px-install")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1gm0b96cqa0w6mwx8283hc9l7wdjr37fsb7l60igvnjhj674n7nx"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("python-requests" ,python-requests)
              ("python-tqdm" ,python-tqdm)
              ("python-pytz" ,python-pytz)
              ("python-qrcode" ,python-qrcode)
              ("python-py-cpuinfo" ,python-py-cpuinfo)
              ("python-urllib3" ,python-urllib3)))
    (native-inputs (list pkg-config python-setuptools))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX OS Installer")
    (description "A command line driven installer with sane defaults.")
    (license license:gpl3)))

(define-public guix-install
  (package
    (name "guix-install")
    (version "0.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/franzos/guix-install")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fa0x2ijjgplb5s0ivsdrlsm53i9v0m1mc5xhchw6i8imab6jdgy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       ;; Workspace also holds the iced GUI crate; build only the CLI here.
       #:cargo-build-flags '("--release" "-p" "guix-install")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "target/release/guix-install" bin)))))))
    (inputs (px-cargo-inputs 'guix-install))
    (home-page "https://github.com/franzos/guix-install")
    (synopsis "Guix System installer")
    (description
     "Guix System installer.  Boot a Guix ISO, run one binary, get a working
system---libre Guix, Nonguix, PantherX, or an enterprise config from a
server.")
    (license license:gpl3+)))