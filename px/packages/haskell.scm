;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px packages haskell)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages xorg))

(define-public ghc-tzdata
  (package
    (name "ghc-tzdata")
    (version "0.2.20250115.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tzdata" version))
       (sha256
        (base32 "1h9sn9rngmvk2lqljcmysq9m7nwxlscm65rbzjqik8apsnb9qa53"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tzdata")))
    (inputs (list ghc-vector))
    (native-inputs (list ghc-hunit ghc-tasty ghc-tasty-hunit ghc-tasty-th))
    (home-page "https://github.com/ysangkok/haskell-tzdata")
    (synopsis "Time zone database (as files and as a module)")
    (description
     "This package distributes the standard Time Zone Database in a cabal package,
so that it can be used in Haskell programs uniformly on all platforms.")
    (license license:asl2.0)))

(define-public ghc-tz
  (package
    (name "ghc-tz")
    (version "0.1.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tz" version))
       (sha256
        (base32 "1vqnfk656i6j3j1bf9lc36adziv52x1b2ccq6afp8cka1nay2mcd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tz")))
    (inputs (list ghc-data-default ghc-tzdata ghc-vector))
    (native-inputs (list ghc-hunit
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-th
                         ghc-quickcheck
                         ghc-tasty-quickcheck))
    (arguments
     `(#:tests? #f  ; Tests require /usr/share/zoneinfo
       #:cabal-revision ("9"
                         "0a2i820w6zpf0vi4ammi4jsq80h072abd5czsxjmisjkwz2rrajp")))
    (home-page "https://github.com/ysangkok/haskell-tz")
    (synopsis "Efficient time zone handling")
    (description
     "A library that can read time zone info files (Olson files), and does
time zone conversions in a pure and efficient way.")
    (license license:asl2.0)))

(define-public ghc-attoparsec-aeson
  (package
    (name "ghc-attoparsec-aeson")
    (version "2.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "attoparsec-aeson" version))
       (sha256
        (base32 "1a86x493mrr7h468imcdjahxfvl2rrg6b6cygvzxja046cfgnjmk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "attoparsec-aeson")))
    (inputs (list ghc-aeson))
    (home-page "https://github.com/haskell/aeson")
    (synopsis "Parsing of aeson's Value with attoparsec")
    (description
     "Compatibility package that re-exports Data.Aeson.Parser from aeson.")
    (license license:bsd-3)))

(define-public ghc-terminal-progress-bar
  (package
    (name "ghc-terminal-progress-bar")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "terminal-progress-bar" version))
       (sha256
        (base32 "0li0w2wlbmq7mrg57ddnd33vqwndlxyl501bp0wkwyy9k14xmjgy"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "terminal-progress-bar")))
    (inputs (list ghc-terminal-size))
    (native-inputs (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (home-page "https://github.com/roelvandijk/terminal-progress-bar")
    (synopsis "A progress bar in the terminal")
    (description
     "This package implements a progress bar that is displayed in a terminal.")
    (license license:bsd-3)))

(define-public ghc-bytestring-progress
  (package
    (name "ghc-bytestring-progress")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bytestring-progress" version))
       (sha256
        (base32 "140dn6zyc1ka8vjxwd6zzc3mwd651zrawcwk3d5ipfxrgddf9bws"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bytestring-progress")))
    (inputs (list ghc-terminal-progress-bar))
    (home-page "http://github.com/acw/bytestring-progress")
    (synopsis "A library for tracking the consumption of a lazy ByteString")
    (description
     "Track how fast a ByteString is being consumed, useful for progress reporting.")
    (license license:bsd-3)))

(define-public arbtt
  (package
    (name "arbtt")
    (version "0.12.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "arbtt" version))
       (sha256
        (base32 "1wgc17fwnsqff8amwvv6jymhwjqrw653simv6gyxx89s1cla954d"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "arbtt")))
    (inputs (list ghc-utf8-string
                  ghc-strict
                  ghc-aeson
                  ghc-attoparsec-aeson
                  ghc-tz
                  ghc-x11
                  ghc-pcre-light
                  ghc-terminal-progress-bar
                  ghc-bytestring-progress
                  ghc-conduit
                  ghc-attoparsec
                  ghc-resourcet
                  ghc-unliftio-core
                  ;; System libraries for X11 support
                  libx11
                  libxscrnsaver
                  pcre))
    (native-inputs (list ghc-tasty ghc-tasty-golden ghc-tasty-hunit
                         ghc-process-extras))
    (arguments
     `(#:tests? #f  ; Tests require /usr/share/zoneinfo and built executables
       #:cabal-revision ("1"
                         "1kbma0ssjg8pd1rk9vi2bi7gy0fkfqpd6syh9271z350avgiwhf9")))
    (home-page "http://arbtt.nomeata.de/")
    (synopsis "Automatic Rule-Based Time Tracker")
    (description
     "arbtt is a background daemon that stores which windows are open, which one
has the focus and how long since your last action, and stores this.  It is
also a program that will, based on expressive rules you specify, derive what
you were doing, and what for.  WARNING: The log file might contain very
sensitive private data.  Make sure you understand the consequences of a
full-time logger and be careful with this data.")
    (license license:gpl2)))
