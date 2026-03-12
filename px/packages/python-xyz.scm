;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages python-xyz)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
  #:use-module (guix utils)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages django)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages check)
  #:use-module (gnu packages web)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (px packages framework))

(define-public python2-pymongo
  (package-with-python2 python-pymongo))

(define-public python-pyro5
  (package
    (name "python-pyro5")
    (version "5.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pyro5" version))
       (sha256
        (base32 "1qpjyh7l5l9p6mhsb77m84ch6hkvr74hay4ldyi0rnj1qdbnjvk1"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-serpent" ,python-serpent)))
    (home-page "https://github.com/irmen/Pyro5")
    (synopsis "Remote object communication library, fifth major version")
    (description "Remote object communication library, fifth major version")
    (license license:expat)))

(define-public python-sdnotify
  (package
    (name "python-sdnotify")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sdnotify" version))
       (sha256
        (base32 "1wdrdg2j16pmqhk0ify20s5pngijh7zc6hyxhh8w8v5k8v3pz5vk"))))
    (build-system python-build-system)
    (home-page "https://github.com/bb4242/sdnotify")
    (synopsis
     "A pure Python implementation of systemd's service notification protocol (sd_notify)")
    (description
     "A pure Python implementation of systemd's service notification protocol (sd_notify)")
    (license #f)))

(define-public python-survey
  (package
    (name "python-survey")
    (version "3.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "survey" version))
       (sha256
        (base32 "1qpg5alg6m9kfj5y90chgr66g0f33r3xqrrx2d496vipwsm3vbsc"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-wrapio" ,python-wrapio)))
    (home-page "https://github.com/Exahilosys/survey")
    (synopsis "A simple library for creating beautiful interactive prompts.")
    (description
     "A simple library for creating beautiful interactive prompts.")
    (license license:expat)))

(define-public python-wrapio
  (package
    (name "python-wrapio")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wrapio" version))
       (sha256
        (base32 "1s696hblap6qz8pc6zspg75bnvh7d48ww1qh1d8d5zjrnwhiqjh9"))))
    (build-system python-build-system)
    (home-page "https://github.com/Exahilosys/wrapio")
    (synopsis "Handling event-based streams.")
    (description "Handling event-based streams.")
    (license license:expat)))

(define-public python-types-requests
  (package
    (name "python-types-requests")
    (version "2.25.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "types-requests" version))
       (sha256
        (base32 "1vh203dppi6457lwv7z46dc8rpanjlahk4v3394nq1jwyp0425g2"))))
    (build-system python-build-system)
    (home-page "https://github.com/python/typeshed")
    (synopsis "Typing stubs for requests")
    (description "Typing stubs for requests")
    (license #f)))

(define-public python-mail-parser
  (package
    (name "python-mail-parser")
    (version "3.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mail-parser" version))
       (sha256
        (base32 "11q384z2pa3i4w0d07mnqd25r16sdgvgd8m75ysff6w0kr3vhxvm"))))
    (build-system python-build-system)
    (inputs `(("python-simplejson" ,python-simplejson)
              ("python-six" ,python-six)))
    (home-page "https://github.com/SpamScope/mail-parser")
    (synopsis
     "mail-parser is not only a wrapper for email Python Standard Library.")
    (description
     "mail-parser is not only a wrapper for email Python Standard Library.
It give you an easy way to pass from raw mail to Python object that you can
use in your code.")
    (license license:asl2.0)))

(define-public python-exitstatus
  (package
    (name "python-exitstatus")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "exitstatus" version))
       (sha256
        (base32 "193x2775ffy9qkaz267g2qjagd132xprh5nv4iq7a4kq61a26yjf"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "")
    (synopsis "")
    (description "todo.")
    (license license:psfl)))

(define-public python-requests-cache
  (package
    (name "python-requests-cache")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/reclosedev/requests-cache/archive/v" version
             ".tar.gz"))
       (sha256
        (base32 "05r26hb1ck0q4ffm334x078nq97hz6cg0nr8nlw1vvh9rl4g0ikq"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs `(("python-requests" ,python-requests)))
    (home-page "https://github.com/reclosedev/requests-cache")
    (synopsis
     "Python-requests-cache is a transparent persistent cache for python-requests (version >= 1.1.0) library.")
    (description
     "Python-requests-cache uses python-requests, so it needs an available installation of python-requests.")
    (license license:expat)))

(define-public python-etherscan
  (package
    (name "python-etherscan")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/d0/7a/d2c0cc7ce6c54854931f0e069997f1b82dafeff1bdd302e34153ccca9f59/etherscan-"
             version ".tar.gz"))
       (sha256
        (base32 "1qqgiy6q8kdsl3gvclgvbnzni8wm3yccsxsq85dlc7mw2dfzhapp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs `(("python-requests-cache" ,python-requests-cache)))
    (home-page "https://github.com/neoctobers/etherscan")
    (synopsis "Python-etherscan provides a wrapper for Etherscan.io API.")
    (description
     "Python-etherscan uses python-requests-cache, so it needs an available installation of python-requests-cache.")
    (license license:expat)))

(define-public python-block-io
  (package
    (name "python-block-io")
    (version "1.1.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/BlockIo/block_io-python/archive/" version
             ".tar.gz"))
       (sha256
        (base32 "15n8qapdlx8n9f27vj9qr34wgfahwgnqwh039plc6abs4flqjjrb"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (propagated-inputs `(("python-requests" ,python-requests)
                         ("python-pycryptodome" ,python-pycryptodome)
                         ("python-ecdsa" ,python-ecdsa)
                         ("python-six" ,python-six)
                         ("python-base58" ,python-base58)))
    (home-page "https://github.com/BlockIo/block_io-python")
    (synopsis
     "This Python package is the official reference client for the Block.io payments API.")
    (description "Please use Python2.7+. Also compatible with Python 3.0+.")
    (license license:expat)))

(define-public python-click-8
  (package
    (inherit python-click)
    (name "python-click")
    (version "8.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "click" version))
       (sha256
        (base32 "0ymdyf37acq4qxh038q0xx44qgj6y2kf0jd0ivvix6qij88w214c"))))))

(define-public python-persist-queue
  (package
    (name "python-persist-queue")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "persist-queue" version))
       (sha256
        (base32 "0xhvj26jkc4fk0yjzn47is6wh8figyp5cralj8d56r4bidn78wp2"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/peter-wangxu/persist-queue")
    (synopsis "A thread-safe disk based persistent queue in Python.")
    (description
     "persist-queue implements a file-based queue and a serial of sqlite3-based queues")
    (license license:bsd-2)))

(define-public python-supervisor
  (package
    (name "python-supervisor")
    (version "4.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "supervisor" version))
       (sha256
        (base32 "04mw7vnhzizzprk83h1k06djrwpv0zxial8s52983i933ap1nxil"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs (list python-setuptools))
    (native-inputs (list python-pytest python-pytest-cov))
    (home-page "http://supervisord.org/")
    (synopsis "A system for controlling process state under UNIX")
    (description
     "This package provides a system for controlling process state under UNIX")
    (license #f)))

(define-public python-takethetime
  (package
    (name "python-takethetime")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "TakeTheTime" version))
       (sha256
        (base32 "1y9gzqb9l1f2smx8783ccjzjvby5mphshgrfks7s75mml59h9qyv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/ErikBjare/TakeTheTime")
    (synopsis "Take The Time, a time-taking library for Python")
    (description "Take The Time, a time-taking library for Python.")
    (license license:expat)))

(define-public python-ckcc-protocol
  (package
    (name "python-ckcc-protocol")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ckcc-protocol" version))
       (sha256
        (base32 "1rgcpckyz38nwx8szh8cdgcny9m82d35pgm619ca93ihwg9x94yd"))))
    (build-system python-build-system)
	(inputs (list python-hidapi
	              python-ecdsa
				  python-pyaes
				  python-click))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/Coldcard/ckcc-protocol")
    (synopsis "Python library for Coldcard")
    (description "Python library and command line tool for
communicating with your Coldcard over USB")
    (license license:expat)))

(define-public px-python-shared
  (package
    (name "px-python-shared")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "08bf7xa0qsnj3i3ss89hqpgakazk0ajz1jiankwgfm8bbzk2j1bv"))))
    (build-system python-build-system)
    (arguments
      (list #:tests? #f
            #:phases
            #~(modify-phases %standard-phases
                (delete 'sanity-check))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("python-requests" ,python-requests)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Stuff that's shared across px-projects.")
    (description "Stuff like well known applications that's shared across px-projects.")
    (license license:expat)))

;; etesync-dav: Requirement.parse('Flask-WTF<1.0.0,>=0.14.2'), {'etesync-dav'})
(define-public python-flask-wtf-0.15.1
  (package
    (name "python-flask-wtf")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-WTF" version))
       (sha256
        (base32 "1p7jzxa3xckg13z1v9mck576m977h4qfczs3ag12sc4iz22p25zz"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "-vv"))))))
    (propagated-inputs (list python-flask python-itsdangerous python-wtforms
                             python-flask-babelex))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/lepture/flask-wtf")
    (synopsis "Simple integration of Flask and WTForms")
    (description "Flask-WTF integrates Flask and WTForms, including CSRF, file
upload, and reCAPTCHA.")
    (license license:bsd-3)))

(define-public python-speaklater
  (package
    (name "python-speaklater")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "speaklater" version))
       (sha256
        (base32 "1ab5dbfzzgz6cnz4xlwx79gz83id4bhiw67k1cgqrlzfs0va7zjr"))))
    (build-system python-build-system)
    (home-page "http://github.com/mitsuhiko/speaklater")
    (synopsis
     "implements a lazy string for python useful for use with gettext")
    (description
     "implements a lazy string for python useful for use with gettext")
    (license #f)))

;; python-flask-wtf tests require python-flask-babelex
(define-public python-flask-babelex
  (package
    (name "python-flask-babelex")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-BabelEx" version))
       (sha256
        (base32 "09yfr8hlwvpgvq8kp1y7qbnnl0q28hi0348bv199ssiqx779r99r"))))
    (build-system python-build-system)
    (propagated-inputs (list python-babel python-flask python-jinja2
                             python-speaklater))
    (home-page "http://github.com/mrjoes/flask-babelex")
    (synopsis "Adds i18n/l10n support to Flask applications")
    (description "Adds i18n/l10n support to Flask applications")
    (license license:bsd-3)))

(define-public state-massage
  (package
    (name "state-massage")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "123n2lry0b1zfbs1356vci7xiqs5wmnsl016k90ql39sxkml94fx"))))
    (build-system python-build-system)
    (arguments
      (list #:tests? #f
            #:phases
            #~(modify-phases %standard-phases
                (delete 'sanity-check))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Massages the state of your system.")
    (description "Basically ansible, minus a lot of the features, plus speed.")
    (license license:bsd-3)))

(define-public python-polyfactory
  (package
    (name "python-polyfactory")
    (version "2.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "polyfactory" version))
       (sha256
        (base32 "0ykkn7jmj5w0z1l9llbjrdy3yqrmi3j5njq1ri2df5953flxpn56"))))
    (build-system pyproject-build-system)
    (arguments
      (list #:tests? #f))
    (propagated-inputs (list python-faker python-typing-extensions))
    (native-inputs (list python-hatchling))
    (home-page #f)
    (synopsis "Mock data generation factories")
    (description "Mock data generation factories.")
    (license license:expat)))

(define-public python-litestar-htmx
  (package
    (name "python-litestar-htmx")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "litestar_htmx" version))
       (sha256
        (base32 "026ajqb24jxfkvbndh5iif0wf909535wxrdyr2ziik5qiq03f9ds"))))
    (build-system pyproject-build-system)
    (arguments
      (list #:tests? #f))
    (native-inputs (list python-hatchling))
    (home-page #f)
    (synopsis "HTMX Integration for Litesstar")
    (description "HTMX Integration for Litesstar.")
    (license license:expat)))

(define-public python-litestar
  (package
    (name "python-litestar")
    (version "2.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "litestar" version))
       (sha256
        (base32 "0dssvxyqrnnphh2as393fx9ilpr90jzkqw5i43s0kgqjwkkj0cyy"))))
    (build-system pyproject-build-system)
    (arguments
      (list #:tests? #f))
    (propagated-inputs (list python-anyio
                             python-click
                             python-exceptiongroup
                             python-httpx
                             python-importlib-metadata
                             python-importlib-resources
                             python-litestar-htmx
                             python-msgspec
                             python-multidict
                             python-multipart
                             python-polyfactory
                             python-pyyaml
                             python-rich
                             python-rich-click
                             python-typing-extensions))
    (native-inputs (list python-hatchling))
    (home-page #f)
    (synopsis
     "Litestar - A production-ready, highly performant, extensible ASGI API Framework")
    (description
     "Litestar - A production-ready, highly performant, extensible ASGI API Framework.")
    (license license:expat)))

(define-public python-sentry-sdk-2
  (package
    (name "python-sentry-sdk")
    (version "2.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sentry_sdk" version))
       (sha256
        (base32 "1iq2kk56wbpfvbd266bnah8azd0zv7vcxrlmk98j26jm86f037km"))))
    (build-system pyproject-build-system)
    (arguments
      (list #:tests? #f))
    (propagated-inputs (list python-certifi python-urllib3))
    (native-inputs (list python-litestar python-setuptools python-wheel))
    (home-page "https://github.com/getsentry/sentry-python")
    (synopsis "Python client for Sentry (https://sentry.io)")
    (description "Python client for Sentry (https://sentry.io).")
    (license license:expat)))

(define-public fw-fanctrl
  (package
    (name "fw-fanctrl")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/TamtamHero/fw-fanctrl/archive/refs/tags/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a26xrj0cfc007jbrzhs8a6dj2gv9qaw3zbds177b1ng41an76qy"))
       ;; Replace Python 3.12+ f-string syntax (PEP 701) with 3.11-compatible
       ;; form: config["key"] -> config['key'] inside f-strings.
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (substitute* "src/fw_fanctrl/Configuration.py"
              (("config\\[\"defaultStrategy\"\\]\\}' is not")
               "config['defaultStrategy']}' is not"))
            (substitute* "src/fw_fanctrl/dto/runtime_result/StatusRuntimeResult.py"
              (("self\\.configuration\\[\"data\"\\]\\[\"defaultStrategy\"\\]")
               "self.configuration['data']['defaultStrategy']")
              (("self\\.configuration\\[\"data\"\\]\\[\"strategyOnDischarging\"\\]")
               "self.configuration['data']['strategyOnDischarging']"))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-jsonschema ectool))
    (home-page "https://github.com/TamtamHero/fw-fanctrl")
    (synopsis "A service to better control Framework Laptop's fan(s)")
    (description
     "Fw-fanctrl is a simple Python CLI service that controls Framework Laptop's
fan(s) speed according to a configurable speed/temperature curve.")
    (license license:bsd-3)))
