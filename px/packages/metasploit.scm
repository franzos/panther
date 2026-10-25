;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages metasploit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ruby)
  #:use-module ((gnu packages ruby-xyz) #:hide (ruby-rack-session ruby-ascii85 ruby-addressable ruby-ast ruby-bcrypt ruby-bcrypt-pbkdf ruby-benchmark ruby-bigdecimal ruby-coderay ruby-concurrent-ruby ruby-connection-pool ruby-crack ruby-crass ruby-daemons ruby-date ruby-debug ruby-domain-name ruby-ed25519 ruby-erubi ruby-faker ruby-faraday ruby-faraday-net-http ruby-hashdiff ruby-http-accept ruby-http-cookie ruby-httpclient ruby-i18n ruby-io-console ruby-ipaddr ruby-irb ruby-language-server-protocol ruby-msgpack ruby-mustermann ruby-net-imap ruby-net-protocol ruby-net-smtp ruby-net-ssh ruby-nio4r ruby-nokogiri ruby-nori ruby-octokit ruby-ostruct ruby-parallel ruby-parser ruby-pdf-reader ruby-pg ruby-prism ruby-pry ruby-psych ruby-public-suffix ruby-puma ruby-rack ruby-rack-protection ruby-rack-test ruby-rdoc ruby-regexp-parser ruby-reline ruby-rexml ruby-rubocop ruby-rubocop-ast ruby-rubyzip ruby-sinatra ruby-sqlite3 ruby-stringio ruby-thin ruby-tilt ruby-timecop ruby-ttfunk ruby-tzinfo ruby-tzinfo-data ruby-unicode-emoji ruby-useragent ruby-warden ruby-webmock ruby-webrick ruby-websocket-driver ruby-websocket-extensions ruby-xml-simple ruby-yard ruby-zeitwerk))
  #:use-module ((gnu packages ruby-check) #:hide (ruby-mini-portile-2 ruby-docile ruby-json ruby-mime-types ruby-mime-types-data ruby-minitest ruby-rspec-core ruby-rspec-expectations ruby-rspec-mocks ruby-rspec-support ruby-simplecov ruby-simplecov-html))
  #:use-module ((gnu packages rails) #:hide (ruby-actionpack ruby-actionview ruby-activemodel ruby-activerecord ruby-activesupport ruby-bootsnap ruby-rails-dom-testing ruby-rails-html-sanitizer ruby-railties ruby-rspec-rails)))


;; Pinned gem versions (match metasploit Gemfile.lock) overriding guix defaults

(define-public ruby-rack-session
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-rack-session))
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rack-session" version))
              (sha256 (base32 "1s7zcxlmg88a6dam4aqbgk9xkpy6dkdfqmmcszkkliy3q3w38m2r"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-mini-portile-2
  (package
    (inherit (@ (gnu packages ruby-check) ruby-mini-portile-2))
    (version "2.8.9")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mini_portile2" version))
              (sha256 (base32 "12f2830x7pq3kj0v8nz0zjvaw02sv01bqs1zwdrc04704kwcgmqc"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-ascii85
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-ascii85))
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "Ascii85" version))
              (sha256 (base32 "0nmyxpngg5rycyryhq9l9hapz1y3iqyflskyksxkqm0832a5vjqm"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-actionpack
  (package
    (inherit (@ (gnu packages rails) ruby-actionpack))
    (version "8.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "actionpack" version))
              (sha256 (base32 "0dabvb49acbwvy91587cbn36ghv3bsyl14a9aq4ll4nxfn4qdpn9"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-actionview
  (package
    (inherit (@ (gnu packages rails) ruby-actionview))
    (version "8.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "actionview" version))
              (sha256 (base32 "04ql6lpvdmrl5169y166pfr9w53c6f40jkgmn4ljgkzh7pkaj3vd"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-activemodel
  (package
    (inherit (@ (gnu packages rails) ruby-activemodel))
    (version "8.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "activemodel" version))
              (sha256 (base32 "1hjv2kmv7i0jk8zkng3pxa1kdd90qpgr3v60qvs764yw8qyq35n7"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-activerecord
  (package
    (inherit (@ (gnu packages rails) ruby-activerecord))
    (version "8.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "activerecord" version))
              (sha256 (base32 "1ri9l5v4601bxwrkl105k1ccxxg2wpvg6x94rwqr834irnv63cl9"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-activesupport
  (package
    (inherit (@ (gnu packages rails) ruby-activesupport))
    (version "8.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "activesupport" version))
              (sha256 (base32 "08ybmp63qrfaxq7bv7mvb4xvfb4fcylw2a0szankzkrpdbzi7wip"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-addressable
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-addressable))
    (version "2.9.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "addressable" version))
              (sha256 (base32 "1by7h2lwziiblizpd5yx87jsq8ppdhzvwf08ga34wzqgcv1nmpvz"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-ast
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-ast))
    (version "2.4.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ast" version))
              (sha256 (base32 "10yknjyn0728gjn6b5syynvrvrwm66bhssbxq8mkhshxghaiailm"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-bcrypt
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-bcrypt))
    (version "3.1.22")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bcrypt" version))
              (sha256 (base32 "0clhya4p8lhjj7hp31inp321wgzb0b5wbwppmya5sw1dikl7400z"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-bcrypt-pbkdf
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-bcrypt-pbkdf))
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bcrypt_pbkdf" version))
              (sha256 (base32 "04rb3rp9bdxn1y3qiflfpj7ccwb8ghrfbydh5vfz1l9px3fpg41g"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-benchmark
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-benchmark))
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "benchmark" version))
              (sha256 (base32 "1kicilpma5l0lwayqjb5577bm0hbjndj2gh150xz09xsgc1l1vyl"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-bigdecimal
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-bigdecimal))
    (version "3.3.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bigdecimal" version))
              (sha256 (base32 "0612spks81fvpv2zrrv3371lbs6mwd7w6g5zafglyk75ici1x87a"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-bootsnap
  (package
    (inherit (@ (gnu packages rails) ruby-bootsnap))
    (version "1.18.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bootsnap" version))
              (sha256 (base32 "0mdgj9yw1hmx3xh2qxyjc31y8igmxzd9h0c245ay2zkz76pl4k5c"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-coderay
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-coderay))
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "coderay" version))
              (sha256 (base32 "0jvxqxzply1lwp7ysn94zjhh57vc14mcshw1ygw14ib8lhc00lyw"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-concurrent-ruby
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-concurrent-ruby))
    (version "1.3.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "concurrent-ruby" version))
              (sha256 (base32 "1ipbrgvf0pp6zxdk5ascp6i29aybz2bx9wdrlchjmpx6mhvkwfw1"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-connection-pool
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-connection-pool))
    (version "2.5.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "connection_pool" version))
              (sha256 (base32 "02p7l47gvchbvnbag6kb4x2hg8n28r25ybslyvrr2q214wir5qg9"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-crack
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-crack))
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "crack" version))
              (sha256 (base32 "0zjcdl5i6lw508r01dym05ibhkc784cfn93m1d26c7fk1hwi0jpz"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-crass
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-crass))
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "crass" version))
              (sha256 (base32 "15djj19ynz3sbw54fsf8n7y3sha8a333f2mgvjfwhr46jhcqg1ll"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-daemons
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-daemons))
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "daemons" version))
              (sha256 (base32 "07cszb0zl8mqmwhc8a2yfg36vi6lbgrp4pa5bvmryrpcz9v6viwg"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-date
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-date))
    (version "3.4.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "date" version))
              (sha256 (base32 "0kz6mc4b9m49iaans6cbx031j9y7ldghpi5fzsdh0n3ixwa8w9mz"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-debug
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-debug))
    (version "1.11.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "debug" version))
              (sha256 (base32 "1wmfy5n5v2rzpr5vz698sqfj1gl596bxrqw44sahq4x0rxjdn98l"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-docile
  (package
    (inherit (@ (gnu packages ruby-check) ruby-docile))
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "docile" version))
              (sha256 (base32 "07pj4z3h8wk4fgdn6s62vw1lwvhj0ac0x10vfbdkr9xzk7krn5cn"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-domain-name
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-domain-name))
    (version "0.6.20240107")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "domain_name" version))
              (sha256 (base32 "0cyr2xm576gqhqicsyqnhanni47408w2pgvrfi8pd13h2li3nsaz"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-ed25519
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-ed25519))
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ed25519" version))
              (sha256 (base32 "01n5rbyws1ijwc5dw7s88xx3zzacxx9k97qn8x11b6k8k18pzs8n"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-erubi
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-erubi))
    (version "1.13.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "erubi" version))
              (sha256 (base32 "1naaxsqkv5b3vklab5sbb9sdpszrjzlfsbqpy7ncbnw510xi10m0"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-faker
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-faker))
    (version "3.5.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "faker" version))
              (sha256 (base32 "1z4vhl6lbd0la2j0ab05sz8wq5mqvpikzhjrc142z24x4zmgpl8s"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-faraday
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-faraday))
    (version "2.14.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "faraday" version))
              (sha256 (base32 "0vmb5wpiz7mf2r6vjvhvfp38rrfvqjzmv7m3max0s6v28j641d4v"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-faraday-net-http
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-faraday-net-http))
    (version "3.4.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "faraday-net_http" version))
              (sha256 (base32 "125m3qri52vwh5v9dhq0dkqxf8629cxrf99yyc01pva72wasyy0f"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-ffi
  (package
    (inherit (@ (gnu packages libffi) ruby-ffi))
    (version "1.16.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ffi" version))
              (sha256 (base32 "1yvii03hcgqj30maavddqamqy50h7y6xcn2wcyq72wn823zl4ckd"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-hashdiff
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-hashdiff))
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hashdiff" version))
              (sha256 (base32 "1lbw8lqzjv17vnwb9vy5ki4jiyihybcc5h2rmcrqiz1xa6y9s1ww"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-http-accept
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-http-accept))
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "http-accept" version))
              (sha256 (base32 "09m1facypsdjynfwrcv19xcb1mqg8z6kk31g8r33pfxzh838c9n6"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-httpclient
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-httpclient))
    (version "2.9.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "httpclient" version))
              (sha256 (base32 "1j4qwj1nv66v3n9s4xqf64x2galvjm630bwa5xngicllwic5jr2b"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-http-cookie
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-http-cookie))
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "http-cookie" version))
              (sha256 (base32 "19hsskzk5zpv14mnf07pq71hfk1fsjwfjcw616pgjjzjbi2f0kxi"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-i18n
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-i18n))
    (version "1.14.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "i18n" version))
              (sha256 (base32 "03sx3ahz1v5kbqjwxj48msw3maplpp2iyzs22l4jrzrqh4zmgfnf"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-io-console
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-io-console))
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "io-console" version))
              (sha256 (base32 "1jszj95hazqqpnrjjzr326nn1j32xmsc9xvd97mbcrrgdc54858y"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-ipaddr
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-ipaddr))
    (version "1.2.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ipaddr" version))
              (sha256 (base32 "0wmgwqv6c1kq8cxbxddllnrlh5jjmjw73i1sqbnvq55zzn3l0zyb"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-irb
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-irb))
    (version "1.15.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "irb" version))
              (sha256 (base32 "1fpxa2m83rb7xlzs57daqwnzqjmz6j35xr7zb15s73975sak4br2"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-json
  (package
    (inherit (@ (gnu packages ruby-check) ruby-json))
    (version "2.21.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "json" version))
              (sha256 (base32 "0shwgjqbj856mb6m9kgkpy08nhym2gdvc2yaprlimfmky9y3n78z"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-language-server-protocol
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-language-server-protocol))
    (version "3.17.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "language_server-protocol" version))
              (sha256 (base32 "1k0311vah76kg5m6zr7wmkwyk5p2f9d9hyckjpn3xgr83ajkj7px"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-mime-types
  (package
    (inherit (@ (gnu packages ruby-check) ruby-mime-types))
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mime-types" version))
              (sha256 (base32 "0mjyxl7c0xzyqdqa8r45hqg7jcw2prp3hkp39mdf223g4hfgdsyw"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-mime-types-data
  (package
    (inherit (@ (gnu packages ruby-check) ruby-mime-types-data))
    (version "3.2025.0924")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mime-types-data" version))
              (sha256 (base32 "0a27k4jcrx7pvb0p59fn1frh14iy087c2aygrdkmgwsrbshvqxpj"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-minitest
  (package
    (inherit (@ (gnu packages ruby-check) ruby-minitest))
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest" version))
              (sha256 (base32 "0mn7q9yzrwinvfvkyjiz548a4rmcwbmz2fn9nyzh4j1snin6q6rr"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-msgpack
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-msgpack))
    (version "1.8.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "msgpack" version))
              (sha256 (base32 "1fhpfl9fznn6wi39xh05crswigywvyiw3rn0cbjf7prmgfm9whcp"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-mustermann
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-mustermann))
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mustermann" version))
              (sha256 (base32 "123ycmq6pkivv29bqbv79jv2cs04xakzd0fz1lalgvfs5nxfky6i"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-net-imap
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-net-imap))
    (version "0.6.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-imap" version))
              (sha256 (base32 "0q1nq8mx22v9sn1296s45nm6dni42y3m3s1f4pg5dn3mx4xmgjdm"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-net-protocol
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-net-protocol))
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-protocol" version))
              (sha256 (base32 "0wklahcafcib6jwrr3angj7zzcb9csq3c0xj3axldb8w9wyhqcds"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-net-smtp
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-net-smtp))
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-smtp" version))
              (sha256 (base32 "0dh7nzjp0fiaqq1jz90nv4nxhc2w359d7c199gmzq965cfps15pd"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-net-ssh
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-net-ssh))
    (version "7.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-ssh" version))
              (sha256 (base32 "1w1ypxa3n6mskkwb00b489314km19l61p5h3bar6zr8cng27c80p"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-nio4r
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-nio4r))
    (version "2.7.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "nio4r" version))
              (sha256 (base32 "18fwy5yqnvgixq3cn0h63lm8jaxsjjxkmj8rhiv8wpzv9271d43c"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-nokogiri
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-nokogiri))
    (version "1.19.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "nokogiri" version))
              (sha256 (base32 "1d9safb4dly6qmc2g06444l0zifby52yy6j1a5fa1g4j3ihm3jah"))))
    (arguments
     (list #:tests? #f
           #:gem-flags
           #~(list "--" "--use-system-libraries"
                   (string-append "--with-xml2-include="
                                  #$(this-package-input "libxml2")
                                  "/include/libxml2"))))
    (inputs (list zlib libxml2 libxslt))
    (native-inputs '())))

(define-public ruby-nori
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-nori))
    (version "2.7.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "nori" version))
              (sha256 (base32 "0qb84bbi74q0zgs09sdkq750jf2ri3lblbry0xi4g1ard4rwsrk1"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-octokit
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-octokit))
    (version "10.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "octokit" version))
              (sha256 (base32 "1s14kbjfm9vdvcrwqdarfdbfsjqs1jxpglp60plvfdvnkd9rmsc2"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-ostruct
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-ostruct))
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ostruct" version))
              (sha256 (base32 "05xqijcf80sza5pnlp1c8whdaay8x5dc13214ngh790zrizgp8q9"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-parallel
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-parallel))
    (version "1.27.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "parallel" version))
              (sha256 (base32 "0c719bfgcszqvk9z47w2p8j2wkz5y35k48ywwas5yxbbh3hm3haa"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-parser
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-parser))
    (version "3.3.9.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "parser" version))
              (sha256 (base32 "1wl7frfk68q6gsf6q6j32jl5m3yc0b9x8ycxz3hy79miaj9r5mll"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-pdf-reader
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-pdf-reader))
    (version "2.14.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pdf-reader" version))
              (sha256 (base32 "06s17fbb9br58ng02kzwijch0kbdsjzr3rnrganr98s9q8hlanml"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-pg
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-pg))
    (version "1.5.9")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pg" version))
              (sha256 (base32 "1p2gqqrm895fzr9vi8d118zhql67bm8ydjvgqbq1crdnfggzn7kn"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-prism
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-prism))
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "prism" version))
              (sha256 (base32 "0d3i31npmlhmigcs1zlb9lksg7z7lv6nffams71wrz5rriv1n35l"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-pry
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-pry))
    (version "0.15.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pry" version))
              (sha256 (base32 "0ssv704qg75mwlyagdfr9xxbzn1ziyqgzm0x474jkynk8234pm8j"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-psych
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-psych))
    (version "5.3.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "psych" version))
              (sha256 (base32 "0x0r3gc66abv8i4dw0x0370b5hrshjfp6kpp7wbp178cy775fypb"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-public-suffix
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-public-suffix))
    (version "7.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "public_suffix" version))
              (sha256 (base32 "08znfv30pxmdkjyihvbjqbvv874dj3nybmmyscl958dy3f7v12qs"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-puma
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-puma))
    (version "8.0.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "puma" version))
              (sha256 (base32 "1yw6nvkvddriacmva8hm0za0961d6j96dm7zm6748rmyzcfqgvf8"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rack
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-rack))
    (version "3.1.21")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rack" version))
              (sha256 (base32 "0xwpvgr9g1y2b367b9gy292c86xph2l5jy0lvk8xnc7giqi5yp18"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rack-protection
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-rack-protection))
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rack-protection" version))
              (sha256 (base32 "1b4bamcbpk29i7jvly3i7ayfj69yc1g03gm4s7jgamccvx12hvng"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rack-test
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-rack-test))
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rack-test" version))
              (sha256 (base32 "0qy4ylhcfdn65a5mz2hly7g9vl0g13p5a0rmm6sc0sih5ilkcnh0"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rails-dom-testing
  (package
    (inherit (@ (gnu packages rails) ruby-rails-dom-testing))
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rails-dom-testing" version))
              (sha256 (base32 "07awj8bp7jib54d0khqw391ryw8nphvqgw4bb12cl4drlx9pkk4a"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rails-html-sanitizer
  (package
    (inherit (@ (gnu packages rails) ruby-rails-html-sanitizer))
    (version "1.6.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rails-html-sanitizer" version))
              (sha256 (base32 "0q55i6mpad20m2x1lg5pkqfpbmmapk0sjsrvr1sqgnj2hb5f5z1m"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-railties
  (package
    (inherit (@ (gnu packages rails) ruby-railties))
    (version "8.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "railties" version))
              (sha256 (base32 "1md96yl05v436jkgz9725cax9hf61sv74267cg7yidwnl3lwd65d"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rdoc
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-rdoc))
    (version "6.15.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rdoc" version))
              (sha256 (base32 "01cbwyx9rapyjdx847bhcb9iclmabdcajqs0rlm7rrv99636h3hg"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-regexp-parser
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-regexp-parser))
    (version "2.11.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "regexp_parser" version))
              (sha256 (base32 "192mzi0wgwl024pwpbfa6c2a2xlvbh3mjd75a0sakdvkl60z64ya"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-reline
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-reline))
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "reline" version))
              (sha256 (base32 "0ii8l0q5zkang3lxqlsamzfz5ja7jc8ln905isfdawl802k2db8x"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rexml
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-rexml))
    (version "3.4.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rexml" version))
              (sha256 (base32 "1jmbf6lf7pcyacpb939xjjpn1f84c3nw83dy3p1lwjx0l2ljfif7"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rspec-core
  (package
    (inherit (@ (gnu packages ruby-check) ruby-rspec-core))
    (version "3.13.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-core" version))
              (sha256 (base32 "18sgga9zjrd5579m9rpb78l7yn9a0bjzwz51z5kiq4y6jwl6hgxb"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rspec-expectations
  (package
    (inherit (@ (gnu packages ruby-check) ruby-rspec-expectations))
    (version "3.13.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-expectations" version))
              (sha256 (base32 "0dl8npj0jfpy31bxi6syc7jymyd861q277sfr6jawq2hv6hx791k"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rspec-mocks
  (package
    (inherit (@ (gnu packages ruby-check) ruby-rspec-mocks))
    (version "3.13.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-mocks" version))
              (sha256 (base32 "10gajm8iscl7gb8q926hyna83bw3fx2zb4sqdzjrznjs51pqlcz4"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rspec-rails
  (package
    (inherit (@ (gnu packages rails) ruby-rspec-rails))
    (version "8.0.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-rails" version))
              (sha256 (base32 "1kis8dfxlvi6gdzrv9nsn3ckw0c2z7armhni917qs1jx7yjkjc8i"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rspec-support
  (package
    (inherit (@ (gnu packages ruby-check) ruby-rspec-support))
    (version "3.13.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-support" version))
              (sha256 (base32 "1cmgz34hwj5s3jwxhyl8mszs24nci12ffbrmr5jb1si74iqf739f"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rubocop
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-rubocop))
    (version "1.75.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rubocop" version))
              (sha256 (base32 "1scbammv27cxmbqbs03akgig6jf0mswapy470q12cgr64nxnwmi3"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rubocop-ast
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-rubocop-ast))
    (version "1.47.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rubocop-ast" version))
              (sha256 (base32 "1bh1kls2cs2j3cmj6f2j2zmfqfknj2a6i441d828nh2mg00q49jr"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-rubyzip
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-rubyzip))
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rubyzip" version))
              (sha256 (base32 "15dnb2admqpyw6ipxbaqj4cnfwldwkm11gwrzlf2sa329pa99296"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-simplecov
  (package
    (inherit (@ (gnu packages ruby-check) ruby-simplecov))
    (version "0.18.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "simplecov" version))
              (sha256 (base32 "1n55gkbirl7zvjk4bli5zm6rgpljlhrhn0ay42rvacz170yihin2"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-simplecov-html
  (package
    (inherit (@ (gnu packages ruby-check) ruby-simplecov-html))
    (version "0.13.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "simplecov-html" version))
              (sha256 (base32 "02zi3rwihp7rlnp9x18c9idnkx7x68w6jmxdhyc0xrhjwrz0pasx"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-sinatra
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-sinatra))
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sinatra" version))
              (sha256 (base32 "103h6wjpcqp3i034hi44za2v365yz7qk9s5df8lmasq43nqvkbmp"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-sqlite3
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-sqlite3))
    (version "2.9.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sqlite3" version))
              (sha256 (base32 "15swvvy6nk3zlv4k693dfcaah8lana180v4fan4v51kqq6wwaqb1"))))
    (arguments
     (list #:tests? #f
           #:gem-flags #~(list "--" "--enable-system-libraries")))
    (inputs (list sqlite))
    (native-inputs '())))

(define-public ruby-stringio
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-stringio))
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "stringio" version))
              (sha256 (base32 "07mfqb40b2wh53k33h91zva78f9zwcdnl85jiq74wnaw2wa6wiak"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-thin
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-thin))
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "thin" version))
              (sha256 (base32 "1sddrm5n8jfvyqcmfhxrif6jsp1vla4wv9sx9f3c7xbphdjfbgav"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-tilt
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-tilt))
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "tilt" version))
              (sha256 (base32 "0szpapi229v3scrvw1pgy0vpjm7z3qlf58m1198kxn70cs278g96"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-timecop
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-timecop))
    (version "0.9.10")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "timecop" version))
              (sha256 (base32 "1syq1hdxq13ggy5c4sfk2378kzc1cgxdyv6b8c86pkydaz74bfhj"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-ttfunk
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-ttfunk))
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ttfunk" version))
              (sha256 (base32 "1ji0kn8jkf1rpskv3ijzxvqwixg4p6sk8kg0vmwyjinci7jcgjx7"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-tzinfo
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-tzinfo))
    (version "2.0.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "tzinfo" version))
              (sha256 (base32 "16w2g84dzaf3z13gxyzlzbf748kylk5bdgg3n1ipvkvvqy685bwd"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-tzinfo-data
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-tzinfo-data))
    (version "1.2025.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "tzinfo-data" version))
              (sha256 (base32 "02yz3x0qxnnwbf7k18yck5pggbnyy43rq0d5w2r6rwlk3981m31d"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-unicode-emoji
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-unicode-emoji))
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "unicode-emoji" version))
              (sha256 (base32 "1995yfjbvjlwrslq48gzzc9j0blkdzlbda9h90pjbm0yvzax55s9"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-useragent
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-useragent))
    (version "0.16.11")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "useragent" version))
              (sha256 (base32 "0i1q2xdjam4d7gwwc35lfnz0wyyzvnca0zslcfxm9fabml9n83kh"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-warden
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-warden))
    (version "1.2.9")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "warden" version))
              (sha256 (base32 "1l7gl7vms023w4clg02pm4ky9j12la2vzsixi2xrv9imbn44ys26"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-webmock
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-webmock))
    (version "3.26.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "webmock" version))
              (sha256 (base32 "142cbab47mjxmg8gc89d94sd3h7an9ligh38r9n88wb3xbr5cibp"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-webrick
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-webrick))
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "webrick" version))
              (sha256 (base32 "12d9n8hll67j737ym2zw4v23cn4vxyfkb6vyv1rzpwv6y6a3qbdl"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-websocket-driver
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-websocket-driver))
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "websocket-driver" version))
              (sha256 (base32 "0ij19k6034x0c4hw0ywa7wnk5s912r8aq0hhjss10d5z36q5dicp"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-websocket-extensions
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-websocket-extensions))
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "websocket-extensions" version))
              (sha256 (base32 "0hc2g9qps8lmhibl5baa91b4qx8wqw872rgwagml78ydj8qacsqw"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-xml-simple
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-xml-simple))
    (version "1.1.9")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "xml-simple" version))
              (sha256 (base32 "0pb9plyl71mdbjr4kllfy53qx6g68ryxblmnq9dilvy837jk24fj"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-yard
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-yard))
    (version "0.9.45")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "yard" version))
              (sha256 (base32 "0lfwngmsz3xq9k9la9890cqpm4vk3rda4171yzms7f3w7x4i3qjj"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))

(define-public ruby-zeitwerk
  (package
    (inherit (@ (gnu packages ruby-xyz) ruby-zeitwerk))
    (version "2.7.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "zeitwerk" version))
              (sha256 (base32 "119ypabas886gd0n9kiid3q41w76gz60s8qmiak6pljpkd56ps5j"))))
    (arguments (list #:tests? #f))
    (native-inputs '())))


;; Metasploit framework gems and dependencies

;; Additional runtime gems required by Gemfile.lock (missed by importer)

(define-public ruby-aarch64
  (package
    (name "ruby-aarch64")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "aarch64" version))
              (sha256 (base32 "1c0hkbm94prkw0nb76l0il7fhz1xz5dkgzh6wwyrs88lwggv9avh"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Tired of writing Ruby in Ruby? Now you can write ARM64 assembly in Rub")
    (description "Tired of writing Ruby in Ruby? Now you can write ARM64 assembly in Ruby!.")
    (home-page "https://github.com/tenderlove/aarch64")
    (license license:asl2.0)))

(define-public ruby-syslog
  (package
    (name "ruby-syslog")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "syslog" version))
              (sha256 (base32 "023lbh48fcn72gwyh1x52ycs1wx1bnhdajmv0qvkidmdsmxnxzjd"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Ruby interface for the POSIX system logging facility")
    (description "Ruby interface for the POSIX system logging facility.")
    (home-page "https://github.com/ruby/syslog")
    (license (list license:ruby license:bsd-2))))

(define-public ruby-win32api
  (package
    (name "ruby-win32api")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "win32api" version))
              (sha256 (base32 "1liryy0pz0gpw5sadbb9xwaflay272b8bwv4pclh6wkhldnh7wg7"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Wrapper library for win32 API")
    (description "The wrapper library for win32 API.")
    (home-page "https://github.com/ruby/win32api")
    (license license:ruby)))

(define-public ruby-uri
  (package
    (name "ruby-uri")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "uri" version))
              (sha256 (base32 "1ijpbj7mdrq7rhpq2kb51yykhrs2s54wfs6sm9z3icgz4y6sb7rp"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "URI is a module providing classes to handle Uniform Resource Identifie")
    (description "URI is a module providing classes to handle Uniform Resource Identifiers.")
    (home-page "https://github.com/ruby/uri")
    (license (list license:ruby license:bsd-2))))

(define-public ruby-pp
  (package
    (name "ruby-pp")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pp" version))
              (sha256 (base32 "1xlxmg86k5kifci1xvlmgw56x88dmqf04zfzn7zcr4qb8ladal99"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Provides a PrettyPrinter for Ruby objects")
    (description "Provides a PrettyPrinter for Ruby objects.")
    (home-page "https://github.com/ruby/pp")
    (license (list license:ruby license:bsd-2))))

(define-public ruby-net-http
  (package
    (name "ruby-net-http")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-http" version))
              (sha256 (base32 "15k96fj6qwbaiv6g52l538ass95ds1qwgynqdridz29yqrkhpfi5"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "HTTP client api for Ruby")
    (description "HTTP client api for Ruby.")
    (home-page "https://github.com/ruby/net-http")
    (license (list license:ruby license:bsd-2))))

(define-public ruby-winrm
  (package
    (name "ruby-winrm")
    (version "2.3.9")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "winrm" version))
              (sha256 (base32 "01jxpshw5kx5ha21ymaaj14vibv5bvm0dd80ccc6xl3jaxy7cszg"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Ruby library for Windows Remote Management")
    (description "Ruby library for Windows Remote Management.")
    (home-page "https://github.com/WinRb/WinRM")
    (license license:asl2.0)))

(define-public ruby-gssapi
  (package
    (name "ruby-gssapi")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "gssapi" version))
              (sha256 (base32 "1qdfhj12aq8v0y961v4xv96a1y2z80h3xhvzrs9vsfgf884g6765"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "FFI wrapper around the system GSSAPI library. Please make sure and read t")
    (description "A FFI wrapper around the system GSSAPI library.  Please make sure and read the     Yard docs or standard GSSAPI documentation if you have any questions.      There is also a class called GSSAPI::Simple.")
    (home-page "http://github.com/zenchild/gssapi")
    (license license:expat)))

(define-public ruby-gyoku
  (package
    (name "ruby-gyoku")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "gyoku" version))
              (sha256 (base32 "1kd2q59xpm39hpvmmvyi6g3f1fr05xjbnxwkrdqz4xy7hirqi79q"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Gyoku translates Ruby Hashes to XML")
    (description "Gyoku translates Ruby Hashes to XML.")
    (home-page "https://github.com/savonrb/gyoku")
    (license license:expat)))

(define-public ruby-json-schemer
  (package
    (name "ruby-json-schemer")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "json_schemer" version))
              (sha256 (base32 "15p31bq932bfpsi1wgrkgwm71l7z1h1w53q6vl44w6kjrr6gn09g"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "JSON Schema validator. Supports drafts 4, 6, 7, 2019-09, 2020-12, OpenAPI 3")
    (description "JSON Schema validator.  Supports drafts 4, 6, 7, 2019-09, 2020-12, OpenAPI 3.0, and OpenAPI 3.1.")
    (home-page "https://github.com/davishmcclurg/json_schemer")
    (license license:expat)))

(define-public ruby-hana
  (package
    (name "ruby-hana")
    (version "1.3.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hana" version))
              (sha256 (base32 "03cvrv2wl25j9n4n509hjvqnmwa60k92j741b64a1zjisr1dn9al"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Implementation of [JSON Patch][1] and [JSON Pointer][2] RFC")
    (description "Implementation of [JSON Patch][1] and [JSON Pointer][2] RFC.")
    (home-page "http://github.com/tenderlove/hana")
    (license license:expat)))

(define-public ruby-logging
  (package
    (name "ruby-logging")
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "logging" version))
              (sha256 (base32 "1jqcq2yxh973f3aw63nd3wxhqyhkncz3pf8v2gs3df0iqair725s"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Flexible logging library for Ruby")
    (description "Logging is a flexible logging library for Ruby programs, based on
the design of Java's log4j library.  It features a hierarchical logging system,
custom level names, and multiple output destinations.")
    (home-page "http://rubygems.org/gems/logging")
    (license license:expat)))

(define-public ruby-little-plugger
  (package
    (name "ruby-little-plugger")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "little-plugger" version))
              (sha256 (base32 "1frilv82dyxnlg8k1jhrvyd73l6k17mxc5vwxx080r4x1p04gwym"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "LittlePlugger is a module that provides Gem based plugin management. By ext")
    (description "LittlePlugger is a module that provides Gem based plugin management.  By extending your own class or module with LittlePlugger you can easily manage the loading and initializing of plugins provided by.")
    (home-page "http://gemcutter.org/gems/little-plugger")
    (license license:expat)))

(define-public ruby-lru-redux
  (package
    (name "ruby-lru-redux")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "lru_redux" version))
              (sha256 (base32 "1yxghzg7476sivz8yyr9nkak2dlbls0b89vc2kg52k0nmg6d0wgf"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Efficient implementation of an lru cache")
    (description "An efficient implementation of an lru cache.")
    (home-page "https://github.com/SamSaffron/lru_redux")
    (license license:expat)))

(define-public ruby-faraday-retry
  (package
    (name "ruby-faraday-retry")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "faraday-retry" version))
              (sha256 (base32 "023ncwlagnf2irx2ckyj1pg1f1x436jgr4a5y45mih298p8zwij1"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Catches exceptions and retries each request a limited number of times")
    (description "Catches exceptions and retries each request a limited number of times.")
    (home-page "https://github.com/lostisland/faraday-retry")
    (license license:expat)))

(define-public ruby-elftools
  (package
    (name "ruby-elftools")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "elftools" version))
              (sha256 (base32 "1y5ahqk24ngjiyfbj5xapxwfb34yslcpn4l0khdq4pkxkajgqilx"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Light weight ELF parser. elftools is designed to be a low-level ELF parse")
    (description "A light weight ELF parser. elftools is designed to be a low-level ELF parser.   Inspired by https://github.com/eliben/pyelftools.")
    (home-page "https://github.com/david942j/rbelftools")
    (license license:expat)))

(define-public ruby-tomlrb
  (package
    (name "ruby-tomlrb")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "tomlrb" version))
              (sha256 (base32 "168v339gqaly00i4zqg2ag2h10r3rl7999d0cqrrpb63gaa7fbr6"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Parses TOML 1.1 and 1.0 documents into Ruby Hashes. Pure Ruby, no runtime d")
    (description "Parses TOML 1.1 and 1.0 documents into Ruby Hashes.  Pure Ruby, no runtime dependencies, checked against the official toml-test suite.")
    (home-page "https://github.com/fbernier/tomlrb")
    (license license:expat)))

(define-public ruby-unix-crypt
  (package
    (name "ruby-unix-crypt")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "unix-crypt" version))
              (sha256 (base32 "11p5ij6iasr46fbzz0z3jibaiwf242bq8xwav33rljwcwh85sy5y"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Performs the UNIX crypt(3) algorithm using DES (standard 13 character passw")
    (description "Performs the UNIX crypt(3) algorithm using DES (standard 13 character passwords), MD5 (starting with $1$), SHA256 (starting with $5$) and SHA512 (starting with $6$).")
    (home-page "https://github.com/mogest/unix-crypt")
    (license license:bsd-0)))

(define-public ruby-with-env
  (package
    (name "ruby-with-env")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "with_env" version))
              (sha256 (base32 "1r5ns064mbb99hf1dyxsk9183hznc5i7mn3bi86zka6dlvqf9csh"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "WithEnv is an extremely small helper module for executing code with ENV var")
    (description "WithEnv is an extremely small helper module for executing code with ENV variables.  It exists because     I got tired of re-writing or copying over a #with_env helper method for the 131st time.")
    (home-page "https://github.com/mhs/with_env-rb")
    (license license:expat)))

(define-public ruby-require-all
  (package
    (name "ruby-require-all")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "require_all" version))
              (sha256 (base32 "1l7ym6vagvb9wifaq2w0njkbpil7pzvpn42inpm8hcw3lbx56y4k"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Wonderfully simple way to load your code")
    (description "A wonderfully simple way to load your code.")
    (home-page "http://github.com/jarmo/require_all")
    (license license:expat)))

(define-public ruby-ruby-mysql
  (package
    (name "ruby-ruby-mysql")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ruby-mysql" version))
              (sha256 (base32 "0f84lgsn4m9h692azm6zi8h12r078nbzvnmj047bs838aw45shrp"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "This is MySQL connector. pure Ruby version")
    (description "This is MySQL connector. pure Ruby version.")
    (home-page "http://gitlab.com/tmtms/ruby-mysql")
    (license license:expat)))

(define-public ruby-aws-sdk-ssm
  (package
    (name "ruby-aws-sdk-ssm")
    (version "1.191.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "aws-sdk-ssm" version))
              (sha256 (base32 "1bmxvn9lna8jcr2lavliwsibkfh91hf29cp0h39glwn8sjipcav0"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Official AWS Ruby gem for Amazon Simple Systems Manager (SSM) (Amazon SSM)")
    (description "Official AWS Ruby gem for Amazon Simple Systems Manager (SSM) (Amazon SSM).  This gem is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-sdk-ec2instanceconnect
  (package
    (name "ruby-aws-sdk-ec2instanceconnect")
    (version "1.55.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "aws-sdk-ec2instanceconnect" version))
              (sha256 (base32 "0zv6vc5s6hads412cy639k3gx88y519vhzizsgng0vnj1azsxls0"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Official AWS Ruby gem for AWS EC2 Instance Connect (EC2 Instance Connect)")
    (description "Official AWS Ruby gem for AWS EC2 Instance Connect (EC2 Instance Connect).  This gem is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-swagger-blocks
  (package
    (name "ruby-swagger-blocks")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "swagger-blocks" version))
              (sha256 (base32 "0bycg5si4pr69b0qqiqzhwcich90mvmn0v0gs39slvxg5nv3h28k"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Define and serve live-updating Swagger JSON for Ruby apps")
    (description "Define and serve live-updating Swagger JSON for Ruby apps.")
    (home-page "https://github.com/fotinakis/swagger-blocks")
    (license license:expat)))

(define-public ruby-mcp
  (package
    (name "ruby-mcp")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mcp" version))
              (sha256 (base32 "13ihas3bmq8hq3ahcczzi7jy3a5d2bbdg2vc86fy6px84l218ijn"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Official Ruby SDK for Model Context Protocol servers and clients")
    (description "The official Ruby SDK for Model Context Protocol servers and clients.")
    (home-page "https://ruby.sdk.modelcontextprotocol.io")
    (license license:asl2.0)))

(define-public ruby-hrr-rb-ssh-ed25519
  (package
    (name "ruby-hrr-rb-ssh-ed25519")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hrr_rb_ssh-ed25519" version))
              (sha256 (base32 "1mfsvjcjmm63fwjf3zqkmg3cf55vx34vmvix0wj0ba4h9dzjq7p8"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Hrr_rb_ssh extension that supports ED25519 public key algorithm")
    (description "Hrr_rb_ssh extension that supports ED25519 public key algorithm.")
    (home-page "https://github.com/hirura/hrr_rb_ssh-ed25519")
    (license license:asl2.0)))

(define-public ruby-xmlrpc
  (package
    (name "ruby-xmlrpc")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "xmlrpc" version))
       (sha256
        (base32 "0fwfnccagsjrbvrav5nbk3zracj9zncr7i375nn20jd4cfy4cggc"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-rexml ruby-webrick))
    (synopsis
     "XMLRPC is a lightweight protocol that enables remote procedure calls over HTTP.")
    (description
     "XMLRPC is a lightweight protocol that enables remote procedure calls over HTTP.")
    (home-page "https://github.com/ruby/xmlrpc")
    (license (list license:ruby license:bsd-2))))

(define-public ruby-xdr
  (package
    (name "ruby-xdr")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "xdr" version))
       (sha256
        (base32 "0jrvy9k0ih073wxwa74gm6x5r5k3kwgg0sr16rfzfwmvq6nhsbq3"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-activemodel ruby-activesupport))
    (synopsis "XDR Helper Library")
    (description "XDR Helper Library.")
    (home-page "https://github.com/astroband/ruby-xdr")
    (license license:asl2.0)))

(define-public ruby-sshkey
  (package
    (name "ruby-sshkey")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sshkey" version))
       (sha256
        (base32 "1k8i5pzjhcnyf0bhcyn5iixpfp4pz0556rcxwpglh6p0sr8s6nv5"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Generate private/public SSH keypairs using pure Ruby")
    (description "Generate private/public SSH keypairs using pure Ruby.")
    (home-page "https://github.com/bensie/sshkey")
    (license license:expat)))

(define-public ruby-windows-error
  (package
    (name "ruby-windows-error")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "windows_error" version))
       (sha256
        (base32 "06wi6jfsh4g60hvadrxnrzjnqn5zmddzdxbhrvsy8yf7r3fhmjvg"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "The WindowsError gem provides an easily accessible reference for
                          standard Windows API Error Codes. It allows you to do comparisons
                          as well as direct lookups of error codes to translate the numerical
                          value returned by the API, into a meaningful and human readable message.")
    (description
     "The @code{WindowsError} gem provides an easily accessible reference for standard
Windows API Error Codes.  It allows you to do comparisons as well as direct
lookups of error codes to translate the numerical value returned by the API,
into a meaningful and human readable message.")
    (home-page "https://github.com/rapid7/windows_error")
    (license license:bsd-3)))

(define-public ruby-strptime
  (package
    (name "ruby-strptime")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "strptime" version))
       (sha256
        (base32 "1ycs0xz58kymf7yp4h56f0nid2z7g3s18dj7pa3p790pfzzpgvcq"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "A fast strptime/strftime engine which uses VM")
    (description "A fast strptime/strftime engine which uses VM.")
    (home-page "https://github.com/nurse/strptime")
    (license license:bsd-2)))

(define-public ruby-rasn1
  (package
    (name "ruby-rasn1")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rasn1" version))
       (sha256
        (base32 "1rz96j4jzm02rdxrs70hj5an3kmxrfd77d4hy97b80fasj049992"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-strptime))
    (synopsis
     "RASN1 is a pure ruby ASN.1 library. It may encode and decode DER and BER
encodings.
")
    (description
     "RASN1 is a pure ruby ASN.1 library.  It may encode and decode DER and BER
encodings.")
    (home-page "https://codeberg.org/lemontree55/rasn1")
    (license license:expat)))

(define-public ruby-openssl-cmac
  (package
    (name "ruby-openssl-cmac")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "openssl-cmac" version))
       (sha256
        (base32 "1mml6105j6ryd9d019gbwzkdjmvycjlxxld0qzg9vs70f1qdihcc"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Ruby Gem for RFC 4493, 4494, 4615 - The AES-CMAC Algorithm")
    (description "Ruby Gem for RFC 4493, 4494, 4615 - The AES-CMAC Algorithm.")
    (home-page "https://github.com/smalllars/openssl-cmac")
    (license license:expat)))

(define-public ruby-bindata
  (package
    (name "ruby-bindata")
    (version "2.4.15")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bindata" version))
       (sha256
        (base32 "04y4zgh4bbcb8wmkxwfqg4saky1d1f3xw8z6yk543q13h8ky8rz5"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "BinData is a declarative way to read and write binary file formats.

This means the programmer specifies *what* the format of the binary
data is, and BinData works out *how* to read and write data in this
format.  It is an easier ( and more readable ) alternative to
ruby's #pack and #unpack methods.
")
    (description
     "@code{BinData} is a declarative way to read and write binary file formats.  This
means the programmer specifies *what* the format of the binary data is, and
@code{BinData} works out *how* to read and write data in this format.  It is an
easier ( and more readable ) alternative to ruby's #pack and #unpack methods.")
    (home-page "https://github.com/dmendel/bindata")
    (license license:bsd-2)))

(define-public ruby-ruby-smb
  (package
    (name "ruby-ruby-smb")
    (version "3.3.25")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby_smb" version))
       (sha256
        (base32 "1hq6pvwih3s3ng42blvl2ghhr4zr3k2ps774h0af98k7r5kfnc4h"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-bindata
                             ruby-openssl-ccm
                             ruby-openssl-cmac
                             ruby-rasn1
                             ruby-rubyntlm
                             ruby-windows-error))
    (synopsis "Pure Ruby implementation of the SMB Protocol Family")
    (description
     "This package provides a pure Ruby implementation of the SMB Protocol Family.")
    (home-page "https://github.com/rapid7/ruby_smb")
    (license license:bsd-3)))

(define-public ruby-macho
  (package
    (name "ruby-macho")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby-macho" version))
       (sha256
        (base32 "0859xi0shidwb9cfbczxsk8qrbi3qkmkp9xzyca1xzp0gmzv7ni3"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Library for viewing and manipulating Mach-O files in Ruby")
    (description
     "This package provides a library for viewing and manipulating Mach-O files in
Ruby.")
    (home-page "https://github.com/Homebrew/ruby-macho")
    (license license:expat)))

(define-public ruby-rex-zip
  (package
    (name "ruby-rex-zip")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-zip" version))
       (sha256
        (base32 "0iyvf7hi2327hl4a3vssb6s7nxglcwmlm39rklrraz0xqwpmrbla"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-rex-text))
    (synopsis
     "Ruby Exploitation(Rex) library for working with zip and related files")
    (description
     "Ruby Exploitation(Rex) library for working with zip and related files.")
    (home-page "https://github.com/rapid7/rex-zip")
    (license license:bsd-3)))

(define-public ruby-rex-sslscan
  (package
    (name "ruby-rex-sslscan")
    (version "0.1.13")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-sslscan" version))
       (sha256
        (base32 "183rnr3y9pfq9q8v7a72nm97nqhcks6skx4z7y569nn506f1k7v7"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-rex-core ruby-rex-socket ruby-rex-text))
    (synopsis
     "This library is a pure ruby implmentation of the SSLScan tool originally written
                          by Ian Ventura-Whiting. It currently depends on the system version of OpenSSL")
    (description
     "This library is a pure ruby implmentation of the SSLScan tool originally written
by Ian Ventura-Whiting.  It currently depends on the system version of
@code{OpenSSL}.")
    (home-page "https://github.com/rapid7/rex-sslscan")
    (license license:bsd-3)))

(define-public ruby-rex-rop-builder
  (package
    (name "ruby-rex-rop-builder")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-rop_builder" version))
       (sha256
        (base32 "0i5i5sbij7zwjzjkwnf1rlklpi03r0ibzc3c9n9kv1x5wcn04p04"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-metasm ruby-rex-core ruby-rex-text))
    (synopsis "Ruby Exploitation(Rex) Library for building ROP chains")
    (description "Ruby Exploitation(Rex) Library for building ROP chains.")
    (home-page "https://github.com/rapid7/rex-rop_builder")
    (license license:bsd-3)))

(define-public ruby-rex-registry
  (package
    (name "ruby-rex-registry")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-registry" version))
       (sha256
        (base32 "07kvpxkg37vi8z7g3sj12rx605vg4v8gsp05ns124a1980mmysw7"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Ruby Exploitation(Rex) library for parsing offline registry files from a Windows machine")
    (description
     "Ruby Exploitation(Rex) library for parsing offline registry files from a Windows
machine.")
    (home-page "https://github.com/rapid7/rex-registry")
    (license license:bsd-3)))

(define-public ruby-rex-random-identifier
  (package
    (name "ruby-rex-random-identifier")
    (version "0.1.21")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-random_identifier" version))
       (sha256
        (base32 "1awnn8rc7g9kjxwfx4xkplhvi2vxxpjf2m9d2yj9s8firh9wlyws"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-bigdecimal ruby-rex-text))
    (synopsis
     "Ruby Exploitation(Rex) library for generating Random identifier strings")
    (description
     "Ruby Exploitation(Rex) library for generating Random identifier strings.")
    (home-page "https://github.com/rapid7/rex-random_identifier")
    (license license:bsd-3)))

(define-public ruby-rex-powershell
  (package
    (name "ruby-rex-powershell")
    (version "0.1.105")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-powershell" version))
       (sha256
        (base32 "0wjqvi4h0icmk80w0s38i6i2r76phc2v73f6709j173w25p47dyj"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-bigdecimal ruby-rex-random-identifier
                             ruby-rex-text ruby-rc4))
    (synopsis
     "Ruby Exploitation(Rex) library for generating/manipulating Powershell scripts")
    (description
     "Ruby Exploitation(Rex) library for generating/manipulating Powershell scripts.")
    (home-page "https://github.com/rapid7/rex-powershell")
    (license license:bsd-3)))

(define-public ruby-rex-ole
  (package
    (name "ruby-rex-ole")
    (version "0.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-ole" version))
       (sha256
        (base32 "1gr6yli7hb4v4sgzm7997xwccv0ij5yyjfxa0imsv5xwkkqvqxv8"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-rex-text))
    (synopsis
     "Ruby Exploitation(Rex) library gem for reading/writing Object-Linking-and-Embedding (OLE) files and streams")
    (description "Ruby Exploitation(Rex) library gem for reading/writing
Object-Linking-and-Embedding (OLE) files and streams.")
    (home-page "https://github.com/rapid7/rex-ole")
    (license license:bsd-3)))

(define-public ruby-rex-nop
  (package
    (name "ruby-rex-nop")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-nop" version))
       (sha256
        (base32 "0blajyinziiab8x4a2ghylrv3f111ya4j2nx0qvjcraxyzzgaasj"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-rex-arch))
    (synopsis
     "This library contains the opty2 library for dynamic generation of x86 multi-byte NOPs.
                          This is useful in writing exploits and encoders. It allows you to dynamic generate variable
                          length instruction sets that are equivalent to a No Operation(NOP) without using
                          the actual 0x90 bytecode. The original code was written by Optyx and spoonm. ")
    (description
     "This library contains the opty2 library for dynamic generation of x86 multi-byte
NOPs.  This is useful in writing exploits and encoders.  It allows you to
dynamic generate variable length instruction sets that are equivalent to a No
Operation(NOP) without using the actual 0x90 bytecode.  The original code was
written by Optyx and spoonm.")
    (home-page "https://github.com/rapid7/rex-nop")
    (license license:bsd-3)))

(define-public ruby-rex-mime
  (package
    (name "ruby-rex-mime")
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-mime" version))
       (sha256
        (base32 "0fwpak9dsxd5qg4wvxacnl0jcg6hha4j6ryifsz68kqp3gfp4z7s"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-bigdecimal ruby-rex-text))
    (synopsis "This library is for creating and/or parsing MIME messages")
    (description "This library is for creating and/or parsing MIME messages.")
    (home-page "https://github.com/rapid7/rex-mime")
    (license license:bsd-3)))

(define-public ruby-rex-java
  (package
    (name "ruby-rex-java")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-java" version))
       (sha256
        (base32 "0l5q63yhr25di2g79mjgnkwwhwxwqkbp6wbjybn5qqcbspxp66pm"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Ruby Exploitation(Rex) library for parsing Java serialized streams.")
    (description
     "Ruby Exploitation(Rex) library for parsing Java serialized streams.")
    (home-page "https://github.com/rapid7/rex-java")
    (license license:bsd-3)))

(define-public ruby-rex-exploitation
  (package
    (name "ruby-rex-exploitation")
    (version "0.1.44")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-exploitation" version))
       (sha256
        (base32 "1lpafc8qivps37wc35pklpx5pzcp0prs9201sb5rv7l7x7a4fybr"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-bigdecimal
                             ruby-jsobfu
                             ruby-metasm
                             ruby-racc
                             ruby-rex-arch
                             ruby-rex-encoder
                             ruby-rexml
                             ruby-rex-text))
    (synopsis
     "This gem contains various helper mechanisms for creating exploits.
                          This includes SEH Overwrite helpers, egghunters, command stagers and more.")
    (description
     "This gem contains various helper mechanisms for creating exploits.  This
includes SEH Overwrite helpers, egghunters, command stagers and more.")
    (home-page "https://github.com/rapid7/rex-exploitation")
    (license license:bsd-3)))

(define-public ruby-rex-encoder
  (package
    (name "ruby-rex-encoder")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-encoder" version))
       (sha256
        (base32 "0sqxbmm2yj009ihiy3a1iyy2f1576idix7v3r2sy66575q0f45la"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-metasm ruby-rex-arch ruby-rex-text))
    (synopsis
     "This library provides the basis for all of the polymorphic encoders
                          that Metasploit uses for payload encoding. Encoders are used to try and create
                          a version of a payload that is free of bad characters as defined by the exploit. ")
    (description
     "This library provides the basis for all of the polymorphic encoders that
Metasploit uses for payload encoding.  Encoders are used to try and create a
version of a payload that is free of bad characters as defined by the exploit.")
    (home-page "https://github.com/rapid7/rex-encoder")
    (license license:bsd-3)))

(define-public ruby-rex-struct2
  (package
    (name "ruby-rex-struct2")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-struct2" version))
       (sha256
        (base32 "1k8pv062kg7pc3d7yz4vywkwvs321rsbq63piiyzcirkillwn29d"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Ruby Exploitation(Rex) library for generating/manipulating C-Style structs")
    (description
     "Ruby Exploitation(Rex) library for generating/manipulating C-Style structs.")
    (home-page "https://github.com/rapid7/rex-struct2")
    (license license:bsd-3)))

(define-public ruby-rex-bin-tools
  (package
    (name "ruby-rex-bin-tools")
    (version "0.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-bin_tools" version))
       (sha256
        (base32 "0gpdvf8qqh0mm1c90vip1mpjgqrlgvaydyg5wbi3bl8dm3m1qghi"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-metasm ruby-rex-arch ruby-rex-core
                             ruby-rex-struct2 ruby-rex-text))
    (synopsis
     "A suite of tools for analyzing Elf,Mach, and PE format executables to find specific chunks of code.")
    (description
     "This package provides a suite of tools for analyzing Elf,Mach, and PE format
executables to find specific chunks of code.")
    (home-page "https://github.com/rapid7/rex-bin_tools")
    (license license:bsd-3)))

(define-public ruby-rex-text
  (package
    (name "ruby-rex-text")
    (version "0.2.63")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-text" version))
       (sha256
        (base32 "06mj93g8akibnr7z7xmscxzg3ik6jaw0bpkarr7ljmznalia7j6n"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-bigdecimal))
    (synopsis
     "This Gem contains all of the Ruby Exploitation(Rex) methods for text manipulation and generation")
    (description
     "This Gem contains all of the Ruby Exploitation(Rex) methods for text
manipulation and generation.")
    (home-page "https://github.com/rapid7/rex-text")
    (license license:bsd-3)))

(define-public ruby-rex-arch
  (package
    (name "ruby-rex-arch")
    (version "0.1.20")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-arch" version))
       (sha256
        (base32 "0b9svs55zi4bi53wj792qc43d1gjn1p96sb36ikr7f4pfz13yry7"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-rex-text))
    (synopsis
     "This library contains architecture specific information such as registers, opcodes, and stack manipulation routines.")
    (description
     "This library contains architecture specific information such as registers,
opcodes, and stack manipulation routines.")
    (home-page "https://github.com/rapid7/rex-arch")
    (license license:bsd-3)))

(define-public ruby-rb-readline
  (package
    (name "ruby-rb-readline")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rb-readline" version))
       (sha256
        (base32 "14w79a121czmvk1s953qfzww30mqjb2zc0k9qhi0ivxxk3hxg6wy"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "The readline library provides a pure Ruby implementation of the GNU readline C library, as well as the Readline extension that ships as part of the standard library.")
    (description
     "The readline library provides a pure Ruby implementation of the GNU readline C
library, as well as the Readline extension that ships as part of the standard
library.")
    (home-page "http://github.com/ConnorAtherton/rb-readline")
    (license license:bsd-3)))

(define-public ruby-patch-finder
  (package
    (name "ruby-patch-finder")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "patch_finder" version))
       (sha256
        (base32 "1md9scls55n1riw26vw1ak0ajq38dfygr36l0h00wqhv51cq745m"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Generic Patch Finder")
    (description "Generic Patch Finder.")
    (home-page "http://github.com/wchen-r7/patch-finder")
    (license license:bsd-3)))

(define-public ruby-pcaprub
  (package
    (name "ruby-pcaprub")
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pcaprub" version))
       (sha256
        (base32 "0ag6p7n5s1s3mcsqw8rzafz5dlanwwqfkh7sb44qlp0dq8bvh73v"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (inputs (list libpcap))
    (synopsis "Libpcap bindings for Ruby 2.x")
    (description "Libpcap bindings for Ruby 2.x.")
    (home-page "https://github.com/pcaprub/pcaprub")
    (license license:lgpl2.1)))

(define-public ruby-packetfu
  (package
    (name "ruby-packetfu")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "packetfu" version))
       (sha256
        (base32 "0crsc4mcikpc01d19ppryzwgvhk9hg9r73g5bwac32x979zwyks8"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-pcaprub))
    (synopsis
     "
    PacketFu is a mid-level packet manipulation library for Ruby. With
    it, users can read, parse, and write network packets with the level of
    ease and fun they expect from Ruby.
  ")
    (description
     "@code{PacketFu} is a mid-level packet manipulation library for Ruby.  With it,
users can read, parse, and write network packets with the level of ease and fun
they expect from Ruby.")
    (home-page "https://github.com/packetfu/packetfu")
    (license license:bsd-3)))

(define-public ruby-openvas-omp
  (package
    (name "ruby-openvas-omp")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "openvas-omp" version))
       (sha256
        (base32 "14xf614vd76qjdjxjv14mmjar6s64fwp4cwb7bv5g1wc29srg28x"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Communicate with OpenVAS manager through OMP.
This library is used for communication with OpenVAS manager over OMP.
You can start, stop, pause and resume scan. Watch progress and status of
scan, download report, etc.")
    (description
     "Communicate with @code{OpenVAS} manager through OMP. This library is used for
communication with @code{OpenVAS} manager over OMP. You can start, stop, pause
and resume scan.  Watch progress and status of scan, download report, etc.")
    (home-page "http://github.com/kost/openvas-omp")
    (license license:expat)))

(define-public ruby-openssl-ccm
  (package
    (name "ruby-openssl-ccm")
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "openssl-ccm" version))
       (sha256
        (base32 "1mqr538wcfjc1q9qxsc2pz0s81kw1f3xk7k1qy903n5b3bh9vri3"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Ruby Gem for RFC 3610 - Counter with CBC-MAC (CCM)")
    (description "Ruby Gem for RFC 3610 - Counter with CBC-MAC (CCM).")
    (home-page "https://github.com/smalllars/openssl-ccm")
    (license license:expat)))

(define-public ruby-nexpose
  (package
    (name "ruby-nexpose")
    (version "7.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "nexpose" version))
       (sha256
        (base32 "0jz5xiwiwagd663qdlfhmc9fm76x78cqhighmfivy6w5v0n4xyq0"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "This gem provides a Ruby API to the Nexpose vulnerability management product by Rapid7.")
    (description
     "This gem provides a Ruby API to the Nexpose vulnerability management product by
Rapid7.")
    (home-page "https://github.com/rapid7/nexpose-client")
    (license license:bsd-3)))

(define-public ruby-network-interface
  (package
    (name "ruby-network-interface")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "network_interface" version))
       (sha256
        (base32 "0hqkas4c809w2gnic1srhq5rd2hpsfnhmrvm1vkix8w775qql74z"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "
     This gem was originally added to the Metasploit Pcaprub gem. It's been spun
     out into its own gem for anyone who might want to programmatically get
     information on their network interfaces. ")
    (description
     "This gem was originally added to the Metasploit Pcaprub gem.  It's been spun out
into its own gem for anyone who might want to programmatically get information
on their network interfaces.")
    (home-page "https://github.com/rapid7/network_interface")
    (license license:expat)))

(define-public ruby-net-ldap
  (package
    (name "ruby-net-ldap")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "net-ldap" version))
       (sha256
        (base32 "0wjkrvcwnxa6ggq0nfz004f1blm1c67fv7c6614sraak0wshn25j"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-base64 ruby-ostruct))
    (synopsis
     "Net::LDAP for Ruby (also called net-ldap) implements client access for the
Lightweight Directory Access Protocol (LDAP), an IETF standard protocol for
accessing distributed directory services. Net::LDAP is written completely in
Ruby with no external dependencies. It supports most LDAP client features and a
subset of server features as well.

Net::LDAP has been tested against modern popular LDAP servers including
OpenLDAP and Active Directory. The current release is mostly compliant with
earlier versions of the IETF LDAP RFCs (2251-2256, 2829-2830, 3377, and 3771).
Our roadmap for Net::LDAP 1.0 is to gain full <em>client</em> compliance with
the most recent LDAP RFCs (4510-4519, plutions of 4520-4532).")
    (description
     "Net::LDAP for Ruby (also called net-ldap) implements client access for the
Lightweight Directory Access Protocol (LDAP), an IETF standard protocol for
accessing distributed directory services.  Net::LDAP is written completely in
Ruby with no external dependencies.  It supports most LDAP client features and a
subset of server features as well.  Net::LDAP has been tested against modern
popular LDAP servers including @code{OpenLDAP} and Active Directory.  The
current release is mostly compliant with earlier versions of the IETF LDAP RFCs
(2251-2256, 2829-2830, 3377, and 3771).  Our roadmap for Net::LDAP 1.0 is to
gain full <em>client</em> compliance with the most recent LDAP RFCs (4510-4519,
plutions of 4520-4532).")
    (home-page "http://github.com/ruby-ldap/ruby-net-ldap")
    (license license:expat)))

(define-public ruby-nessus-rest
  (package
    (name "ruby-nessus-rest")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "nessus_rest" version))
       (sha256
        (base32 "1allyrd4rll333zbmsi3hcyg6cw1dhc4bg347ibsw191nswnp8ci"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Ruby library for Nessus (version 6+) JSON/REST interface. This library is used for communication with Nessus over REST interface. You can start, stop, pause and resume scan. Watch progress and status of scan, download report, etc. ")
    (description
     "Ruby library for Nessus (version 6+) JSON/REST interface.  This library is used
for communication with Nessus over REST interface.  You can start, stop, pause
and resume scan.  Watch progress and status of scan, download report, etc.")
    (home-page "https://github.com/kost/nessus_rest-ruby")
    (license license:expat)))

(define-public ruby-mqtt
  (package
    (name "ruby-mqtt")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mqtt" version))
       (sha256
        (base32 "0fyy6ybrm2bgj1nvmh6q9a9sa5sa2hcqvav8789hi13k7cjkkhaj"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-logger))
    (synopsis
     "Pure Ruby gem that implements the MQTT protocol, a lightweight protocol for publish/subscribe messaging.")
    (description
     "Pure Ruby gem that implements the MQTT protocol, a lightweight protocol for
publish/subscribe messaging.")
    (home-page "https://github.com/njh/ruby-mqtt")
    (license license:expat)))

(define-public ruby-metasploit-payloads-mettle
  (package
    (name "ruby-metasploit-payloads-mettle")
    (version "1.0.48")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "metasploit_payloads-mettle" version))
       (sha256
        (base32 "0s83sgkynzc3hn4r66mw8f8cc67z9rmg86byyn6567ar78r40h1k"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Compiled binaries for Metasploit's next-gen Meterpreter")
    (description "Compiled binaries for Metasploit's next-gen Meterpreter.")
    (home-page "http://www.metasploit.com")
    (license license:bsd-3)))

(define-public ruby-metasploit-payloads
  (package
    (name "ruby-metasploit-payloads")
    (version "2.0.257")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "metasploit-payloads" version))
       (sha256
        (base32 "17jn857wfyz5wnk6d7m75z8w1842ibxx05sdzgyskz5kazj96ia3"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Compiled binaries for Metasploit's Meterpreter")
    (description "Compiled binaries for Metasploit's Meterpreter.")
    (home-page "http://www.metasploit.com")
    (license license:bsd-3)))

(define-public ruby-rubyntlm
  (package
    (name "ruby-rubyntlm")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rubyntlm" version))
       (sha256
        (base32 "1x8l0d1v88m40mby4jvgal46137cv8gga2lk7zlrxqlsp41380a7"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-base64))
    (synopsis
     "Ruby/NTLM provides message creator and parser for the NTLM authentication.")
    (description
     "Ruby/NTLM provides message creator and parser for the NTLM authentication.")
    (home-page "https://github.com/winrb/rubyntlm")
    (license license:expat)))

(define-public ruby-rex-core
  (package
    (name "ruby-rex-core")
    (version "0.1.37")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-core" version))
       (sha256
        (base32 "1cbmrh5zirf4ilgrj9fkix3zc0fmlhzb3qjx2x1pmmxhjpf3dlhs"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Core libraries required for the Ruby Exploitation (Rex) Suite.
    Rex provides a variety of classes useful for security testing and exploit development.")
    (description
     "Core libraries required for the Ruby Exploitation (Rex) Suite.  Rex provides a
variety of classes useful for security testing and exploit development.")
    (home-page "https://github.com/rapid7/rex-core")
    (license license:bsd-3)))

(define-public ruby-rex-socket
  (package
    (name "ruby-rex-socket")
    (version "0.1.71")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rex-socket" version))
       (sha256
        (base32 "14knmm30wgpmcwnb8mxsisd7x8ni0aqx5d2agwkpc2i09w3q9fq4"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-dnsruby ruby-rex-core))
    (synopsis
     "The Ruby Exploitation (Rex) Socket Abstraction Library. This library
                          includes all of the code needed to turn sockets into Rex::Sockets with the functionality
                          for things like L3 pivoting used by Metasploit. ")
    (description
     "The Ruby Exploitation (Rex) Socket Abstraction Library.  This library includes
all of the code needed to turn sockets into Rex::Sockets with the functionality
for things like L3 pivoting used by Metasploit.")
    (home-page "https://github.com/rapid7/rex-socket")
    (license license:bsd-3)))

(define-public ruby-recog
  (package
    (name "ruby-recog")
    (version "3.1.14")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "recog" version))
       (sha256
        (base32 "1k8m1z8h367lqnjw1kj7mp8yjhgmhdd2wkkf0azpkin3kzzywzqi"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-nokogiri))
    (synopsis
     "Recog is a framework for identifying products, services, operating systems, and hardware by matching fingerprints against data returned from various network probes. Recog makes it simply to extract useful information from web server banners, snmp system description fields, and a whole lot more.")
    (description
     "Recog is a framework for identifying products, services, operating systems, and
hardware by matching fingerprints against data returned from various network
probes.  Recog makes it simply to extract useful information from web server
banners, snmp system description fields, and a whole lot more.")
    (home-page "https://www.github.com/rapid7/recog-ruby")
    (license license:bsd-2)))

(define-public ruby-metasploit-model
  (package
    (name "ruby-metasploit-model")
    (version "5.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "metasploit-model" version))
       (sha256
        (base32 "0b6d18pd56ghcvkkzxdda9g0m3ydmppv8fjjpppzplnx18ccs1sj"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-activemodel
                             ruby-activesupport
                             ruby-bigdecimal
                             ruby-drb
                             ruby-mutex-m
                             ruby-railties))
    (synopsis
     "Common code, such as validators and mixins, that are shared between ActiveModels in metasploit-framework and ActiveRecords in metasploit_data_models.")
    (description
     "Common code, such as validators and mixins, that are shared between
@code{ActiveModels} in metasploit-framework and @code{ActiveRecords} in
metasploit_data_models.")
    (home-page #f)
    (license license:bsd-3)))

(define-public ruby-arel-helpers
  (package
    (name "ruby-arel-helpers")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "arel-helpers" version))
       (sha256
        (base32 "1ijp8llfcsbn6x9r8mjkiq45c62vjyll8pva6db69yj33bw5kjcp"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-activerecord))
    (synopsis
     "Useful tools to help construct database queries with ActiveRecord and Arel.")
    (description
     "Useful tools to help construct database queries with @code{ActiveRecord} and
Arel.")
    (home-page "https://github.com/camertron/arel-helpers")
    (license license:expat)))

(define-public ruby-metasploit-data-models
  (package
    (name "ruby-metasploit-data-models")
    (version "6.0.18")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "metasploit_data_models" version))
       (sha256
        (base32 "13f5kn8q6p7wvxyi1pxdlf9v0nmfh2fc93n4fwms6g83xc30sdmh"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-activerecord
                             ruby-activesupport
                             ruby-arel-helpers
                             ruby-bigdecimal
                             ruby-drb
                             ruby-metasploit-concern
                             ruby-metasploit-model
                             ruby-mutex-m
                             ruby-pg
                             ruby-railties
                             ruby-recog
                             ruby-webrick))
    (synopsis
     "Implements minimal ActiveRecord models and database helper code used in both the Metasploit Framework (MSF) and Metasploit commercial editions.")
    (description
     "This package implements minimal @code{ActiveRecord} models and database helper
code used in both the Metasploit Framework (MSF) and Metasploit commercial
editions.")
    (home-page #f)
    (license license:bsd-3)))

(define-public ruby-csv
  (package
    (name "ruby-csv")
    (version "3.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "csv" version))
       (sha256
        (base32 "0gz7r2kazwwwyrwi95hbnhy54kwkfac5swh2gy5p5vw36fn38lbf"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "The CSV library provides a complete interface to CSV files and data. It offers tools to enable you to read and write to and from Strings or IO objects, as needed.")
    (description
     "The CSV library provides a complete interface to CSV files and data.  It offers
tools to enable you to read and write to and from Strings or IO objects, as
needed.")
    (home-page "https://github.com/ruby/csv")
    (license (list license:ruby license:bsd-2))))

(define-public ruby-metasploit-credential
  (package
    (name "ruby-metasploit-credential")
    (version "6.0.25")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "metasploit-credential" version))
       (sha256
        (base32 "1kjadfqnmnldyyhlwvv9ldk7gf5f18y819lidpr1zac900ba2pwq"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-bigdecimal
                             ruby-csv
                             ruby-drb
                             ruby-metasploit-concern
                             ruby-metasploit-data-models
                             ruby-metasploit-model
                             ruby-mutex-m
                             ruby-net-ssh
                             ruby-pg
                             ruby-railties
                             ruby-rex-socket
                             ruby-rubyntlm
                             ruby-rubyzip))
    (synopsis
     "The Metasploit::Credential namespace and its ActiveRecord::Base subclasses")
    (description
     "The Metasploit::Credential namespace and its @code{ActiveRecord::Base}
subclasses.")
    (home-page "https://github.com/rapid7/metasploit-credential")
    (license license:bsd-3)))

(define-public ruby-drb
  (package
    (name "ruby-drb")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "drb" version))
       (sha256
        (base32 "0wrkl7yiix268s2md1h6wh91311w95ikd8fy8m5gx589npyxc00b"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Distributed object system for Ruby")
    (description "Distributed object system for Ruby.")
    (home-page "https://github.com/ruby/drb")
    (license (list license:ruby license:bsd-2))))

(define-public ruby-metasploit-concern
  (package
    (name "ruby-metasploit-concern")
    (version "5.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "metasploit-concern" version))
       (sha256
        (base32 "11frhqg5vh9dqk3q3lv5grdd3r8yhxz2qpq87wrr3cl31569bg1p"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-activemodel
                             ruby-activesupport
                             ruby-drb
                             ruby-mutex-m
                             ruby-railties
                             ruby-zeitwerk))
    (synopsis
     "Automatically includes Modules from app/concerns/<module_with_concerns>/<concern>.rb into <module_with_concerns> to ease monkey-patching associations and validations on ActiveRecord::Base descendents from other gems when layering schemas.")
    (description
     "Automatically includes Modules from
app/concerns/<module_with_concerns>/<concern>.rb into <module_with_concerns> to
ease monkey-patching associations and validations on @code{ActiveRecord::Base}
descendents from other gems when layering schemas.")
    (home-page "https://github.com/rapid7/metasploit-concern")
    (license license:bsd-3)))

(define-public ruby-metasm
  (package
    (name "ruby-metasm")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "metasm" version))
       (sha256
        (base32 "1j6c64fnsifna8x7b6bcpb286fiiapn7fnqrc1zwpxjdw78fg1mg"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Metasm is a cross-architecture assembler, disassembler, linker, and debugger.")
    (description
     "Metasm is a cross-architecture assembler, disassembler, linker, and debugger.")
    (home-page "http://metasm.cr0.org")
    (license license:lgpl2.1)))

(define-public ruby-rkelly-remix
  (package
    (name "ruby-rkelly-remix")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rkelly-remix" version))
       (sha256
        (base32 "1g7hjl9nx7f953y7lncmfgp0xgxfxvgfm367q6da9niik6rp1y3j"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "RKelly Remix is a fork of the
RKelly[https://github.com/tenderlove/rkelly] JavaScript parser")
    (description
     "RKelly Remix is a fork of the RKelly[https://github.com/tenderlove/rkelly]
@code{JavaScript} parser.")
    (home-page "https://github.com/nene/rkelly-remix")
    (license license:expat)))

(define-public ruby-jsobfu
  (package
    (name "ruby-jsobfu")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "jsobfu" version))
       (sha256
        (base32 "1hchns89cfj0gggm2zbr7ghb630imxm2x2d21ffx2jlasn9xbkyk"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-rkelly-remix))
    (synopsis "Javascript code obfuscator")
    (description "This package provides a Javascript code obfuscator.")
    (home-page "https://github.com/rapid7/jsobfu")
    (license license:bsd-3)))

(define-public ruby-hrr-rb-ssh
  (package
    (name "ruby-hrr-rb-ssh")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "hrr_rb_ssh" version))
       (sha256
        (base32 "1dr6mv98ll0crdn2wm2yy9ywh130iljcsvnnvs6639k19qbfk7qf"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Pure Ruby SSH 2.0 server and client implementation")
    (description "Pure Ruby SSH 2.0 server and client implementation.")
    (home-page "https://github.com/hirura/hrr_rb_ssh")
    (license license:asl2.0)))

(define-public ruby-filesize
  (package
    (name "ruby-filesize")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "filesize" version))
       (sha256
        (base32 "17p7rf1x7h3ivaznb4n4kmxnnzj25zaviryqgn2n12v2kmibhp8g"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "filesize is a small class for handling filesizes with both the SI and binary prefixes, allowing conversion from any size to any other size.")
    (description
     "filesize is a small class for handling filesizes with both the SI and binary
prefixes, allowing conversion from any size to any other size.")
    (home-page "https://github.com/dominikh/filesize")
    (license license:expat)))

(define-public ruby-faye-websocket
  (package
    (name "ruby-faye-websocket")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "faye-websocket" version))
       (sha256
        (base32 "1qa2mf22893cf4w5zfqmmwli2rjpjrr51r7fp89hlv9hs3y7v7xd"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-eventmachine ruby-websocket-driver))
    (synopsis "Standards-compliant WebSocket server and client")
    (description "Standards-compliant @code{WebSocket} server and client.")
    (home-page "https://github.com/faye/faye-websocket-ruby")
    (license license:asl2.0)))

(define-public ruby-http-parser-rb
  (package
    (name "ruby-http-parser-rb")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "http_parser.rb" version))
       (sha256
        (base32 "1gj4fmls0mf52dlr928gaq0c0cb0m3aqa9kaa6l0ikl2zbqk42as"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Ruby bindings to https://github.com/joyent/http-parser and https://github.com/http-parser/http-parser.java")
    (description "Ruby bindings to https://github.com/joyent/http-parser and
https://github.com/http-parser/http-parser.java.")
    (home-page "https://github.com/tmm1/http_parser.rb")
    (license license:expat)))

(define-public ruby-em-socksify
  (package
    (name "ruby-em-socksify")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "em-socksify" version))
       (sha256
        (base32 "1vbl74x9m4hccmmhcnp36s50mn7d81annfj3fcqjdhdcm2khi3bx"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-base64 ruby-eventmachine))
    (synopsis "Transparent proxy support for any EventMachine protocol")
    (description
     "Transparent proxy support for any @code{EventMachine} protocol.")
    (home-page "https://github.com/igrigorik/em-socksify")
    (license license:expat)))

(define-public ruby-cookiejar
  (package
    (name "ruby-cookiejar")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cookiejar" version))
       (sha256
        (base32 "1px0zlnlkwwp9prdkm2lamgy412y009646n2cgsa1xxsqk7nmc8i"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Allows for parsing and returning cookies in Ruby HTTP client code")
    (description
     "Allows for parsing and returning cookies in Ruby HTTP client code.")
    (home-page "http://alkaline-solutions.com")
    (license license:bsd-2)))

(define-public ruby-em-http-request
  (package
    (name "ruby-em-http-request")
    (version "1.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "em-http-request" version))
       (sha256
        (base32 "1azx5rgm1zvx7391sfwcxzyccs46x495vb34ql2ch83f58mwgyqn"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-addressable ruby-cookiejar ruby-em-socksify
                             ruby-eventmachine ruby-http-parser-rb))
    (synopsis "EventMachine based, async HTTP Request client")
    (description "@code{EventMachine} based, async HTTP Request client.")
    (home-page "http://github.com/igrigorik/em-http-request")
    (license license:expat)))

(define-public ruby-simpleidn
  (package
    (name "ruby-simpleidn")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "simpleidn" version))
       (sha256
        (base32 "0a9c1mdy12y81ck7mcn9f9i2s2wwzjh1nr92ps354q517zq9dkh8"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "This gem allows easy conversion from punycode ACE strings to unicode UTF-8 strings and vice-versa.")
    (description
     "This gem allows easy conversion from punycode ACE strings to unicode UTF-8
strings and vice-versa.")
    (home-page "https://github.com/mmriis/simpleidn")
    (license license:expat)))

(define-public ruby-dnsruby
  (package
    (name "ruby-dnsruby")
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "dnsruby" version))
       (sha256
        (base32 "14v554cb5b14wikzlkhfbr8ms97rjx5ysci0bs8v8qlffrn770vs"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-base64 ruby-logger ruby-simpleidn))
    (synopsis "Dnsruby is a pure Ruby DNS client library which implements a
stub resolver. It aims to comply with all DNS RFCs, including
DNSSEC NSEC3 support")
    (description
     "Dnsruby is a pure Ruby DNS client library which implements a stub resolver.  It
aims to comply with all DNS RFCs, including DNSSEC NSEC3 support.")
    (home-page "https://github.com/alexdalitz/dnsruby")
    (license license:asl2.0)))

(define-public ruby-bson
  (package
    (name "ruby-bson")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bson" version))
       (sha256
        (base32 "02q695963ix1qcdwy205rwd38mv089z54n2i5n3ih1kad864jz55"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Fully featured BSON specification implementation in Ruby")
    (description
     "This package provides a fully featured BSON specification implementation in
Ruby.")
    (home-page
     "https://www.mongodb.com/docs/ruby-driver/current/tutorials/bson-v4/")
    (license license:asl2.0)))

(define-public ruby-bit-struct
  (package
    (name "ruby-bit-struct")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bit-struct" version))
       (sha256
        (base32 "1r8l4zxfqygybsvh7q0wzrbrfxv3dbgbha38xlhdnh0q9h5pmf0j"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Library for packed binary data stored in ruby Strings. Useful for accessing fields in network packets and binary files.")
    (description
     "Library for packed binary data stored in ruby Strings.  Useful for accessing
fields in network packets and binary files.")
    (home-page "https://github.com/vjoel/bit-struct")
    (license license:ruby)))

(define-public ruby-aws-sdk-kms
  (package
    (name "ruby-aws-sdk-kms")
    (version "1.99.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-sdk-kms" version))
       (sha256
        (base32 "1a3mh89kfh6flqxw48wfv9wfwkj2zxazw096mqm56wnnzz1jyads"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-aws-sdk-core ruby-aws-sigv4))
    (synopsis
     "Official AWS Ruby gem for AWS Key Management Service (KMS). This gem is part of the AWS SDK for Ruby.")
    (description
     "Official AWS Ruby gem for AWS Key Management Service (KMS).  This gem is part of
the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-sdk-s3
  (package
    (name "ruby-aws-sdk-s3")
    (version "1.182.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-sdk-s3" version))
       (sha256
        (base32 "03a55dbihv6xvgfwhx0f35rwc7q3rr0555vfpxlwpdjw75wkbz6h"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-aws-sdk-core ruby-aws-sdk-kms ruby-aws-sigv4))
    (synopsis
     "Official AWS Ruby gem for Amazon Simple Storage Service (Amazon S3). This gem is part of the AWS SDK for Ruby.")
    (description
     "Official AWS Ruby gem for Amazon Simple Storage Service (Amazon S3).  This gem
is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-sdk-iam
  (package
    (name "ruby-aws-sdk-iam")
    (version "1.119.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-sdk-iam" version))
       (sha256
        (base32 "0irr4dldji1n3gqkg4li52cs6fj1xibb1c8i7bhsg5pjdv7a4vzz"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-aws-sdk-core ruby-aws-sigv4))
    (synopsis
     "Official AWS Ruby gem for AWS Identity and Access Management (IAM). This gem is part of the AWS SDK for Ruby.")
    (description
     "Official AWS Ruby gem for AWS Identity and Access Management (IAM).  This gem is
part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-jmespath
  (package
    (name "ruby-jmespath")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "jmespath" version))
       (sha256
        (base32 "1cdw9vw2qly7q7r41s7phnac264rbsdqgj4l0h4nqgbjb157g393"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Implements JMESPath for Ruby")
    (description "This package implements JMESPath for Ruby.")
    (home-page "http://github.com/trevorrowe/jmespath.rb")
    (license license:asl2.0)))

(define-public ruby-aws-sigv4
  (package
    (name "ruby-aws-sigv4")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-sigv4" version))
       (sha256
        (base32 "1nx1il781qg58nwjkkdn9fw741cjjnixfsh389234qm8j5lpka2h"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-aws-eventstream))
    (synopsis
     "Amazon Web Services Signature Version 4 signing library. Generates sigv4 signature for HTTP requests.")
    (description
     "Amazon Web Services Signature Version 4 signing library.  Generates sigv4
signature for HTTP requests.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-partitions
  (package
    (name "ruby-aws-partitions")
    (version "1.1065.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-partitions" version))
       (sha256
        (base32 "178v2dgrqb5x2xnmfn05a325qza0ymbbcqm4qbljd2clldb0yjiq"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Provides interfaces to enumerate AWS partitions, regions, and services.")
    (description
     "This package provides interfaces to enumerate AWS partitions, regions, and
services.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-eventstream
  (package
    (name "ruby-aws-eventstream")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-eventstream" version))
       (sha256
        (base32 "1mvjjn8vh1c3nhibmjj9qcwxagj6m9yy961wblfqdmvhr9aklb3y"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis
     "Amazon Web Services event stream library. Decodes and encodes binary stream under `vnd.amazon.event-stream` content-type")
    (description
     "Amazon Web Services event stream library.  Decodes and encodes binary stream
under `vnd.amazon.event-stream` content-type.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-sdk-core
  (package
    (name "ruby-aws-sdk-core")
    (version "3.220.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-sdk-core" version))
       (sha256
        (base32 "0cjdqmy5hjbgzc0iac1i143va76qgp7jc7hg1aviy1n8cgywq44y"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-aws-eventstream
                             ruby-aws-partitions
                             ruby-aws-sigv4
                             ruby-base64
                             ruby-bigdecimal
                             ruby-jmespath
                             ruby-logger
                             ruby-rexml))
    (synopsis
     "Provides API clients for AWS. This gem is part of the official AWS SDK for Ruby.")
    (description
     "This package provides API clients for AWS. This gem is part of the official AWS
SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-sdk-ec2
  (package
    (name "ruby-aws-sdk-ec2")
    (version "1.511.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aws-sdk-ec2" version))
       (sha256
        (base32 "1y6cxsg3648m14v0ccyp7mvqglg9gafvy0vnm1nb9b2mrjz5z4rk"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-aws-sdk-core ruby-aws-sigv4))
    (synopsis
     "Official AWS Ruby gem for Amazon Elastic Compute Cloud (Amazon EC2). This gem is part of the AWS SDK for Ruby.")
    (description
     "Official AWS Ruby gem for Amazon Elastic Compute Cloud (Amazon EC2).  This gem
is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))


;; The Metasploit Framework itself: installed as a source tree with wrapper
;; scripts.  Keeps bundler (config/boot.rb), resolving against the pinned gems
;; above via GEM_PATH.  Payload gems ship prebuilt binaries.
(define (ruby-without-tests pkg)
  (package
    (inherit pkg)
    (arguments
     (substitute-keyword-arguments (package-arguments pkg)
       ((#:tests? _ #f) #f)))))

;; A few transitive dev/test gems fail their own suites when built from
;; source (no substitute); tests are irrelevant to the framework at runtime.
(define %msf-skip-flaky-tests
  (package-input-rewriting/spec
   (map (lambda (n) (cons n ruby-without-tests))
        '("ruby-byebug" "ruby-pry-byebug" "ruby-ruby-prof"
          "ruby-memory-profiler" "ruby-puma"))))

(define-public metasploit-framework
  (%msf-skip-flaky-tests
  (package
    (name "metasploit-framework")
    (version "6.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rapid7/metasploit-framework")
             (commit "e25d6ee62a0c7f7df27f367c8028859fdf8136a7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "077kdsg630v2rlh9cshqybpaxzzl4agpj3js6vf1w1jmzgxr0c3j"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((share (string-append #$output "/share/metasploit-framework"))
                     (bin (string-append #$output "/bin"))
                     (ruby (string-append #$(this-package-input "ruby") "/bin/ruby"))
                     (gem-path
                      (string-join
                       (filter-map
                        (lambda (entry)
                          (let ((d (string-append (cdr entry)
                                                  "/lib/ruby/vendor_ruby")))
                            (and (directory-exists? d) d)))
                        inputs)
                       ":")))
                (mkdir-p share)
                (copy-recursively "." share)
                (mkdir-p bin)
                (for-each
                 (lambda (prog)
                   (let ((w (string-append bin "/" prog)))
                     (call-with-output-file w
                       (lambda (port)
                         (format port "#!~a/bin/bash~%"
                                 #$(this-package-input "bash-minimal"))
                         (format port "export GEM_PATH=~a~%" gem-path)
                         (format port "export BUNDLE_GEMFILE=~a/Gemfile~%" share)
                         (format port "export BUNDLE_WITHOUT=development:test:coverage~%")
                         ;; rb-readline shells out to infocmp (ncurses).
                         (format port "export PATH=~a/bin${PATH:+:$PATH}~%"
                                 #$(this-package-input "ncurses"))
                         (format port "exec ~a ~a/~a \"$@\"~%" ruby share prog)))
                     (chmod w #o755)))
                 '("msfconsole" "msfvenom" "msfd" "msfrpc" "msfrpcd"
                   "msfdb" "msfupdate"))))))))
    (inputs
     (list ruby bash-minimal ncurses
           ruby-aarch64 ruby-syslog ruby-win32api ruby-uri ruby-pp
           ruby-net-http ruby-rack-session ruby-mini-portile-2 ruby-ascii85
           ruby-actionpack ruby-actionview ruby-activemodel
           ruby-activerecord ruby-activesupport ruby-addressable ruby-ast
           ruby-bcrypt ruby-bcrypt-pbkdf ruby-benchmark ruby-bigdecimal
           ruby-bootsnap ruby-coderay ruby-concurrent-ruby
           ruby-connection-pool ruby-crack ruby-crass ruby-daemons ruby-date
           ruby-debug ruby-docile ruby-domain-name ruby-ed25519 ruby-erubi
           ruby-faker ruby-faraday ruby-faraday-net-http ruby-ffi
           ruby-hashdiff ruby-http-accept ruby-httpclient ruby-http-cookie
           ruby-i18n ruby-io-console ruby-ipaddr ruby-irb ruby-json
           ruby-language-server-protocol ruby-mime-types
           ruby-mime-types-data ruby-minitest ruby-msgpack ruby-mustermann
           ruby-net-imap ruby-net-protocol ruby-net-smtp ruby-net-ssh
           ruby-nio4r ruby-nokogiri ruby-nori ruby-octokit ruby-ostruct
           ruby-parallel ruby-parser ruby-pdf-reader ruby-pg ruby-prism
           ruby-pry ruby-psych ruby-public-suffix ruby-puma ruby-rack
           ruby-rack-protection ruby-rack-test ruby-rails-dom-testing
           ruby-rails-html-sanitizer ruby-railties ruby-rdoc
           ruby-regexp-parser ruby-reline ruby-rexml ruby-rspec-core
           ruby-rspec-expectations ruby-rspec-mocks ruby-rspec-rails
           ruby-rspec-support ruby-rubocop ruby-rubocop-ast ruby-rubyzip
           ruby-simplecov ruby-simplecov-html ruby-sinatra ruby-sqlite3
           ruby-stringio ruby-thin ruby-tilt ruby-timecop ruby-ttfunk
           ruby-tzinfo ruby-tzinfo-data ruby-unicode-emoji ruby-useragent
           ruby-warden ruby-webmock ruby-webrick ruby-websocket-driver
           ruby-websocket-extensions ruby-xml-simple ruby-yard ruby-zeitwerk
           ruby-winrm ruby-gssapi ruby-gyoku ruby-json-schemer ruby-hana
           ruby-logging ruby-little-plugger ruby-lru-redux
           ruby-faraday-retry ruby-elftools ruby-tomlrb ruby-unix-crypt
           ruby-with-env ruby-require-all ruby-ruby-mysql ruby-aws-sdk-ssm
           ruby-aws-sdk-ec2instanceconnect ruby-swagger-blocks ruby-mcp
           ruby-hrr-rb-ssh-ed25519 ruby-xmlrpc ruby-xdr ruby-sshkey
           ruby-windows-error ruby-strptime ruby-rasn1 ruby-openssl-cmac
           ruby-bindata ruby-ruby-smb ruby-macho ruby-rex-zip
           ruby-rex-sslscan ruby-rex-rop-builder ruby-rex-registry
           ruby-rex-random-identifier ruby-rex-powershell ruby-rex-ole
           ruby-rex-nop ruby-rex-mime ruby-rex-java ruby-rex-exploitation
           ruby-rex-encoder ruby-rex-struct2 ruby-rex-bin-tools
           ruby-rex-text ruby-rex-arch ruby-rb-readline ruby-patch-finder
           ruby-pcaprub ruby-packetfu ruby-openvas-omp ruby-openssl-ccm
           ruby-nexpose ruby-network-interface ruby-net-ldap
           ruby-nessus-rest ruby-mqtt ruby-metasploit-payloads-mettle
           ruby-metasploit-payloads ruby-rubyntlm ruby-rex-core
           ruby-rex-socket ruby-recog ruby-metasploit-model
           ruby-arel-helpers ruby-metasploit-data-models ruby-csv
           ruby-metasploit-credential ruby-drb ruby-metasploit-concern
           ruby-metasm ruby-rkelly-remix ruby-jsobfu ruby-hrr-rb-ssh
           ruby-filesize ruby-faye-websocket ruby-http-parser-rb
           ruby-em-socksify ruby-cookiejar ruby-em-http-request
           ruby-simpleidn ruby-dnsruby ruby-bson ruby-bit-struct
           ruby-aws-sdk-kms ruby-aws-sdk-s3 ruby-aws-sdk-iam ruby-jmespath
           ruby-aws-sigv4 ruby-aws-partitions ruby-aws-eventstream
           ruby-aws-sdk-core ruby-aws-sdk-ec2 ruby-afm ruby-base64
           ruby-builder ruby-byebug ruby-chunky-png ruby-diff-lcs ruby-erb
           ruby-eventmachine ruby-fivemat ruby-hashery ruby-lint-roller
           ruby-logger ruby-loofah ruby-memory-profiler ruby-method-source
           ruby-multi-json ruby-mutex-m ruby-netrc ruby-net-sftp
           ruby-pry-byebug ruby-racc ruby-rackup ruby-rainbow ruby-rake
           ruby-redcarpet ruby-rest-client ruby-rspec ruby-rspec-rerun
           ruby-ruby2-keywords ruby-ruby-prof ruby-sawyer ruby-securerandom
           ruby-thor ruby-timeout ruby-tsort ruby-unicode-display-width))
    (home-page "https://www.metasploit.com/")
    (synopsis "Penetration testing framework")
    (description
     "The Metasploit Framework is a platform for developing, testing, and
executing exploit code against a remote target.  It includes a large database
of exploits, payloads, encoders, and auxiliary modules, driven through the
@command{msfconsole} interface and the @command{msfvenom} payload
generator.  Payload components ship as prebuilt binaries.")
    (license (list license:bsd-3 license:expat)))))
