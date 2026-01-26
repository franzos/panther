;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages gradle)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (nongnu packages gradle)
  #:export (gradle-8 gradle-7))

(define-public gradle-8
  (package
    (inherit gradle)
    (version "8.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://services.gradle.org/distributions/gradle-"
             version "-bin.zip"))
       (sha256
        (base32 "039q203g9kg0c91v39lv7k1pz88q9s63fqmgknibav8glyc14xzi"))))))

(define-public gradle-7
  (package
    (inherit gradle)
    (version "7.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://services.gradle.org/distributions/gradle-"
             version "-bin.zip"))
       (sha256
        (base32 "1q1vcyldkyhn2v8xjfw90wdhbwgbsqrd4a9kzi471g03ydv9fgb7"))))))
