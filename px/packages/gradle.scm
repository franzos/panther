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
    (version "8.14.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://services.gradle.org/distributions/gradle-"
             version "-bin.zip"))
       (sha256
        (base32 "0lgngaf7bwy7yn344i5sjwc0zsk2hxa4ifz61hnnvjn35xlqknll"))))))

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
