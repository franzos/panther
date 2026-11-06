;;; Package Repository for GNU Guix
;;; Copyright © 2026 Franz Geffke <mail@gofranz.com>

(define-module (px services browser)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (chrome-policies-configuration
            chrome-policies-configuration?
            chrome-policies-configuration-directory
            chrome-policies-configuration-policies
            chrome-policies-service-type
            %default-chrome-policies))

;;;
;;; Chrome and Chromium read every *.json under their managed policy
;;; directory at launch and apply the union as enterprise policy, which is the
;;; only way to set these knobs system-wide without touching a profile.
;;;

;; Two values are deliberately not the most restrictive: SafeBrowsingProtectionLevel
;; 1 keeps standard phishing and malware blocking (0 removes it), and
;; ChromeVariations 1 still fetches the seed but applies only critical and
;; security fixes (2 stops the fetch and loses the kill switches). Sync is left
;; alone. Web Bluetooth is off because Chrome opens a BLE discovery scan at
;; launch and never closes it, which keeps bonded LE peripherals from
;; reconnecting to the host.
(define %default-chrome-policies
  '(("MetricsReportingEnabled" . #f)
    ("UrlKeyedAnonymizedDataCollectionEnabled" . #f)
    ("DefaultGeolocationSetting" . 2)
    ("DefaultWebBluetoothGuardSetting" . 2)
    ("DomainReliabilityAllowed" . #f)
    ("ChromeVariations" . 1)
    ("SafeBrowsingProtectionLevel" . 1)
    ("SafeBrowsingExtendedReportingEnabled" . #f)
    ("SearchSuggestEnabled" . #f)
    ("AlternateErrorPagesEnabled" . #f)
    ("SpellCheckServiceEnabled" . #f)
    ("NetworkPredictionOptions" . 2)
    ("BackgroundModeEnabled" . #f)
    ("EnableMediaRouter" . #f)
    ("DefaultBrowserSettingEnabled" . #f)
    ("PromotionalTabsEnabled" . #f)
    ("FeedbackSurveysEnabled" . #f)
    ("PrivacySandboxAdTopicsEnabled" . #f)
    ("PrivacySandboxSiteEnabledAdsEnabled" . #f)
    ("PrivacySandboxAdMeasurementEnabled" . #f)))

(define (chrome-policies? value)
  (and (list? value)
       (every (lambda (entry)
                (and (pair? entry) (string? (car entry))))
              value)))

(define-configuration/no-serialization chrome-policies-configuration
  (directory
   (string "/etc/opt/chrome/policies/managed")
   "The managed policy directory, under @file{/etc}.  Google Chrome reads
@file{/etc/opt/chrome/policies/managed}, Chromium
@file{/etc/chromium/policies/managed}.")
  (policies
   (chrome-policies %default-chrome-policies)
   "Alist of policy names to values, written as JSON.  Booleans, numbers and
strings map directly; a list-valued policy is given as a vector, such as
@code{#(\"example.com\")}.  See @url{https://chromeenterprise.google/policies/}
for the names."))

(define (chrome-policies-file config)
  (plain-file "policies.json"
              (string-append
               (scm->json-string (chrome-policies-configuration-policies config)
                                 #:pretty #t)
               "\n")))

(define (chrome-policies-etc config)
  (let ((directory (chrome-policies-configuration-directory config)))
    (unless (string-prefix? "/etc/" directory)
      (error (format #f "chrome-policies-configuration: 'directory' must be under /etc: ~s"
                     directory)))
    (list (list (string-append (string-drop directory (string-length "/etc/"))
                               "/policies.json")
                (chrome-policies-file config)))))

(define chrome-policies-service-type
  (service-type
   (name 'chrome-policies)
   (extensions
    (list (service-extension etc-service-type chrome-policies-etc)))
   (default-value (chrome-policies-configuration))
   (description
    "Install a managed policy file for Google Chrome or Chromium, with a
privacy-oriented default set.")))
