;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px services input)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu packages linux) #:select (interception-tools))
  #:use-module ((px packages linux) #:select (mouse-debounce))

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (px-mouse-debounce-configuration
            px-mouse-debounce-configuration?
            px-mouse-debounce-service-type))

;;
;; px-mouse-debounce-service-type
;;
;; Runs udevmon over one pointing device, piping its evdev stream through the
;; mouse-debounce filter and back out on a uinput clone.  libinput debounces
;; buttons too, but only over a 12 to 25 millisecond window it doesn't expose,
;; so a switch that bounces for longer has to be caught below it.
;;

(define-record-type* <px-mouse-debounce-configuration>
                     px-mouse-debounce-configuration
                     make-px-mouse-debounce-configuration
  px-mouse-debounce-configuration?
  (package px-mouse-debounce-configuration-package
           (default mouse-debounce)
           (docstring "The package providing the mouse-debounce filter"))
  (interception px-mouse-debounce-configuration-interception
                (default interception-tools)
                (docstring "The package providing intercept, uinput and udevmon"))
  ;; Full match, not a substring; see /proc/bus/input/devices for the names.
  (device-name px-mouse-debounce-configuration-device-name
               (docstring "Regex matching the evdev name of the device to filter"))
  (hold-ms px-mouse-debounce-configuration-hold-ms
           (default 70)
           (docstring "How long to withhold a suspect release, in milliseconds"))
  (short-ms px-mouse-debounce-configuration-short-ms
            (default 80)
            (docstring "A release this soon after its press is suspect"))
  (buttons px-mouse-debounce-configuration-buttons
           (default '(272))
           (docstring "evdev button codes to debounce; 272 is BTN_LEFT"))
  (log-file px-mouse-debounce-configuration-log-file
            (default "/var/log/mouse-debounce.log")
            (docstring "Where udevmon writes its output")))

(define (mouse-debounce-udevmon-config config)
  (match-record config <px-mouse-debounce-configuration>
                (package interception device-name hold-ms short-ms buttons)
    (let ((button-list (string-join (map number->string buttons) ",")))
      (mixed-text-file
       "udevmon.yaml"
       "- JOB: \"" interception "/bin/intercept -g $DEVNODE"
       " | " package "/bin/mouse-debounce"
       " --hold-ms " (number->string hold-ms)
       " --short-ms " (number->string short-ms)
       " --buttons " button-list
       " | " interception "/bin/uinput -d $DEVNODE\"\n"
       "  DEVICE:\n"
       "    NAME: \"" device-name "\"\n"
       ;; A mouse presents several event nodes; this pins the job to the one
       ;; carrying the buttons.
       "    EVENTS:\n"
       "      EV_KEY: [" button-list "]\n"))))

(define (mouse-debounce-shepherd-service config)
  (match-record config <px-mouse-debounce-configuration>
                (interception log-file)
    (list (shepherd-service (documentation
                             "Filter microswitch chatter out of a mouse button stream")
                            (provision '(mouse-debounce))
                            (requirement '(udev))
                            (start #~(make-forkexec-constructor
                                      (list #$(file-append interception "/bin/udevmon")
                                            "-c"
                                            #$(mouse-debounce-udevmon-config config))
                                      #:log-file #$log-file))
                            (stop #~(make-kill-destructor))))))

(define px-mouse-debounce-service-type
  (service-type (name 'mouse-debounce)
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   mouse-debounce-shepherd-service)))
                (description
                 "Debounce a worn mouse microswitch by filtering its evdev
stream through @code{mouse-debounce} before libinput sees it.")))
