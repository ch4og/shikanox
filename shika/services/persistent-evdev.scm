;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Example:
;;;   (service persistent-evdev-service-type
;;;            (persistent-evdev-configuration
;;;             (devices
;;;              '(("persist-mouse0" . "usb-Logitech_G403-event-if01")
;;;                ("persist-mouse1" . "usb-Logitech_G403-event-mouse")
;;;                ("persist-keyboard0" . "usb-Microsoft_Natural-event-kbd")))))

(define-module (shika services persistent-evdev)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (json)
  #:use-module (shika packages persistent-evdev)
  #:use-module (srfi srfi-13)
  #:export (persistent-evdev-configuration
            persistent-evdev-configuration?
            persistent-evdev-configuration-devices
            persistent-evdev-service-type))

(define-configuration/no-serialization persistent-evdev-configuration
  (devices
   (list '())
   "An alist mapping string names to names below /dev/input/by-id/."))

(define (persistent-evdev-device device)
  (let ((name (car device))
        (path (cdr device)))
    (cons name
          (if (string-prefix? "/" path)
              path
              (string-append "/dev/input/by-id/" path)))))

(define (persistent-evdev-config config)
  (plain-file
   "persistent-evdev.json"
   (scm->json-string
    `((cache . "/var/cache/persistent-evdev")
      (devices .
       ,(map persistent-evdev-device
             (persistent-evdev-configuration-devices config))))
    #:pretty #t)))

(define (persistent-evdev-activation _)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p "/var/cache/persistent-evdev"))))

(define (persistent-evdev-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Run persistent-evdev.")
    (provision '(persistent-evdev))
    (requirement '(udev user-processes))
    (start #~(make-forkexec-constructor
              (list #$(file-append persistent-evdev "/bin/persistent-evdev")
                    #$(persistent-evdev-config config))))
    (stop #~(make-kill-destructor))
    (respawn? #t))))

(define-public persistent-evdev-service-type
  (service-type
   (name 'persistent-evdev)
   (extensions
    (list (service-extension shepherd-root-service-type
                             persistent-evdev-shepherd-service)
          (service-extension activation-service-type
                             persistent-evdev-activation)
          (service-extension udev-service-type
                             (const (list persistent-evdev)))))
   (default-value (persistent-evdev-configuration))
   (description "Run persistent-evdev and install its udev rules.")))
