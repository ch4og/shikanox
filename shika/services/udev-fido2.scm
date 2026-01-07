;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; FIDO2 udev rules service to allow specified groups to access security keys.
;;;
;;; Examples:
;;;   (service udev-fido2-service-type)
;;;
;;;   (service udev-fido2-service-type
;;;            (udev-fido2-configuration
;;;             (groups '("plugdev" "users")))) ;; Add "users" group as well
;;;

(define-module (shika services udev-fido2)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages security-token)
  #:use-module (guix records)
  #:export (udev-fido2-configuration
            udev-fido2-configuration?
            udev-fido2-service-type))

(define-record-type* <udev-fido2-configuration>
  udev-fido2-configuration make-udev-fido2-configuration
  udev-fido2-configuration?
  (groups udev-fido2-configuration-groups
          (default '("plugdev"))))

(define (udev-fido2-accounts config)
  (map (lambda (group)
         (user-group (name group) (system? #t)))
       (udev-fido2-configuration-groups config)))

(define (udev-fido2-rules config)
  (list libfido2))

(define-public udev-fido2-service-type
  (service-type
    (name 'udev-fido2)
    (description "Udev rules for FIDO2 security keys.")
    (extensions
     (list (service-extension udev-service-type
                              udev-fido2-rules)
           (service-extension account-service-type
                              udev-fido2-accounts)))
    (default-value (udev-fido2-configuration))))
