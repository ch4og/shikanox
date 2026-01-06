;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Creates XDG runtime directory for a user at /run/user/UID.
;;;
;;; Examples:
;;;   (service runtime-dir-service-type)
;;;
;;;   (service runtime-dir-service-type
;;;            (runtime-dir-configuration
;;;             (uid "1001")))  ; For user with UID 1001

(define-module (shika services runtime-dir)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:export (runtime-dir-configuration
            runtime-dir-configuration?
            runtime-dir-service-type))

(define-record-type* <runtime-dir-configuration>
  runtime-dir-configuration make-runtime-dir-configuration
  runtime-dir-configuration?
  (uid runtime-dir-configuration-uid
       (default "1000")))

(define (runtime-dir-shepherd-services config)
  (let ((uid (runtime-dir-configuration-uid config)))
    (list
     (shepherd-service
      (documentation "Create XDG runtime dir.")
      (provision '(runtime-dir))
      (requirement '(seatd))
      (start
       #~(lambda _
           (let* ((uid #$uid)
                  (dir (string-append "/run/user/" uid)))
             (unless (file-exists? dir)
               (mkdir-p dir))
             (system* "chown" "-R" (string-append uid ":" uid) dir)
             (chmod dir #o700))))
      (one-shot? #t)))))

(define-public runtime-dir-service-type
  (service-type
   (name 'runtime-dir)
   (extensions
    (list (service-extension shepherd-root-service-type
                             runtime-dir-shepherd-services)))
   (default-value (runtime-dir-configuration))
   (description "Create XDG runtime dir.")))
