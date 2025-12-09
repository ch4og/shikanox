;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Automatic Guix garbage collection service.
;;;
;;; Examples:
;;;   (service guix-gc-service-type)
;;;
;;;   (service guix-gc-service-type
;;;            (guix-gc-configuration
;;;             (schedule "0 2 * * *")        ; At 02:00 daily
;;;             (delete-generations "3d")))   ; Delete generations older than 3 days
;;;
;;;   (service guix-gc-service-type
;;;            (guix-gc-configuration
;;;             (delete-generations #f)))     ; Just 'guix gc'

(define-module (koshi services guix-gc)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (guix-gc-configuration
            guix-gc-configuration?
            guix-gc-configuration-schedule
            guix-gc-configuration-delete-generations
            guix-gc-service-type))

(define-record-type* <guix-gc-configuration>
  guix-gc-configuration make-guix-gc-configuration
  guix-gc-configuration?
  (schedule guix-gc-configuration-schedule
            (default "0 0 * * *"))
  (delete-generations guix-gc-configuration-delete-generations
                      (default "1d")))

(define (guix-gc-mcron-job config)
  (let ((schedule (guix-gc-configuration-schedule config))
        (delete-generations (guix-gc-configuration-delete-generations config)))
    (list #~(job #$schedule
                 #$(if delete-generations
                       #~(string-append "guix gc -d " #$delete-generations)
                       #~"guix gc")))))

(define guix-gc-service-type
  (service-type
   (name 'guix-gc)
   (description "Automatic Guix garbage collection service using mcron.")
   (extensions
    (list (service-extension mcron-service-type
                             guix-gc-mcron-job)))
   (default-value (guix-gc-configuration))))
