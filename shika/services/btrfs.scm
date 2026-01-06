;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Automatic Btrfs scrub and balance services for Guix System.
;;;
;;; 1. `btrfs-scrub-service-type`: runs periodic `btrfs scrub start`
;;; for one or more mounted Btrfs filesystems using mcron.
;;;
;;; 2. `btrfs-balance-service-type`: runs periodic `btrfs balance start`
;;; for one or more mounted Btrfs filesystems using mcron.
;;;
;;; Example usage:
;;;
;;;   (service btrfs-scrub-service-type
;;;            (btrfs-scrub-configuration
;;;             (schedule "0 3 * * 0")
;;;             (filesystems '("/" "/home"))))
;;;
;;;   (service btrfs-scrub-service-type
;;;            (btrfs-scrub-configuration
;;;             (filesystems '("/data"))))
;;;
;;;   (service btrfs-balance-service-type
;;;            (btrfs-balance-configuration
;;;             (schedule '(lambda (current-time) (+ current-time (* 7 24 60 60))))
;;;             (filesystems '("/" "/data"))))
;;;

(define-module (shika services btrfs)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (gnu packages linux)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (btrfs-scrub-configuration
            btrfs-scrub-configuration?
            btrfs-scrub-configuration-schedule
            btrfs-scrub-configuration-filesystems
            btrfs-scrub-service-type
            btrfs-balance-configuration
            btrfs-balance-configuration?
            btrfs-balance-configuration-schedule
            btrfs-balance-configuration-filesystems
            btrfs-balance-service-type))

(define-record-type* <btrfs-scrub-configuration>
  btrfs-scrub-configuration make-btrfs-scrub-configuration
  btrfs-scrub-configuration?
  (schedule
   btrfs-scrub-configuration-schedule
   (default "0 0 * * 0"))
  (filesystems
   btrfs-scrub-configuration-filesystems
   (default '())))

(define (btrfs-scrub-mcron-jobs config)
  (let ((schedule    (btrfs-scrub-configuration-schedule config))
        (filesystems (btrfs-scrub-configuration-filesystems config)))
    (map (lambda (fs)
           #~(job #$schedule
                  #$(file-append
                     btrfs-progs
                     (string-append "/bin/btrfs scrub start " fs))))
         filesystems)))

(define btrfs-scrub-service-type
  (service-type
   (name 'btrfs-scrub)
   (description "Periodically run btrfs scrub on set filesystems.")
   (extensions
    (list (service-extension mcron-service-type
                             btrfs-scrub-mcron-jobs)))
   (default-value (btrfs-scrub-configuration))))

(define-record-type* <btrfs-balance-configuration>
  btrfs-balance-configuration make-btrfs-balance-configuration
  btrfs-balance-configuration?
  (schedule
   btrfs-balance-configuration-schedule
   (default "0 0 * * 0"))
  (filesystems
   btrfs-balance-configuration-filesystems
   (default '())))

(define (btrfs-balance-mcron-jobs config)
  (let ((schedule    (btrfs-balance-configuration-schedule config))
        (filesystems (btrfs-balance-configuration-filesystems config)))
    (map (lambda (fs)
           #~(job #$schedule
                  #$(file-append
                     btrfs-progs
                     (string-append "/bin/btrfs balance start -dusage=10 " fs))))
         filesystems)))

(define btrfs-balance-service-type
  (service-type
   (name 'btrfs-balance)
   (description "Periodically run btrfs balance on set filesystems.")
   (extensions
    (list (service-extension mcron-service-type
                             btrfs-balance-mcron-jobs)))
   (default-value (btrfs-balance-configuration))))
