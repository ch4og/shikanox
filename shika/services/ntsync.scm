;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Service to load the ntsync kernel module.
;;;
;;; Examples:
;;;   (service ntsync-service-type)

(define-module (shika services ntsync)
  #:use-module (gnu services)
  #:use-module (gnu services linux)
  #:use-module (guix records))

(define-public ntsync-service-type
  (service-type
   (name 'ntsync)
   (description "Load ntsync kernel module.")
   (extensions
    (list (service-extension kernel-module-loader-service-type
                             (const '("ntsync")))))
   (default-value #f)))
