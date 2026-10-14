;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages hyperheadset)
  #:use-module (guix deprecation))

;; INFO: Deprecated at 2026-08-24.
(define-deprecated/public-alias hyperheadset
  (@ (shika packages desktop) hyperheadset))

;; INFO: Deprecated at 2026-08-24.
(define-deprecated/public-alias hyperheadset-udev-rules
  (@ (shika packages desktop) hyperheadset-udev-rules))
