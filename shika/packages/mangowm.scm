;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages mangowm)
  #:use-module (guix deprecation))

;; INFO: Deprecated at 2026-08-17.
(define-deprecated/public-alias mangowm
  (@ (gnu packages window-management) mangowm))
