;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages browsers)
  #:use-module (guix deprecation))

;; INFO: Deprecated at 2026-10-14.
(define-deprecated/public-alias librewolf-sidebar-chatbot
  (@ (gnu packages librewolf) librewolf))
