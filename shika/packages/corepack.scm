;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages corepack)
  #:use-module (guix deprecation))

;; INFO: Deprecated at 2026-08-18.
(define-deprecated/public-alias corepack-yarn
  (@ (shika packages development) corepack-yarn))

;; INFO: Deprecated at 2026-08-18.
(define-deprecated/public-alias corepack-pnpm
  (@ (shika packages development) corepack-pnpm))
