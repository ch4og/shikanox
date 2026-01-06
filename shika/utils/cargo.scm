;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika utils cargo)
  #:use-module (srfi srfi-26)
  #:use-module (guix build-system cargo)
  #:export (shika-cargo-inputs))

(define shika-cargo-inputs
  (cut cargo-inputs <> #:module '(shika packages rust-crates)))
