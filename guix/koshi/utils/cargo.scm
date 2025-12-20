;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (koshi utils cargo)
  #:use-module (srfi srfi-26)
  #:use-module (guix build-system cargo)
  #:export (koshi-cargo-inputs))

(define koshi-cargo-inputs
  (cut cargo-inputs <> #:module '(koshi packages rust-crates)))
