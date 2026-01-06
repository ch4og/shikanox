;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika home packages development)
  #:use-module (guix utils)
  #:use-module (gnu packages))

(define-public %shika-development-home-packages
  (specifications->packages
   '("direnv"
     "jujutsu"
     "lazygit"
     "maak"
     "reuse"
     "vscodium")))
