;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika home packages gaming)
  #:use-module (guix utils)
  #:use-module (gnu packages))

(define-public %shika-gaming-home-packages
  (specifications->packages
   '("gamescope"
     "heroic-nvidia"
     "honkers-railway-launcher-nvidia"
     "mangohud"
     "prismlauncher-dolly"
     "protonup"
     "steam-nvidia")))
