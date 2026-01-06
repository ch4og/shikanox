;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika home packages theming)
  #:use-module (guix utils)
  #:use-module (gnu packages))

(define-public %shika-theming-home-packages
  (specifications->packages
   '("arashi-icon-theme"
     "bibata-cursor-theme"
     "gnome-themes-extra"
     "murrine"
     "tokyonight-gtk-theme")))
