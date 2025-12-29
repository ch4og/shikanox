;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika system config)
  #:use-module (shika system os)
  #:use-module (nonguix utils)
  #:use-module (nonguix transformations))

(define-public %shika-os
  (make-shika-os "ch" "noko"))

(define-public %shika-os-nvidia
  ((compose (nonguix-transformation-nvidia)
            (nonguix-transformation-linux))
   %shika-os))

%shika-os-nvidia
