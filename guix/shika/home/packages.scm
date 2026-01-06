;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika home packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (shika home packages apps)
  #:use-module (shika home packages browser)
  #:use-module (shika home packages development)
  #:use-module (shika home packages fonts)
  #:use-module (shika home packages gaming)
  #:use-module (shika home packages services)
  #:use-module (shika home packages theming)
  #:use-module (shika home packages utils)
  #:use-module (shika home packages wayland))

(define (load-category category)
  (module-ref (resolve-interface `(shika home packages ,category))
              (symbol-append '%shika- category '-home-packages)))

(define %package-categories
  '(apps
    browser
    development
    fonts
    gaming
    services
    theming
    utils
    wayland))

(define-public %shika-home-packages
  (apply append (map load-category %package-categories)))
