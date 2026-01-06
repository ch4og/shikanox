;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Home service to reload waybar to apply new configuration
;;;
;;; Examples:
;;;   (service home-reload-waybar-service-type)

(define-module (shika services home-reload-waybar)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix records)
  #:use-module (guix gexp))

(define-public home-reload-waybar-service-type
  (service-type
   (name 'home-reload-waybar)
   (description "Reload waybar configuration")
   (extensions
    (list
     (service-extension home-activation-service-type
                        (lambda _
                          #~(begin
                              (use-modules (guix gexp))
                              (invoke "pgrep" "waybar")
                              (invoke "killall" "-SIGUSR2" "waybar"))))))
   (default-value #f)))

