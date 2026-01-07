;;; SPDX-FileCopyrightText: 2024 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Polkit rule to allow users in the 'netdev' (default) group to modify
;;; NetworkManager system settings without requiring root password.
;;;
;;; Examples:
;;;   (service polkit-network-manager-service-type)
;;;
;;;   (service polkit-network-manager-service-type
;;;            (polkit-network-manager-configuration
;;;             (group "network")))  ;; Override default group 'netdev' to 'network'

(define-module (shika services polkit-nm)
  #:use-module (gnu services dbus)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:export (polkit-network-manager-configuration
            polkit-network-manager-configuration?
            polkit-network-manager-service-type))

(define-record-type* <polkit-network-manager-configuration>
  polkit-network-manager-configuration make-polkit-network-manager-configuration
  polkit-network-manager-configuration?
  (group polkit-network-manager-configuration-group
         (default "netdev")))

(define (polkit-network-manager-package config)
  (let ((group (polkit-network-manager-configuration-group config)))
    (file-union
     "polkit-wheel"
     `(("share/polkit-1/rules.d/org.freedesktop.NetworkManager.rules"
        ,(plain-file
          "org.freedesktop.NetworkManager.rules"
          (string-join
           `("polkit.addRule(function(action, subject) {"
             "  if (action.id == \"org.freedesktop.NetworkManager.settings.modify.system\" &&"
             ,(string-append "	subject.local && subject.active && subject.isInGroup (\"" group "\")) {")
             "    return polkit.Result.YES;"
             "  }"
             "});")
           "\n")))))))

(define-public polkit-network-manager-service-type
  (service-type
    (name 'polkit-network-manager)
    (extensions
     (list (service-extension
            polkit-service-type
            (lambda (config)
              `(,(polkit-network-manager-package config))))))
    (default-value (polkit-network-manager-configuration))
    (description
     "Allow users from specified group to modify NetworkManager settings")))
