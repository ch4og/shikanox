;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages browsers)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages librewolf))

(define-public librewolf-sidebar-chatbot
  (package
    (name "librewolf-sidebar-chatbot")
    (version (package-version librewolf))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((input #$(this-package-input "librewolf"))
                 (sed (string-append #$(this-package-native-input "sed")
                                     "/bin/sed"))
                 (browser (string-append #$output "/lib/librewolf"))
                 (launcher (string-append browser "/librewolf"))
                 (policy (string-append browser "/distribution/policies.json"))
                 (desktop (string-append #$output
                                         "/share/applications/librewolf.desktop")))
            (copy-recursively input #$output
                              #:copy-file
                              (lambda (source destination)
                                (if (member (basename source)
                                            '("librewolf"
                                              ".librewolf-real"
                                              "policies.json"
                                              "librewolf.desktop"))
                                    (copy-file source destination)
                                    (symlink source destination))))
            (substitute* (list launcher desktop)
              ((input) #$output))
            (invoke sed "-i" "/SidebarChatbot/,/Value/s/blocked/available/" policy)
            (delete-file (string-append #$output "/bin/librewolf"))
            (symlink launcher (string-append #$output "/bin/librewolf"))))))
    (inputs (list librewolf))
    (native-inputs (list sed))
    (native-search-paths (package-native-search-paths librewolf))
    (home-page (package-home-page librewolf))
    (synopsis
     (string-append (package-synopsis librewolf)
                    " with the Sidebar Chatbot enabled"))
    (description
     (string-append (package-description librewolf)
                    "  This variant enables Sidebar Chatbot through distribution
policy."))
    (license (package-license librewolf))))
