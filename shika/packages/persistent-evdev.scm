;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages persistent-evdev)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module ((guix licenses) #:prefix license:))

(define-public persistent-evdev
  (let* ((commit "52bf246464e09ef4e6f2e1877feccc7b9feba164")
         (version (git-version "0.0.0" "0" commit)))
    (package
      (name "persistent-evdev")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aiberia/persistent-evdev")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xj15kbxvh3w5xqz92i963sxvb2nf9lw67nyg85k307apw6blj3p"))))
      (build-system copy-build-system)
      (properties '((shika-update . #f)))
      (arguments
       (list
        #:install-plan
        #~'(("bin/persistent-evdev.py" "bin/persistent-evdev")
            ("udev/60-persistent-input-uinput.rules" "lib/udev/rules.d/"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'wrap-program
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((python-version
                        #$(version-major+minor (package-version python)))
                       (pythonpath
                        (string-join
                         (map (lambda (input)
                                (string-append input "/lib/python"
                                               python-version "/site-packages"))
                              (list (assoc-ref inputs "python-evdev")
                                    (assoc-ref inputs "python-pyudev")))
                         ":")))
                  (wrap-program
                   (string-append #$output "/bin/persistent-evdev")
                   `("GUIX_PYTHONPATH" ":" prefix (,pythonpath)))))))))
      (inputs (list python-wrapper
                    python-evdev
                    python-pyudev))
      (home-page "https://github.com/aiberia/persistent-evdev")
      (synopsis "Persistent virtual input devices for evdev hotplug support")
      (description
       "Persistent-evdev creates persistent virtual input devices for QEMU and
Libvirt by proxying events from physical evdev devices across hotplug events.")
      (license license:expat))))

persistent-evdev
