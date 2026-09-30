;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages protonplus)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:))

(define-public protonplus
  (package
    (name "protonplus")
    (version "0.6.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Vysp3r/protonplus")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mqigm1vlm757pwn0vv0jdzsqz2sf63b6aa28iymx94q7m8fz3av"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
	  (add-after 'unpack 'patch-test-executable-paths
	    (lambda _
	      (substitute* (find-files "tests" "\\.(py|vala)$")
		(("/bin/sh") (which "sh")))
	      (substitute* "tests/steam-restart-orchestrator-test.vala"
		(("/bin/sleep") (which "sleep")))
              (invoke "python3" "-c"
                      "import base64, io, pathlib, sys, zipfile
path = pathlib.Path('tests/fixtures/archives/steamtinkerlaunch.zip.base64')
output = io.BytesIO()
with zipfile.ZipFile(io.BytesIO(base64.b64decode(path.read_bytes()))) as source:
    with zipfile.ZipFile(output, 'w') as target:
        for entry in source.infolist():
            data = source.read(entry)
            target.writestr(entry, data.replace(b'#!/bin/sh', b'#!' + sys.argv[1].encode()))
path.write_bytes(base64.b64encode(output.getvalue()) + b'\\n')
"
                      (which "sh")))))))
    (native-inputs (list gettext-minimal
                         `(,glib "bin")
                         pkg-config
                         python
                         vala))
    (inputs (list desktop-file-utils
                  gsettings-desktop-schemas
                  `(,gtk "bin")
                  json-glib
                  libadwaita
                  libarchive
                  libgee
                  libnotify
                  libsoup
                  sdl3))
    (home-page "https://github.com/Vysp3r/protonplus")
    (synopsis "Simple Wine and Proton-based compatibility tools manager.")
    (description
     "ProtonPlus is a Proton version manager for installing and managing Proton versions.
It works with Steam, Lutris, Heroic Games Launcher and Bottles. It uses GTK4.")
    (license license:gpl3)))

(define-public protonplus-sandbox
  (package
    (inherit protonplus)
    (name "protonplus-sandbox")
    (arguments
     (substitute-keyword-arguments (package-arguments protonplus)
       ((#:phases phases #~%standard-phases)
	#~(modify-phases #$phases
	    (add-after 'install 'set-home
	      (lambda _
		(let* ((bin (string-append #$output "/bin/protonplus"))
		       (new-home "$HOME/.local/share/guix-sandbox-home/")
		       (prefix (string-append "${GUIX_SANDBOX_HOME:-" new-home "}")))
		  (wrap-program bin
		    `("HOME" = (,prefix))
		    `("XDG_DATA_HOME" = (,(string-append prefix "/.local/share")))
		    `("XDG_CONFIG_HOME" = (,(string-append prefix "/.config")))))))))))
    (synopsis "Simple Wine and Proton-based compatibility tools manager.
Patched for nonguix container path.")))

protonplus
