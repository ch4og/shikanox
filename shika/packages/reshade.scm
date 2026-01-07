;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages reshade)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public reshade
  (package
    (name "reshade")
    (version "6.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/crosire/reshade")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09vwc3cfkxbifyhihy10b9xy0jspwrf8y3ghnv6vrh4cfrvjji0f"))))
    (build-system copy-build-system)
    (home-page "https://github.com/crosire/reshade")
    (synopsis "A generic post-processing injector for games and video software.")
    (description
     "This is a generic post-processing injector for games
and video software. It exposes an automated way to access
both frame color and depth information and a custom shader
language called ReShade FX to write effects like ambient
occlusion, depth of field, color correction and more which
work everywhere.")
    (license license:bsd-3)))

reshade
