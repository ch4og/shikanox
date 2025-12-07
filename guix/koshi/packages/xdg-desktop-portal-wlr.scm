;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (koshi packages xdg-desktop-portal-wlr)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix gexp)
  #:use-module (guix utils))

(define-public xdg-desktop-portal-wlr-1
  (package
   (inherit xdg-desktop-portal-wlr)
   (name "xdg-desktop-portal-wlr")
   (version "0.8.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/emersion/xdg-desktop-portal-wlr")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0dm9zbg91h7rq3ks00brfmc3yhdlsfa4059np34xcabikdyp0nqn"))
            (patches (search-patches "xdg-desktop-portal-wlr-harcoded-length.patch"))))))

xdg-desktop-portal-wlr-1
