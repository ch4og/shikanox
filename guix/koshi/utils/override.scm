;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (koshi utils override)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:export (koshi-git-override))

(define* (koshi-git-override
          base
          #:key
          name
          version
          commit
          url
          hash
          home-page)
  (package
   (inherit base)
   (version (git-version version "0" commit))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (commit commit)
           (url (or url
                    (git-reference-url
                     (origin-uri
                      (package-source base)))))))
     (file-name (git-file-name (or name (package-name base))
                               version))
     (sha256 (base32 hash))))
   (home-page (or home-page (package-home-page base)))))
