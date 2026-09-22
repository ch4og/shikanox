;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages virtualization)
  #:use-module (gnu packages virtualization)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define qemu-patch
  (origin
    (method url-fetch)
    (uri "https://raw.githubusercontent.com/zhaodice/qemu-anti-detection/7f87d47416ff083e2951467020f4aa38fc9f2130/qemu-10.2.2.patch")
    (sha256
     (base32
      "03hiv84lh5ayvi25a21h9xln8pcm3kcbf0rjwczyy76rrshg218d"))))

(define-public qemu-anti-detection
  (package/inherit qemu
    (name "qemu-anti-detection")
    (arguments
     (substitute-keyword-arguments (package-arguments qemu)
       ((#:tests? tests? #t) #f)))
    (source
     (origin
       (inherit (package-source qemu))
       (patches
        (cons qemu-patch
              (origin-patches (package-source qemu))))))))
