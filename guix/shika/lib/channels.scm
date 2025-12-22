;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika lib channels)
  #:use-module (guix)
  #:use-module (guix channels)
  #:use-module (srfi srfi-1))

(define-public %shika-chs
  (list (channel
         (name 'mangowc)
         (url "https://github.com/DreamMaoMao/mangowc")
         (branch "main")
         (commit
          "471c71f65c3c15ebe633edf4757361649757f990"))
        (channel
         (name 'quickshell)
         (url "https://github.com/ch4og/quickshell.git")
         (branch "master")
         (commit
          "95364813e0882ea87955eec8d67a6bb5b74341d4"))
        (channel
         (name 'aagl)
         (url "https://codeberg.org/ch4og/aagl-guix.git")
         (branch "main")
         (commit
          "53390838f459050202a2496bd9cd48503d63feb2")
         (introduction
          (make-channel-introduction
           "1055d880e124d69a2aef85cac98a813d442a55fa"
           (openpgp-fingerprint
            "7C9E 7EBA 828C 58DF DACE  5BED 4DCC 7AB7 FC75 319B"))))
        (channel
         (name 'guix)
         (url "https://git.guix.gnu.org/guix.git")
         (branch "master")
         (commit
          "a2df6c460f9777879a78a510fa5c3cc08ed0b4c1")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix.git")
         (branch "master")
         (commit
          "a6376bff79bfccc5e9519cf20954a89197884fac")
         (introduction
          (make-channel-introduction
           "0f68c1684169cbef8824fb246dfefa3e6832225b"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

%shika-chs
