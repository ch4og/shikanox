;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika lib channels)
  #:use-module (guix)
  #:use-module (guix channels)
  #:use-module (srfi srfi-1))

(define-public shika-chs
  (list (channel
          (name 'guix)
          (url "https://git.guix.gnu.org/guix.git")
          (branch "master")
          (commit "01bb0f2aa11918ce2b9187b9d53c1da62e40d775")
          (introduction
           (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
          (name 'nonguix)
          (url "https://gitlab.com/nonguix/nonguix.git")
          (branch "master")
          (commit "82be0b7adaaaa7a98d47382d7f72dd2e31d8e6d8")
          (introduction
           (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
	(channel
	 (name 'quickshell)
	 (url "https://github.com/ch4og/quickshell.git")
	 (branch "master")
	 (commit "95364813e0882ea87955eec8d67a6bb5b74341d4"))))

shika-chs
