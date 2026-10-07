;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages graphics)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (shika utils cargo)
  #:use-module ((guix licenses) #:prefix license:))

(define-public photocraft
  (package
    (name "photocraft")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/storytold/photocraft")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16sc1b2k5688vs9ac2amxvfzanw3g8vwan0xf0vwaw99xq3h6g73"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      ;; https://github.com/storytold/photocraft/issues/392
      #:cargo-build-flags ''("--release"
                             "-p" "photocraft"
                             "-p" "photocraft-cli")
      #:cargo-test-flags ''("--workspace")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (share (string-append #$output "/share"))
                    (release "target/release/")
                    (metadata "packaging/linux/ai.storyteller.photocraft"))
                (install-file (string-append release "photocraft") bin)
                (install-file (string-append release "photocraft-cli") bin)
                (install-file (string-append metadata ".desktop")
                              (string-append share "/applications"))
                (install-file (string-append metadata ".mime.xml")
                              (string-append share "/mime/packages"))
                (copy-recursively "assets/app-icon/hicolor"
                                  (string-append share "/icons/hicolor")))))
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/photocraft")
                `("LD_LIBRARY_PATH" ":" prefix
                  #$(map (lambda (name)
                           (file-append (this-package-input name) "/lib"))
                         '("libxkbcommon"
                           "wayland"
                           "mesa"
                           "vulkan-loader")))
                `("PATH" ":" prefix
                  (#$(file-append
                      (this-package-input "xdg-utils") "/bin")))))))))
    (native-inputs (list pkg-config))
    (inputs
     (cons* bash-minimal
            libx11
            libxcursor
            libxi
            libxrandr
            libxkbcommon
            mesa
            vulkan-loader
            wayland
            xdg-utils
            (shika-cargo-inputs 'photocraft)))
    (home-page "https://github.com/storytold/photocraft")
    (synopsis "Native image editor with layered Photoshop document support")
    (description
     "An open-source implementation of Adobe Photoshop written in Rust")
    (license
     (list license:expat
           license:asl2.0
           license:silofl1.1
           license:isc
           license:cc0
           (license:non-copyleft
            "file://assets/dict/LICENSE-SCOWL.txt"
            "SCOWL combines public-domain word lists with permissively licensed
material.  See LICENSE-SCOWL.txt for the applicable notices and terms.")))))
