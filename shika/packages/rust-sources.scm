;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages rust-sources)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (shika utils cargo)
  #:use-module (guix download))

(define-public rust-librespot-0.8.0.34ed484
  (let ((commit "34ed484cb54f1217d1347a271eb299bb63444b75")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-librespot")
       (version (git-version "0.8.0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/crmne/librespot")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "025yyn3yd9l1w26czscfkgg7vnasvjn9wxkgnjg38gh557d2asjs"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
             ;;; Order of crates DO matter!
              #:cargo-package-crates
              ''("librespot-oauth"
                 "librespot-protocol"
                 "librespot-core"
                 "librespot-audio"
                 "librespot-metadata"
                 "librespot-playback"
                 "librespot-connect")))
       (inputs
        (shika-cargo-inputs 'rust-crmne-librespot))
       (home-page "https://github.com/crmne/librespot/")
       (synopsis "Open Source Spotify client library")
       (description
        "librespot is an open source client library for Spotify. It enables
applications to use Spotify's service to control and play music via various
backends, and to act as a Spotify Connect receiver.")
       (license license:expat)))))

(define-public rust-projectm-sys-1.2.3.454f38c
  (let ((commit "454f38c50a968b13028ab6716d33647b3e99388c")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-projectm-sys")
       (version (git-version "1.2.3" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/crmne/projectm-rs")
                 (commit commit)
                 (recursive? #t)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "10w456g6ckpmkk3d7nk5za2hn89sxinknn37pvgzv33pqby3glvf"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates
              ''("projectm-sys")
              #:phases
              #~(modify-phases %standard-phases
                  (add-after 'unpack 'fix-cmake-libdir
                    (lambda _
                      (substitute* "projectm-sys/build.rs"
                        (("ENABLE_PLAYLIST\", enable_playlist_flag")
                         "ENABLE_PLAYLIST\", enable_playlist_flag)\n            .define(\"CMAKE_INSTALL_LIBDIR\", \"lib\""))))
                  (add-before 'package 'make-workspace
                    (lambda _
                      (let ((port (open-file "Cargo.toml" "a")))
                        (display "\n[workspace]\nmembers = [\"projectm-sys\"]\n"
                                 port)
                        (close-port port)))))))
       (inputs
        (shika-cargo-inputs 'rust-projectm-sys))
       (home-page "https://github.com/crmne/projectm-rs")
       (synopsis "Rust bindings for projectM")
       (description
        "Rust bindings for the projectM music visualizer library.")
       (license license:expat)))))
