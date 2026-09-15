;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages spotify)
  #:use-module (guix packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (shika build-system interpreter-binary)
  #:use-module (shika utils cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix licenses) #:prefix nonlicense:))

(define-public spotifast
  (package
    (name "spotifast")
    (version "0.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/crmne/spotifast")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "07asmr157l9w96z5swsrfs87vwnn02pyy3mbjx961vifdrf46zvi"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:rust rust-1.95
      ;; rust-1.95 has no rustdoc; this version has no doctests.
      #:cargo-test-flags ''("--release" "--all-targets")
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-patch-section
            (lambda _
              (substitute* "Cargo.toml"
                (("\\[patch\\.crates-io\\]" _)
                 "")
                (("^(projectm-sys|librespot-[a-z-]+) = .*git = .*" _)
                 ""))))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (install-file "target/release/spotifast" bin)
                (symlink "spotifast" (string-append bin "/fastpotify")))))
          (add-after 'install 'wrap-wayland-libraries
            (lambda _
              (wrap-program (string-append #$output "/bin/spotifast")
                `("LD_LIBRARY_PATH" ":" prefix
                  (,(string-append #$(this-package-input "wayland") "/lib")
                   ,(string-append #$(this-package-input "libxkbcommon") "/lib"))))))
          (add-after 'wrap-wayland-libraries 'install-desktop-files
            (lambda _
              (let ((desktop-directory (string-append #$output "/share/applications"))
                    (icon-directory
                     (string-append #$output "/share/icons/hicolor/scalable/apps")))
                (mkdir-p desktop-directory)
                (mkdir-p icon-directory)
                (install-file "packaging/applications/fastpotify.desktop"
                              desktop-directory)
                (install-file "packaging/icons/fastpotify.svg"
                              icon-directory)))))))
    (native-inputs
      (list clang
            cmake-minimal
            dbus
            pkg-config))
    (inputs
     (cons* alsa-lib
            libx11
            libxkbcommon
            mesa
            pulseaudio
            wayland
            (shika-cargo-inputs 'spotifast)))
    (home-page "https://spotifast.rocks")
    (synopsis "Spotify client written in Rust with egui")
    (description
     "A lightweight Spotify client with local playback, library access,
and Spotify Connect controls for Linux, macOS, and Windows.")
    (license license:expat)))

(define-public spicetify-cli
  (package
    (name "spicetify-cli")
    (version "2.43.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/spicetify/cli")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "166zg65fk3wa7w11wn7w34ipxckhnsx6gv8xwl44cvkj7da9kczg"))))
    (build-system go-build-system)
    (inputs
     (list go-github-com-go-ini-ini
           go-github-com-mattn-go-colorable
           go-github-com-pterm-pterm
           go-golang-org-x-net
           go-golang-org-x-sys))
    (arguments
     `(#:import-path "github.com/spicetify/cli"
       #:unpack-path "github.com/spicetify/cli"
       #:go ,go-1.26
       #:tests? #f
       #:install-source? #f
       #:build-flags
       (list ,(string-append "-ldflags=-s -w -X main.version=" version))
       #:phases (modify-phases %standard-phases
                  ;; Treat the release build as a development build so the
                  ;; bundled CSS map is used instead of fetching one at runtime.
                  (add-after 'unpack 'patch-version
                    (lambda _
                      (let ((source-dir "src/github.com/spicetify/cli"))
                        (substitute* (string-append source-dir
                                                    "/src/preprocess/preprocess.go")
                          (("version != \\\"Dev\\\"")
                           ,(string-append "version != \"" version "\""))))))
                  (add-after 'install 'install-assets
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (old (string-append out "/bin/cli"))
                             (directory (string-append out "/share/spicetify"))
                             (binary (string-append directory "/spicetify"))
                             (source-dir "src/github.com/spicetify/cli"))
                        (mkdir-p directory)
                        (rename-file old binary)
                        (copy-recursively (string-append source-dir "/jsHelper")
                                          (string-append directory "/jsHelper"))
                        (install-file (string-append source-dir "/css-map.json")
                                      directory)
                        (symlink "../share/spicetify/spicetify"
                                 (string-append out "/bin/spicetify"))))))))
    (home-page "https://github.com/spicetify/cli")
    (synopsis "Command-line tool to customize Spotify client")
    (description
     "Spicetify is a command-line tool to customize the Spotify client.")
    (license license:lgpl2.1+)))
