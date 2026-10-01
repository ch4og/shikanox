;;; SPDX-FileCopyrightText: 2025-2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika packages networking)
  #:use-module (guix packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) #:prefix nonlicense:)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public zapret
  (package
    (name "zapret")
    (version "72.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/bol-van/zapret")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06cxi4whypab7zsykmjcc010dd3ksw6xjkda8lk4wz7l8481y33z"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'create-cc-symlink
            (lambda _
              (let ((gcc (which "gcc")))
                (when gcc
                  (symlink gcc "cc")
                  (setenv "PATH"
                          (string-append (getcwd) ":" (getenv "PATH")))))))
          (delete 'configure)
          (delete 'check)
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (install-file "binaries/my/tpws" bin)
                (install-file "binaries/my/nfqws" bin)
                (install-file "binaries/my/ip2net" bin)
                (install-file "binaries/my/mdig" bin)
                (for-each (lambda (file)
                            (chmod file #o755))
                          (find-files bin))))))))
    (native-inputs (list pkg-config gcc-toolchain))
    (inputs (list openssl
                  zlib
                  libmnl
                  libnetfilter-queue
                  libnfnetlink
                  libcap))
    (home-page "https://github.com/bol-van/zapret")
    (synopsis "DPI bypass multi platform tool")
    (description
     "Autonomous countermeasure against DPI")
    (license license:expat)))

(define-public winbox
  (package
    (name "winbox")
    (version "4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.mikrotik.com/routeros/winbox/"
                           version "/WinBox_Linux.zip"))
       (file-name (string-append "WinBox_Linux-" version ".zip"))
       (sha256
        (base32 "1hfd6vbc383ng369hxnzadfq2rz9mpxpx552394lr02mmfmzmca9"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:patchelf-plan
      #~'(("WinBox" ("libc"
                     "dbus"
                     "fontconfig-minimal"
                     "freetype"
                     "libx11"
                     "libxcb"
                     "libxkbcommon"
                     "mesa"
                     "xcb-util-image"
                     "xcb-util-keysyms"
                     "xcb-util-renderutil"
                     "xcb-util-wm"
                     "zlib")))
      #:install-plan
      #~'(("WinBox" "bin/WinBox")
          ("assets/img/winbox.png"
           "share/icons/hicolor/1024x1024/apps/winbox.png"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "unzip" source)))
          (add-after 'install 'wrap-program
            (lambda _
              (let ((program (string-append #$output "/bin/WinBox")))
                (wrap-program program)
                (substitute* program
                  (("^exec ") "unset QT_QPA_PLATFORM\nexec ")))))
          (add-after 'install 'install-desktop-file
            (lambda _
              (make-desktop-entry-file
               (string-append #$output "/share/applications/winbox.desktop")
               #:name "WinBox"
               #:comment "GUI administration for Mikrotik RouterOS"
               #:exec (string-append #$output "/bin/WinBox")
               #:icon "winbox"
               #:startup-w-m-class "winbox"
               #:terminal #f
               #:categories '("Utility")))))))
    (native-inputs (list unzip))
    (inputs (list dbus
                  fontconfig
                  freetype
                  libx11
                  libxcb
                  libxkbcommon
                  mesa
                  xcb-util-image
                  xcb-util-keysyms
                  xcb-util-renderutil
                  xcb-util-wm
                  zlib))
    (supported-systems '("x86_64-linux"))
    (properties '((substitutable? . #f)))
    (home-page "https://mikrotik.com/download")
    (synopsis "Graphical configuration utility for RouterOS devices")
    (description
     "Advanced desktop utility to manage RouterOS limitless configuration
options.  With WinBox you can setup any of MikroTik products.")
    (license
     (nonlicense:undistributable "https://mikrotik.com/software/legal"))))
