;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika system filesystems)
  #:use-module (gnu))

(define-public %shika-mapped-devices
  (list (mapped-device
         (source (uuid "a2c0fca8-06a8-4eae-9bbc-55b75ca47c69"))
         (target "root")
         (type luks-device-mapping))
        (mapped-device
         (source (uuid "66750ef3-48ad-42c7-82a6-ec8ca931c5c3"))
         (target "home")
         (type luks-device-mapping))))

(define-public %shika-file-systems
  (append (list (file-system
                 (device "/dev/mapper/root")
                 (mount-point "/")
                 (type "btrfs")
                 (dependencies %shika-mapped-devices)
                 (needed-for-boot? #t)
                 (flags '(no-atime))
                 (options "compress-force=zstd,subvol=root"))

                (file-system
                 (device "/dev/mapper/root")
                 (mount-point "/gnu/store")
                 (type "btrfs")
                 (dependencies %shika-mapped-devices)
                 (needed-for-boot? #t)
                 (flags '(no-atime))
                 (options "compress-force=zstd,subvol=gnu-store"))

                (file-system
                 (device "/dev/mapper/root")
                 (mount-point "/nix")
                 (type "btrfs")
                 (needed-for-boot? #t)
                 (dependencies %shika-mapped-devices)
                 (flags '(no-atime))
                 (options "compress-force=zstd,subvol=nix"))

                (file-system
                 (device "/dev/mapper/root")
                 (mount-point "/var/log")
                 (type "btrfs")
                 (dependencies %shika-mapped-devices)
                 (needed-for-boot? #t)
                 (options "compress=no,subvol=log"))

                (file-system
                 (device "/dev/mapper/home")
                 (mount-point "/home")
                 (type "btrfs")
                 (dependencies %shika-mapped-devices)
                 (needed-for-boot? #t)
                 (options "compress=zstd,subvol=home"))

                (file-system
                 (device "/dev/mapper/home")
                 (mount-point "/games")
                 (type "btrfs")
                 (needed-for-boot? #t)
                 (dependencies %shika-mapped-devices)
                 (options "compress=zstd,subvol=games"))

                (file-system
                 (device "/dev/mapper/home")
                 (mount-point "/swap")
                 (type "btrfs")
                 (needed-for-boot? #t)
                 (flags '(no-atime))
                 (dependencies %shika-mapped-devices)
                 (options "compress=no,subvol=swap"))

                (file-system
                 (device (uuid "350C-92A0" 'fat))
                 (mount-point "/efi")
                 (type "vfat")))
          %base-file-systems))

(define-public %shika-swap-devices
  (list
   (swap-space
    (target "/swap/swapfile"))))
