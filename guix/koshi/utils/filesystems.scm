;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og. com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (koshi utils filesystems)
  #:use-module (gnu)
  #:export (luks-btrfs))

(define (mapper-name device)
  (and (string? device)
       (string-prefix? "/dev/mapper/" device)
       (substring device
                  (string-length "/dev/mapper/"))))

(define (mapped-device-deps device devices)
  (let ((name (mapper-name device)))
    (if name
        (filter (lambda (md)
                  (member name
                          (mapped-device-targets md))) devices)
        '())))

(define (check-deps-for-boot deps)
  (let loop
    ((lst deps))
    (cond
      ((null? lst)
       #f)
      ((member "root"
               (mapped-device-targets (car lst)))
       #t)
      (else (loop (cdr lst))))))

(define (mapped target)
  (string-append "/dev/mapper/" target))

(define (build-compress-option compress)
  (cond
    ((list? compress)
     (if (null? compress) "compress=zstd"
         (let* ((value (car compress))
                (force? (and (not (null? (cdr compress)))
                             (eq? (cadr compress)
                                  'force))))
           (cond
             ((not value)
              "compress=no")
             ((eq? value #t)
              (if force? "compress-force=zstd" "compress=zstd"))
             ((string? value)
              (if force?
                  (string-append "compress-force=" value)
                  (string-append "compress=" value)))
             (else (error "compress value must be #f, #t, or a string"))))))
    ((not compress)
     "compress=no")
    ((eq? compress #t)
     "compress=zstd")
    ((string? compress)
     (string-append "compress=" compress))
    (else (error "compress must be #f, #t, a string, or a list"))))

(define* (luks-btrfs #:key device
                     mount-point
                     (flags '())
                     (options #f)
                     (subvol #f)
                     (compress #f)
                     (mapped-devices '()))
  "Create a file-system for LUKS-encrypted Btrfs with automatic dependencies.

DEVICE:  mapped device target (e.g., \"root\")
MOUNT-POINT: where to mount
FLAGS: mount flags (default: '())
OPTIONS: mount options string, overrides SUBVOL and COMPRESS (default: #f)
SUBVOL: btrfs subvolume name (default: #f)
COMPRESS: #f (no), #t (zstd), \"algo\", or '(value force) (default: #f)
MAPPED-DEVICES: list of mapped-device records for dependencies (default: '())"

  (unless device
    (error "luks-btrfs:  device is required"))
  (unless mount-point
    (error "luks-btrfs: mount-point is required"))

  (let* ((mapped-dev (mapped device))
         (deps (if (null? mapped-devices)
                   '()
                   (mapped-device-deps mapped-dev mapped-devices)))
         (mount-options (or options
                            (string-join (filter (lambda (s)
                                                   (not (string-null? s)))
                                                 (list (build-compress-option
                                                        compress)
                                                       (if subvol
                                                           (string-append
                                                            "subvol=" subvol)
                                                           ""))) ","))))
    (file-system
      (device mapped-dev)
      (mount-point mount-point)
      (type "btrfs")
      (dependencies deps)
      (needed-for-boot? (check-deps-for-boot deps))
      (flags flags)
      (options mount-options))))
