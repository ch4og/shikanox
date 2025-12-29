;;; SPDX-FileCopyrightText: 2025 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (shika system kernel))

(define-public %shika-kernel-arguments
  '("quiet"
    "module_blacklist=pcspkr"
    "zswap.enabled=1"
    "zswap.max_pool_percent=60"))
