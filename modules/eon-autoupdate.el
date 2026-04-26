;;; eon-autoupdate.el --- Update packages automatically -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;; This module works with Emacs' built-in package manager. It must not be used
;; with other package managers such as Straight or Elpaca. Therefore the
;; packages therein will be installed/activated only when
;; `package-enable-at-startup' is non-nil, no matter if the module is enabled or
;; not.
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; AUTO-UPDATE PACKAGES
;; <https://github.com/rranelli/auto-package-update.el>

(when package-enable-at-startup
  (use-package auto-package-update :ensure t

    :init

    (auto-package-update-maybe)

    :custom

    (auto-package-update-interval 7)
    (auto-package-update-show-preview nil)
    (auto-package-update-hide-results t)
    (auto-package-update-prompt-before-update t)
    ;; Keep old packages around to roll back if necessary
    (auto-package-update-delete-old-versions nil)
    (auto-package-update-excluded-packages '())

    :bind

    (:map ctl-z-x-map
          ("P" . auto-package-update-now))))

;; _____________________________________________________________________________
(provide 'eon-autoupdate)
;;; eon-autoupdate.el ends here
