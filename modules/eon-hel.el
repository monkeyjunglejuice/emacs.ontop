;;; eon-hel.el --- Modal editing: Helix keybindings -*- lexical-binding: t; no-byte-compile: t; -*-

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
;;; NOTE Hel-mode for Emacs is new and under heavy development right now,
;;       it may happen that this config breaks from time to time.
;;
;;; Code:

(eon-module-metadata
 :conflicts '(eon-evil eon-god eon-meow eon-helix)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; HEL
;; <https://github.com/helheim-emacs/hel>

(use-package hel :ensure t

  :init

  ;; Let `hel-mode' handle the cursor indicator
  (eon-cursor-mode -1)

  :custom

  (hel-emacs-state-cursor-type 'bar)
  (hel-insert-state-cursor-type 'bar)
  (hel-normal-state-cursor-type 'box)

  :config

  (hel-mode))

;; _____________________________________________________________________________
(provide 'eon-hel)
;;; eon-hel.el ends here
