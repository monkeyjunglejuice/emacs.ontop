;;; eon-theme-catppuccin.el --- Soothing pastel themes -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience themes
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Catppuccin comes in 4 flavors:
;; 'latte (light)
;; 'frappe (medium dark)
;; 'macchiato (dark)
;; 'mocha (very dark)
;;
;; This theme is quite different from other Emacs themes - it must be set via
;; functions. `eon-theme-load-light' and `eon-theme-load-dark' support themes as
;; functions.
;;
;; Example how to set the theme flavors in your init.el:
;; (with-eval-after-load 'eon-theme-catppuccin
;;   (setopt eon-theme-light (lambda () (catppuccin-load-flavor 'latte))
;;           eon-theme-dark (lambda () (catppuccin-load-flavor 'frappe))
;;           eon-theme-variant-default 'light)
;;   (eon-theme-load-default)
;;   (add-hook 'server-after-make-frame-hook #'catppuccin-reload))
;;
;;; Code:

;; _____________________________________________________________________________
;;; CATPPUCCIN THEMES
;; <https://github.com/catppuccin/emacs>

(use-package catppuccin-theme :ensure t)

;; _____________________________________________________________________________
(provide 'eon-theme-catppuccin)
;;; eon-theme-catppuccin.el ends here
