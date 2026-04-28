;;; eon-theme-batppuccin.el --- Soothing pastel themes -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.2
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience themes
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;; Batppuccin is a rewrite of the Catppuccin theme for Emacs. It aims to follow
;; the official Catppuccin style closely while being structured idiomatically
;; for Emacs.
;;
;; Batppuccin comes in 4 flavors:
;; 'batppuccin-latte     (light)
;; 'batppuccin-frappe    (medium dark)
;; 'batppuccin-macchiato (dark)
;; 'batppuccin-mocha     (very dark)
;;
;; Example how to set the theme flavors in your init.el:
;;
;; (setopt eon-theme-light 'batppuccin-latte
;;         eon-theme-dark 'batppuccin-macchiato
;;         eon-theme-variant-default 'light)
;; 
;; (eon-theme-load-default)
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; BATPPUCCIN THEMES
;; <https://github.com/bbatsov/batppuccin-emacs>

(use-package batppuccin :ensure t
  :custom
  (batppuccin-scale-headings nil))

;; _____________________________________________________________________________
(provide 'eon-theme-batppuccin)
;;; eon-theme-batppuccin.el ends here
