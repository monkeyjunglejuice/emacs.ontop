;;; eon-theme-doom.el --- Doom Emacs thene pack -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.1
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
;; Example how to set Doom themes in your init.el:
;;
;; (setopt eon-theme-light 'doom-one-light
;;         eon-theme-dark 'doom-one
;;         eon-theme-variant-default 'light)
;; 
;; (eon-theme-load-default)
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; DOOM THEMES
;; <https://github.com/doomemacs/themes>

(use-package doom-themes :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

;; _____________________________________________________________________________
(provide 'eon-theme-doom)
;;; eon-theme-doom.el ends here
