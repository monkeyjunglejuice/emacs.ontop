;;; eon-theme-spacemacs.el --- Spacemacs themes -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
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
;; Example how to set the Spacemacs theme in your init.el:
;; (with-eval-after-load 'eon-theme-spacemacs
;;   (setopt eon-theme-light 'spacemacs-light
;;           eon-theme-dark 'spacemacs-dark
;;           eon-theme-variant-default 'light)
;;   (eon-theme-load-default))
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; SPACEMACS THEME
;; <https://github.com/nashamri/spacemacs-theme>

(use-package spacemacs-theme :ensure t
  :custom
  (spacemacs-theme-underline-parens t)
  (spacemacs-theme-org-height nil)
  (spacemacs-theme-org-highlight t))

;; _____________________________________________________________________________
(provide 'eon-theme-spacemacs)
;;; eon-theme-spacemacs.el ends here
