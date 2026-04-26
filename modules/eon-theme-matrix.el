;;; eon-theme-matrix.el --- Theme inspired by The Matrix movie -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
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
;; Example how to set "The Matrix" theme in your init.el:
;; (with-eval-after-load 'eon-theme-matrix
;;   (setopt eon-theme-dark 'the-matrix
;;           eon-theme-variant-default 'dark)
;;   (eon-theme-load-default))
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; THE MATRIX THEME
;; <https://github.com/monkeyjunglejuice/matrix-emacs-theme>

(use-package the-matrix-theme :ensure t)

;; _____________________________________________________________________________
(provide 'eon-theme-matrix)
;;; eon-theme-matrix.el ends here
