;;; eon-paren-face.el --- Less visible parenthesis -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.1
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
;; Make parens stand out less
;; Website: <https://github.com/tarsius/paren-face>
;;
;;; Code:

(eon-module-metadata
 :conflicts '(eon-rainbow-delimiters)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; PAREN FACE MODE

(use-package paren-face :ensure t

  :config

  ;; Recognize more Lisp-style language modes
  (eon-add-to-list* 'paren-face-modes (eon-lisp-src-modes))
  (eon-add-to-list* 'paren-face-modes (eon-lisp-repl-modes))

  ;; Enable Paren Face Mode for all `paren-face-modes`
  (global-paren-face-mode 1))

;; _____________________________________________________________________________
(provide 'eon-paren-face)
;;; eon-paren-face.el ends here
