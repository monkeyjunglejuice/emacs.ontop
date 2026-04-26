;;; eon-yasnippet.el --- Code snippets -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; YASNIPPET
;; <https://github.com/joaotavora/yasnippet>

(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;; SNIPPETS COLLECTION
;; <https://github.com/AndreaCrotti/yasnippet-snippets>
;; <https://andreacrotti.pro/yasnippet-snippets/snippets>

(use-package yasnippet-snippets :ensure t
  :after yasnippet)

;; _____________________________________________________________________________
(provide 'eon-yasnippet)
;;; eon-yasnippet.el ends here
