;;; eon-lang-scheme-gambit.el --- Gambit Scheme -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience languages scheme
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

;; Module with common definitions and functionality is required
(eon-load-module 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/gambit>
(use-package geiser-gambit :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'gambit))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-gambit)
;;; eon-lang-scheme-gambit.el ends here
