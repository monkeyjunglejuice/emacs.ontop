;;; eon-lang-scheme-mit.el --- MIT Scheme -*- lexical-binding: t; no-byte-compile: t; -*-

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

;; <https://gitlab.com/emacs-geiser/mit>
(use-package geiser-mit :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'mit))

;; Built-in Emacs Scheme package
(use-package scheme :ensure nil
  :custom
  ;; MIT Scheme specific indentation
  (scheme-mit-dialect t))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-mit)
;;; eon-lang-scheme-mit.el ends here
