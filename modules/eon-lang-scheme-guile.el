;;; eon-lang-scheme-guile.el --- Guile Scheme -*- lexical-binding: t; no-byte-compile: t; -*-

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

(eon-module-metadata
 :conflicts '()
 :requires  '(eon eon-lang-scheme))

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

;; <https://gitlab.com/emacs-geiser/guile>
(use-package geiser-guile :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'guile)
  :custom
  (geiser-guile-binary "guile"))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-guile)
;;; eon-lang-scheme-guile.el ends here
