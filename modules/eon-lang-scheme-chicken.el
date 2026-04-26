;;; eon-lang-scheme-chicken.el --- Chicken Scheme -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience languages scheme
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon eon-lang-scheme))

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

;; <https://gitlab.com/emacs-geiser/chicken>
(use-package geiser-chicken :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'chicken))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-chicken)
;;; eon-lang-scheme-chicken.el ends here
