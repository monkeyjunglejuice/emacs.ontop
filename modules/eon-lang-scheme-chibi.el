;;; eon-lang-scheme-chibi.el --- Chibi Scheme -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
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

;; <https://gitlab.com/emacs-geiser/chibi>
(use-package geiser-chibi :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'chibi))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-chibi)
;;; eon-lang-scheme-chibi.el ends here
