;;; eon-pdftools.el --- Sophisticated PDF tool suite -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.1
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience tools
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
 :requires  '(eon))

;; _____________________________________________________________________________
;;; PDF-TOOLS
;; <https://github.com/vedang/pdf-tools/>
;; Compiles binary automatically at the first run and after upgrades

(use-package pdf-tools :ensure t
  :magic
  ("%PDF" . pdf-view-mode)
  ;; Compile without asking
  :hook
  ;; Ensure build if necessary
  (after-init . pdf-tools-install)
  ;; No jumping between pages
  (pdf-view-mode . pdf-view-roll-minor-mode))

;; _____________________________________________________________________________
(provide 'eon-pdftools)
;;; eon-pdftools.el ends here
