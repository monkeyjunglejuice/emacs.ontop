;;; eon-pdftools.el --- Sophisticated PDF tool suite -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
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
  :config
  ;; Compile without asking
  (pdf-tools-install :no-query))

;; _____________________________________________________________________________
(provide 'eon-pdftools)
;;; eon-pdftools.el ends here
