;;; eon-pdftools.el --- Sophisticated PDF tool suite -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

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
