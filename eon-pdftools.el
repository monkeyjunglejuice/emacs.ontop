;;; eon-pdftools.el --- Sophisticated PDF reader  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-pdftools.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; PDF-TOOLS
;; <https://github.com/vedang/pdf-tools/>
;; Compiles binary automatically at the first run and after upgrades

;; Sophisticated PDF viewer
(use-package pdf-tools
  :magic
  ("%PDF" . pdf-view-mode)
  :config
  ;; Compile without asking
  (pdf-tools-install :no-query))

;;  ____________________________________________________________________________
(provide 'eon-pdftools)
;;; eon-pdftools.el ends here
