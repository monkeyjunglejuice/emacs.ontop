;;; ontop-pdftools.el --- Sophisticated PDF reader  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-pdftools.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;; <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package nil))

(eval-when-compile
  (require 'use-package))

;;  ____________________________________________________________________________
;;; PDF-TOOLS
;; <https://github.com/vedang/pdf-tools/>
;; Compiles binary automatically at the first run and after upgrades

;; Sophisticated PDF viewer
(use-package pdf-tools
  :ensure t
  :magic
  ("%PDF" . pdf-view-mode)
  :config
  ;; Compile without asking
  (pdf-tools-install :no-query))

;;  ____________________________________________________________________________
(provide 'ontop-pdftools)
;;; ontop-pdftools.el ends here
