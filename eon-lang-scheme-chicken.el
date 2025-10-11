;;; eon-lang-scheme-chicken.el --- Chicken Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/chicken>
(use-package geiser-chicken :ensure t)

;;  ____________________________________________________________________________
(provide 'eon-lang-scheme-chicken)
;;; eon-lang-scheme-chicken.el ends here
