;;; eon-lang-scheme-chez.el --- Chez Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/chez>
(use-package geiser-chez :ensure t)

;;  ____________________________________________________________________________
(provide 'eon-lang-scheme-chez)
;;; eon-lang-scheme-chez.el ends here
