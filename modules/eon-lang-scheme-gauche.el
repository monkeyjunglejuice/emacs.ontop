;;; eon-lang-scheme-gauche.el --- Gauche Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/gauche>
(use-package geiser-gauche :ensure t
  :config
  (add-to-list 'geiser-active-implementations 'gauche))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-gauche)
;;; eon-lang-scheme-gauche.el ends here
