;;; eon-lang-scheme-chicken.el --- Chicken Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/chicken>
(use-package geiser-chicken :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'chicken))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-chicken)
;;; eon-lang-scheme-chicken.el ends here
