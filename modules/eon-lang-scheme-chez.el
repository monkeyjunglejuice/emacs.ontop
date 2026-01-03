;;; eon-lang-scheme-chez.el --- Chez Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/chez>
(use-package geiser-chez :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'chez))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-chez)
;;; eon-lang-scheme-chez.el ends here
