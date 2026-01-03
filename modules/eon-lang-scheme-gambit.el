;;; eon-lang-scheme-gambit.el --- Gambit Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/gambit>
(use-package geiser-gambit :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'gambit))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-gambit)
;;; eon-lang-scheme-gambit.el ends here
