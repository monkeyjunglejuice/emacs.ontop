;;; eon-lang-scheme-guile.el --- Guile Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/guile>
(use-package geiser-guile :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'guile)
  :custom
  (geiser-guile-binary "guile"))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-guile)
;;; eon-lang-scheme-guile.el ends here
