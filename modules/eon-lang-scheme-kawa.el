;;; eon-lang-scheme-kawa.el --- Kawa Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/kawa>
(use-package geiser-kawa :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'kawa))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-kawa)
;;; eon-lang-scheme-kawa.el ends here
