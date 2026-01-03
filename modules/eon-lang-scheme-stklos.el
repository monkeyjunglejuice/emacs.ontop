;;; eon-lang-scheme-stklos.el --- Stklos Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/stklos>
(use-package geiser-stklos :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'stklos))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-stklos)
;;; eon-lang-scheme-stklos.el ends here
