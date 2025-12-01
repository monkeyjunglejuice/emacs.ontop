;;; eon-lang-scheme-racket.el --- Racket Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/racket>
(use-package geiser-racket :ensure t
  :config
  (add-to-list 'geiser-active-implementations 'racket))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-racket)
;;; eon-lang-scheme-racket.el ends here
