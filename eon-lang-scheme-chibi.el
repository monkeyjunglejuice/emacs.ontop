;;; eon-lang-scheme-chibi.el --- Chibi Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/chibi>
(use-package geiser-chibi :ensure t)

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-chibi)
;;; eon-lang-scheme-chibi.el ends here
