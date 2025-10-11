;;; eon-lang-scheme-gambit.el --- Gambit Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/gambit>
(use-package geiser-gambit :ensure t)

;;  ____________________________________________________________________________
(provide 'eon-lang-scheme-gambit)
;;; eon-lang-scheme-gambit.el ends here
