;;; eon-theme-matrix.el --- The Matrix theme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Example how to set "The Matrix" theme in your init.el:
;; (with-eval-after-load 'eon-theme-matrix
;;   (setopt eon-theme-dark 'the-matrix
;;           eon-theme-variant-default 'dark)
;;   (eon-theme-load-default))
;;
;;; Code:

;;  ____________________________________________________________________________
;;; THE MATRIX THEME
;; <https://github.com/monkeyjunglejuice/matrix-emacs-theme>

(use-package the-matrix-theme :ensure t)

;;  ____________________________________________________________________________
(provide 'eon-theme-matrix)
;;; eon-theme-matrix.el ends here
