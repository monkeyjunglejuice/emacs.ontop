;;; eon-theme-ef.el --- Colorful and legible themes -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Example how to set Ef themes in your init.el:
;; (with-eval-after-load 'eon-theme-ef
;;   (setopt eon-theme-light 'ef-light
;;           eon-theme-dark 'ef-night
;;           eon-theme-variant-default 'light)
;;   (eon-theme-load-default))
;;
;;; Code:

;; _____________________________________________________________________________
;;; EF THEMES
;; <https://github.com/protesilaos/ef-themes>
;; <https://protesilaos.com/emacs/ef-themes>
;; <https://protesilaos.com/emacs/ef-themes-pictures>

(use-package ef-themes :ensure t)

;; _____________________________________________________________________________
(provide 'eon-theme-ef)
;;; eon-theme-ef.el ends here
