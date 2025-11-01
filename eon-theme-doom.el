;;; eon-theme-doom.el --- Doom Emacs thene pack -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Example how to set Doom themes in your init.el:
;; (with-eval-after-load 'eon-theme-doom
;;   (setopt eon-theme-light 'doom-one-light
;;           eon-theme-dark 'doom-one
;;           eon-theme-variant-default 'light)
;;   (eon-theme-load-default))
;;
;;; Code:

;; _____________________________________________________________________________
;;; DOOM THEMES
;; <https://github.com/doomemacs/themes>

(use-package doom-themes :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

;; _____________________________________________________________________________
(provide 'eon-theme-doom)
;;; eon-theme-doom.el ends here
