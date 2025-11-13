;;; eon-theme-spacemacs.el --- Spacemacs themes -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Example how to set the Spacemacs theme in your init.el:
;; (with-eval-after-load 'eon-theme-spacemacs
;;   (setopt eon-theme-light 'spacemacs-light
;;           eon-theme-dark 'spacemacs-dark
;;           eon-theme-variant-default 'light)
;;   (eon-theme-load-default))
;;
;;; Code:

;; _____________________________________________________________________________
;;; SPACEMACS THEME
;; <https://github.com/nashamri/spacemacs-theme>

(use-package spacemacs-theme :ensure t
  :custom
  (spacemacs-theme-underline-parens t)
  (spacemacs-theme-org-height nil)
  (spacemacs-theme-org-highlight t))

;; _____________________________________________________________________________
(provide 'eon-theme-spacemacs)
;;; eon-theme-spacemacs.el ends here
