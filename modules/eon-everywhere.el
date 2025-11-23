;;; eon-everywhere.el --- Use Emacs for text input in other apps -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Emacs-everywhere needs a way to launch when a not-emacs application has focus.
;; The easiest way to do this is to have a global keybinding/keyboard shortcut
;; dedicated to launching:
;;
;; emacsclient --eval "(emacs-everywhere)"
;;
;;; Code:

;; _____________________________________________________________________________
;;; EMACS EVERYWHERE
;; <https://github.com/tecosaur/emacs-everywhere>

(use-package emacs-everywhere :ensure t
  :custom
  (emacs-everywhere-major-mode-function #'text-mode))

;; _____________________________________________________________________________
(provide 'eon-everywhere)
;;; eon-everywhere.el ends here
