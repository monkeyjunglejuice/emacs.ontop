;;; eon-helix.el --- Helix keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; HELIX
;; <https://github.com/mgmarlow/helix-mode>

(use-package helix :ensure t
  :diminish helix-normal-mode helix-insert-mode
  :config
  (helix-mode))

;; KEYS
;; You can add new keys to the Helix keymaps via helix-define-key:
;; Example:
;; (helix-define-key 'space "w" #'do-something-cool)
;; The first argument to helix-define-key is a Helix state.
;; The valid options are: insert, normal, space, view, goto, and window.

;; TYPABLE COMMANDS
;; You can create new typable commands (invoked via ":command-name")
;; with `helix-define-typable-command'.
;; Example:
;; (helix-define-typable-command "format" #'format-all-buffer)

;; Let `which-key' appear quickly
;; _____________________________________________________________________________
;;; WHICH-KEY
(use-package which-key :ensure nil
  :custom
  (which-key-idle-delay 0.25)
  (which-key-idle-secondary-delay 0.0))

;; _____________________________________________________________________________
(provide 'eon-helix)
;;; eon-helix.el ends here
