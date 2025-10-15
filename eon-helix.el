;;; eon-helix.el --- Helix keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; HELIX
;; <https://github.com/mgmarlow/helix-mode>

(use-package helix
  :vc (:url "https://github.com/mgmarlow/helix-mode")
  :diminish helix-normal-mode helix-insert-mode

  :init
  (defun eon-helix--bind-leader-in-states (old new)
    "Explicitly bind the leader key to prevent hijacking."
    (when (and (stringp new) (> (length new) 0))
      (dolist (m (list helix-normal-state-keymap
                       helix-view-map))
        (when (and (stringp old) (> (length old) 0))
          (define-key m (kbd old) nil))
        (define-key m (kbd new) ctl-z-map))))

  (defun eon-helix--set-leaders (sym val)
    "Setter for leader and local leader keys.
Used by custom variables `eon-helix-leader-key' and `eon-helix-localleader-key'."
    (let ((old (and (boundp sym) (default-value sym))))
      (set-default sym val)
      (with-eval-after-load 'helix
        (pcase sym
          ('eon-helix-leader-key
           (eon-helix--bind-leader-in-states old val))
          ('eon-helix-localleader-key
           (when old (define-key ctl-z-map (kbd old) nil))
           (define-key ctl-z-map (kbd val)
                       (cons "Local" ctl-z-localleader-map)))))))

  (defcustom eon-helix-leader-key ","
    "Leader key for Helix."
    :group 'eon :type 'string
    :set #'eon-helix--set-leaders
    :initialize 'custom-initialize-set)

  (defcustom eon-helix-localleader-key ","
    "Local leader key for Helix."
    :group 'eon :type 'string
    :set #'eon-helix--set-leaders
    :initialize 'custom-initialize-set)

  :config
  (helix-mode)

  ;; Explicitly bind the leader key
  (eon-helix--bind-leader-in-states nil eon-helix-leader-key))

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

;; _____________________________________________________________________________
;;; WHICH-KEY

;; Let which-key appear a bit faster
(use-package which-key :ensure nil
  :after helix
  :custom
  (which-key-idle-delay 0.25)
  (which-key-idle-secondary-delay 0.0))

;; _____________________________________________________________________________
;;; DIRED

(use-package dired :ensure nil
  :after helix
  :bind
  ;; Add Ranger-like movements to Dired
  (:map dired-mode-map
        ("l" . dired-find-file)
        ("h" . dired-up-directory)
        ("y" . dired-ranger-copy)
        ("p" . dired-ranger-paste)
        ("P" . dired-ranger-move)))

;; _____________________________________________________________________________
(provide 'eon-helix)
;;; eon-helix.el ends here
