;;; eon-helix.el --- Modal editing: Helix keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; NOTE Helix-mode for Emacs is new and experimental.
;;
;;; Keys
;; You can add new keys to the Helix keymaps via helix-define-key:
;; Example:
;; (helix-define-key 'space "w" #'do-something-cool)
;; The first argument to helix-define-key is a Helix state.
;; The valid options are: insert, normal, space, view, goto, and window.
;;
;;; Typable commands
;; You can create new typable commands (invoked via ":command-name")
;; with `helix-define-typable-command'.
;; Example:
;; (helix-define-typable-command "format" #'format-all-buffer)
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
        (define-key m (kbd new) eon-leader-map))))

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
           (when old (define-key eon-leader-map (kbd old) nil))
           (define-key eon-leader-map (kbd val)
                       (cons "Local" eon-localleader-map)))))))

  (defcustom eon-helix-leader-key ","
    "Leader key for Helix."
    :group 'eon-leader
    :type 'string
    :set #'eon-helix--set-leaders
    :initialize 'custom-initialize-set)

  (defcustom eon-helix-localleader-key ","
    "Local leader key for Helix."
    :group 'eon-leader
    :type 'string
    :set #'eon-helix--set-leaders
    :initialize 'custom-initialize-set)

  :config

  (helix-mode)

  ;; Explicitly bind the leader key
  (eon-helix--bind-leader-in-states nil eon-helix-leader-key)

  ;;; Show extra cursor when Helix is in normal state

  (defun eon-helix--selection-active-p ()
    (or (region-active-p)
        (and (boundp 'helix--current-selection)
             helix--current-selection)))

  (defun eon-helix--cursor-type-compute ()
    "Return a cursor type when Helix modes are active, else nil."
    (cond
     ;; Insert/editing
     ((bound-and-true-p helix-insert-mode)
      eon-cursor-type-write)
     ;; Visual/selection while normal keymap is active
     ((and (bound-and-true-p helix-normal-mode)
           (eon-helix--selection-active-p))
      eon-cursor-type-extra-select)
     ;; Plain normal state; no selection
     ((bound-and-true-p helix-normal-mode)
      eon-cursor-type-extra)
     (t nil)))

  (add-hook 'eon-cursor-type-functions #'eon-helix--cursor-type-compute)

  ;; Refresh on Helix state flips
  (add-hook 'helix-normal-mode-hook #'eon-cursor-type-update)
  (add-hook 'helix-insert-mode-hook #'eon-cursor-type-update))

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
