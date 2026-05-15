;;; eon-helix.el --- Modal editing: Helix keybindings -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.1
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;;; NOTE Helix-mode for Emacs is new and under heavy development right now,
;;       it may happen that this config may break from time to time.
;;
;;; - Keys
;; You can add new keys to the Helix keymaps via helix-define-key:
;; Example:
;; (helix-define-key 'space "w" #'do-something-cool)
;; The first argument to helix-define-key is a Helix state.
;; The valid options are: insert, normal, space, view, goto, and window.
;;
;;; - Typable commands
;; You can create new typable commands (invoked via ":command-name")
;; with `helix-define-typable-command'.
;; Example:
;; (helix-define-typable-command "format" #'format-all-buffer)
;;
;;; Code:

(eon-module-metadata
 :conflicts '(eon-evil eon-god eon-meow)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; HELIX
;; <https://github.com/mgmarlow/helix-mode>

(use-package helix
  :vc (:url "https://github.com/mgmarlow/helix-mode")
  :diminish helix-normal-mode helix-insert-mode

  :init

  (defvar-keymap eon-helix-leader-map
    :doc "Helix frontend for the EON leader keymap.")

  (defun eon-helix--valid-key-p (key)
    "Return non-nil if KEY is a non-empty key description string."
    (and (stringp key)
         (> (length key) 0)))

  (defun eon-helix--state-maps ()
    "Return Helix keymaps that should receive the leader key."
    (delq nil
          (list (and (boundp 'helix-normal-state-keymap)
                     (keymapp helix-normal-state-keymap)
                     helix-normal-state-keymap)
                (and (boundp 'helix-view-map)
                     (keymapp helix-view-map)
                     helix-view-map))))

  (defun eon-helix--bind-leader (old new)
    "Bind NEW as Helix leader in Helix state maps and unset OLD."
    (set-keymap-parent eon-helix-leader-map eon-leader-map)
    (dolist (map (eon-helix--state-maps))
      (when (eon-helix--valid-key-p old)
        (keymap-unset map old t))
      (when (and (bound-and-true-p eon-leader-mode)
                 (eon-helix--valid-key-p new))
        ;; Bind to the Helix frontend, not the resolved map
        (keymap-set map new `("Leader" . ,eon-helix-leader-map)))))

  (defun eon-helix--bind-localleader (old new)
    "Bind NEW as Helix local leader in `eon-helix-leader-map' and unset OLD."
    (when (eon-helix--valid-key-p old)
      (keymap-unset eon-helix-leader-map old t))
    (when (and (bound-and-true-p eon-leader-mode)
               (eon-helix--valid-key-p new))
      (keymap-set eon-helix-leader-map new
                  `("Local" . ,eon-localleader-map))))

  (defun eon-helix--sync-leaders ()
    "Synchronize Helix leader keys with `eon-leader-mode'."
    (eon-helix--bind-leader eon-helix-leader-key
                            eon-helix-leader-key)
    (eon-helix--bind-localleader eon-helix-localleader-key
                                 eon-helix-localleader-key))

  (defun eon-helix--set-leaders (sym val)
    "Set SYM to VAL and update the corresponding Helix leader binding."
    (let ((old (and (boundp sym) (default-value sym))))
      (set-default sym val)
      (pcase sym
        ('eon-helix-leader-key
         (eon-helix--bind-leader old val))
        ('eon-helix-localleader-key
         (eon-helix--bind-localleader old val)))))

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

  (defun eon-helix--selection-active-p ()
    "Return non-nil if Helix has an active selection."
    (or (region-active-p)
        (and (boundp 'helix--current-selection)
             helix--current-selection)))

  (defun eon-helix--cursor-compute ()
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

  :config

  ;; Explicitly bind the leader key
  (eon-helix--sync-leaders)
  (add-hook 'eon-leader-mode-hook #'eon-helix--sync-leaders)

  ;;; Show extra cursor when Helix is in normal state
  (add-hook 'eon-cursor-functions #'eon-helix--cursor-compute)

  ;; Refresh on Helix state flips
  (add-hook 'helix-normal-mode-hook #'eon-cursor-update)
  (add-hook 'helix-insert-mode-hook #'eon-cursor-update)

  ;; Dired-local Helix normal-state overrides
  (helix-define-key 'normal "l" #'dired-find-file 'dired-mode)
  (helix-define-key 'normal "h" #'dired-up-directory 'dired-mode)
  (when (eon-modulep 'eon-dired)
    (helix-define-key 'normal "y" #'dired-ranger-copy 'dired-mode)
    (helix-define-key 'normal "p" #'dired-ranger-paste 'dired-mode)
    (helix-define-key 'normal "P" #'dired-ranger-move 'dired-mode))

  ;; Enable Helix, but do not toggle it off when this file is reloaded
  (unless helix-global-mode
    (helix-mode)))

;; _____________________________________________________________________________
(provide 'eon-helix)
;;; eon-helix.el ends here
