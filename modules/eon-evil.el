;;; eon-evil.el --- Modal editing: Vim keybindings -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
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
;;; Code:

(eon-module-metadata
 :conflicts '(eon-god eon-helix eon-meow)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; EVIL
;; <https://evil.readthedocs.io/en/latest/settings.html>
;; <https://github.com/noctuid/evil-guide>

;; Evil states cheatsheet:
;; normal
;; visual
;; insert
;; emacs
;; operator
;; motion
;; replace
;; global (binds the key without evil current-global-map)

(use-package evil :ensure t

  :init

  ;; Let Evil handle the cursor styles
  (eon-cursor-mode -1)

  ;; Required settings for Evil Collection to handle keybindings
  (setopt evil-want-integration t
          evil-want-keybinding nil)

  ;; We're not using Evil's leader/localleader implementation. Instead we
  ;; wire the agnostic implementation from Emacs ONBOARD './eon.el' that works
  ;; independently from Evil.

  ;; We define a separate leader key for Evil here, so that the default leader
  ;; key fulfills the role of the alternative leader key. The alternative leader
  ;; leader will be accessible from Evil states other than <normal> or <visual>,
  ;; e.g. <insert> or <emacs>.
  ;;
  ;; You may want to customize the variables:
  ;; `eon-evil-leader-key' - default "SPC"
  ;; `eon-evil-localleader-key' - default ","
  ;; `eon-leader-key' - default for Evil "C-SPC"
  ;; `eon-localleader-key' - default "C-,"

  (defvar-keymap eon-evil-leader-map
    :doc "Evil frontend for the EON leader keymap.")

  (defun eon-evil--valid-key-p (key)
    "Return non-nil if KEY is a non-empty key description string."
    (and (stringp key)
         (> (length key) 0)))

  (defun eon-evil--state-maps ()
    "Return Evil keymaps that should receive the leader key."
    (delq nil
          (list (and (boundp 'evil-normal-state-map)
                     (keymapp evil-normal-state-map)
                     evil-normal-state-map)
                (and (boundp 'evil-visual-state-map)
                     (keymapp evil-visual-state-map)
                     evil-visual-state-map)
                (and (boundp 'evil-motion-state-map)
                     (keymapp evil-motion-state-map)
                     evil-motion-state-map))))

  (defun eon-evil--bind-leader (old new)
    "Bind NEW as Evil leader in Evil state maps and unset OLD."
    (set-keymap-parent eon-evil-leader-map eon-leader-map)
    (dolist (map (eon-evil--state-maps))
      (when (eon-evil--valid-key-p old)
        (keymap-unset map old t))
      (when (and (bound-and-true-p eon-leader-mode)
                 (eon-evil--valid-key-p new))
        ;; Bind to the Evil frontend, not the resolved map
        (keymap-set map new `("Leader" . ,eon-evil-leader-map)))))

  (defun eon-evil--bind-localleader (old new)
    "Bind NEW as Evil local leader in `eon-evil-leader-map' and unset OLD."
    (when (eon-evil--valid-key-p old)
      (keymap-unset eon-evil-leader-map old t))
    (when (and (bound-and-true-p eon-leader-mode)
               (eon-evil--valid-key-p new))
      (keymap-set eon-evil-leader-map new
                  `("Local" . ,eon-localleader-map))))

  (defun eon-evil--sync-leaders ()
    "Synchronize Evil leader keys with `eon-leader-mode'."
    (eon-evil--bind-leader eon-evil-leader-key
                           eon-evil-leader-key)
    (eon-evil--bind-localleader eon-evil-localleader-key
                                eon-evil-localleader-key))

  (defun eon-evil--set-leaders (sym val)
    "Set SYM to VAL and update the corresponding Evil leader binding."
    (let ((old (and (boundp sym) (default-value sym))))
      (set-default sym val)
      (pcase sym
        ('eon-evil-leader-key
         (eon-evil--bind-leader old val))
        ('eon-evil-localleader-key
         (eon-evil--bind-localleader old val)))))

  (defcustom eon-evil-leader-key "SPC"
    "Leader key for Evil."
    :group 'eon-leader
    :type 'string
    :set #'eon-evil--set-leaders
    :initialize 'custom-initialize-set)

  (defcustom eon-evil-localleader-key ","
    "Local leader key for Evil."
    :group 'eon-leader
    :type 'string
    :set #'eon-evil--set-leaders
    :initialize 'custom-initialize-set)

  :config

  ;; Enable Evil first
  (evil-mode 1)

  (setq evil-normal-state-cursor eon-cursor-type-extra
        evil-visual-state-cursor eon-cursor-type-extra-select
        evil-insert-state-cursor eon-cursor-type-write
        evil-emacs-state-cursor  eon-cursor-type-write)

  ;; Explicitly bind the Evil leader key, defaults to "SPC". Customize
  ;; `eon-evil-leader-key' and/or `eon-evil-localleader-key' to change.
  (eon-evil--sync-leaders)
  (add-hook 'eon-leader-mode-hook #'eon-evil--sync-leaders)

  ;; Set "M-SPC" as the default alternative leader key for Evil;
  ;; customize `eon-leader-key' to change the binding.
  (setopt eon-leader-key "M-SPC")

  ;; Escape from Evil Emacs state
  (evil-define-key 'emacs 'global [escape] #'evil-normal-state)

  ;; Comment/uncomment by pressing "gcc" in normal mode and "gc" in visual mode
  (evil-define-operator eon-evil-comment-or-uncomment (beg end)
                        "Toggle comment for the region between BEG and END."
                        (interactive "<r>")
                        (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global
                   (kbd "gc") #'eon-evil-comment-or-uncomment))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(use-package evil-vars :ensure nil
  :after evil
  :custom
  (evil-default-cursor 'box)
  (evil-echo-state nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-ex-search-vim-style-regexp t)
  (evil-ex-visual-char-range t)
  (evil-kbd-macro-suppress-motion-error t)
  (evil-move-cursor-back nil)
  (evil-symbol-word-search t)
  (evil-undo-system (if (featurep 'undo-fu) 'undo-fu 'undo-redo))
  ;; TODO `evil-lookup-func' should be a context-dependent documentation lookup;
  ;; - helpful-at-point for Elisp (implemented for now)
  ;; - sly-hyperspec-lookup for Common Lisp / Sly that opens page in EWW
  ;; - eglot doc buffer (eldoc) for LSP-enabled modes
  ;; - etc. -- there's probably a package for that already existing
  (evil-lookup-func #'helpful-at-point))

;; _____________________________________________________________________________
;;; EVIL COLLECTION
;; Eternal mission to evilize everything - a bottomless hole
;; <https://github.com/emacs-evil/evil-collection>

(use-package evil-collection :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode

  :init

  ;; Use Evil editing-behavior in the minibuffer too
  (setopt evil-collection-setup-minibuffer t)

  :custom

  ;; Prevent the leader key from getting shadowed by evil-collection
  (evil-collection-key-blacklist `(,eon-evil-leader-key
                                   ,eon-leader-key
                                   ,eon-localleader-key))

  :config

  (evil-collection-init)

  ;; Add VIM-like movements to Dired
  (evil-collection-define-key 'normal 'dired-mode-map
    "l" #'dired-find-file
    "h" #'dired-up-directory)

  ;; Add VIM-like copy/paste to Dired
  (when (eon-modulep 'eon-dired)
    (evil-collection-define-key 'normal 'dired-mode-map
      "y" #'dired-ranger-copy
      "p" #'dired-ranger-paste
      "P" #'dired-ranger-move)))

;; _____________________________________________________________________________
;;; WHICH-KEY

(use-package which-key :ensure nil
  :after evil
  :custom
  (which-key-allow-evil-operators t))

;; _____________________________________________________________________________
(provide 'eon-evil)
;;; eon-evil.el ends here
