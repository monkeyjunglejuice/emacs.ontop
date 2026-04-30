;;; eon-god.el --- Modal editing: Emacs keybindings -*- lexical-binding: t; no-byte-compile: t; -*-

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
;; What god-mode does:
;;
;; Before: C-p C-k C-n M-^ ) C-j C-y M-r C-x z z M-2 M-f C-x C-s
;; After:    p   k   n g ^ )   j   y g r     . .   2 g f   x   s
;;
;;; Code:

(eon-module-metadata
 :conflicts '(eon-evil eon-helix eon-meow)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; GOD MODE
;; <https://github.com/emacsorphanage/god-mode>

(use-package god-mode :ensure t

  :init

  (defvar-keymap eon-god-leader-map
    :doc "God-mode frontend for the EON leader keymap.")

  (defvar eon-god--emulation-map (make-sparse-keymap)
    "Emulation map used to protect the God leader key.")

  (defun eon-god--valid-key-p (key)
    "Return non-nil if KEY is a non-empty key description string."
    (and (stringp key)
         (> (length key) 0)))

  (defun eon-god--install-emulation ()
    "Install `eon-god--emulation-map' for `god-local-mode'."
    (add-to-list 'emulation-mode-map-alists
                 `((god-local-mode . ,eon-god--emulation-map))))

  (defun eon-god--bind-leader (old new)
    "Bind NEW as God leader in the emulation map and unset OLD."
    (set-keymap-parent eon-god-leader-map eon-leader-map)
    (when (eon-god--valid-key-p old)
      (keymap-unset eon-god--emulation-map old t))
    (when (and (bound-and-true-p eon-leader-mode)
               (eon-god--valid-key-p new))
      ;; Bind to the God frontend, not the resolved map
      (keymap-set eon-god--emulation-map new
                  `("Leader" . ,eon-god-leader-map))))

  (defun eon-god--bind-localleader (old new)
    "Bind NEW as God local leader in `eon-god-leader-map' and unset OLD."
    (when (eon-god--valid-key-p old)
      (keymap-unset eon-god-leader-map old t))
    (when (and (bound-and-true-p eon-leader-mode)
               (eon-god--valid-key-p new))
      (keymap-set eon-god-leader-map new
                  `("Local" . ,eon-localleader-map))))

  (defun eon-god--sync-leaders ()
    "Synchronize God leader keys with `eon-leader-mode'."
    (eon-god--bind-leader eon-god-leader-key eon-god-leader-key)
    (eon-god--bind-localleader eon-god-localleader-key
                               eon-god-localleader-key))

  (defun eon-god--set-leaders (sym val)
    "Set SYM to VAL and update the corresponding God leader binding."
    (let ((old (and (boundp sym) (default-value sym))))
      (set-default sym val)
      (pcase sym
        ('eon-god-leader-key
         (eon-god--bind-leader old val))
        ('eon-god-localleader-key
         (eon-god--bind-localleader old val)))))

  (defcustom eon-god-leader-key ","
    "Leader key for God mode."
    :group 'eon-leader
    :type 'string
    :set #'eon-god--set-leaders
    :initialize 'custom-initialize-set)

  (defcustom eon-god-localleader-key ","
    "Local leader key for God mode, pressed after the leader key."
    :group 'eon-leader
    :type 'string
    :set #'eon-god--set-leaders
    :initialize 'custom-initialize-set)

  ;; Enable God mode almost everywhere;
  ;; see `god-exempt-major-modes' and `god-exempt-preticates' where not.
  (god-mode-all 1)

  :custom

  (god-mode-lighter-string "G")
  (god-mode-enable-function-key-translation nil)

  :config

  (defun eon-god-local-mode-activate ()
    "Turn God mode on.
Bound to \"<escape>\" per default."
    (interactive)
    (god-local-mode 1))

  (defun eon-god-local-mode-disable ()
    "Turn God mode off.
Bound to \"i\" per default."
    (interactive)
    (god-local-mode -1))

  ;; Bind the leader key
  (eon-god--install-emulation)
  (eon-god--sync-leaders)
  (add-hook 'eon-leader-mode-hook #'eon-god--sync-leaders)

  ;; Show special cursor while `god-local-mode' is active in a buffer
  (defun eon-god--cursor-compute ()
    "Return a cursor type when God-mode is active, else nil."
    (cond
     ;; Selection while god-local-mode is active
     ((and (bound-and-true-p god-local-mode)
           (region-active-p))
      eon-cursor-type-extra-select)
     ;; Normal god-local-mode state; no selection
     ((bound-and-true-p god-local-mode)
      eon-cursor-type-extra)
     (t nil)))
  (add-hook 'eon-cursor-functions #'eon-god--cursor-compute)

  ;; Refresh cursor when god-mode toggles
  (add-hook 'god-local-mode-hook #'eon-cursor-update)

  ;; Unblock a major mode
  (defun eon-god-unexempt-mode (&rest modes)
    "Remove certain MODES from `god-exempt-major-modes'.
Once removed, they will start with `god-local-mode' enabled."
    (setopt god-exempt-major-modes
            (seq-difference god-exempt-major-modes modes #'eq)))

  ;; Unblock by predicate
  (defun eon-god-unexempt-predicate (&rest predicates)
    "Remove certain PREDICATES from `god-exempt-predicates'.
Once removed, they will start with `god-local-mode' enabled."
    (setopt god-exempt-predicates
            (seq-difference god-exempt-predicates predicates #'eq)))

  ;; Don't start `vterm' with God-mode enabled
  (eon-add-to-list* 'god-exempt-major-modes 'vterm-mode)

  ;; Intercept the ESC key and let Emacs handle it when in `vterm' buffer;
  ;; toggle via "C-c C-q" between interception and passing through to `vterm'.
  (with-eval-after-load 'vterm
    (add-hook 'vterm-mode-hook
              (lambda ()
                (setq eon-vterm-escape-command #'eon-god-local-mode-activate
                      eon-vterm-send-escape-to-vterm nil)
                (eon-vterm-update-escape))))

  :bind

  ("<escape>" . eon-god-local-mode-activate)
  (:map god-local-mode-map
        ("i" . eon-god-local-mode-disable)
        ("." . repeat)
        ("V" . scroll-down-command)
        ("q" . quit-window)))

;; _____________________________________________________________________________
;;; ISEARCH

;; Adjustments for Isearch
(use-package god-mode-isearch :ensure nil
  :bind
  (:map isearch-mode-map
        ("<escape>" . god-mode-isearch-activate))
  (:map god-mode-isearch-map
        ("<escape>" . god-mode-isearch-disable)))

;; _____________________________________________________________________________
;;; WHICH-KEY

(use-package which-key :ensure nil
  :config
  (which-key-enable-god-mode-support))

;; _____________________________________________________________________________
(provide 'eon-god)
;;; eon-god.el ends here
