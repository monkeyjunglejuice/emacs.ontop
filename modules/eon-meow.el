;;; eon-meow.el --- Modal editing: Meow keybindings -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.2
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
 :conflicts '(eon-evil eon-god eon-hel eon-helix)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; MEOW
;; <https://github.com/meow-edit/meow>

(use-package meow :ensure t

  :init

  ;; Let Meow handle the cursor style
  (eon-cursor-mode -1)

  (defun eon-meow--valid-key-p (key)
    "Return non-nil if KEY is a non-empty key description string."
    (and (stringp key)
         (> (length key) 0)))

  (defun eon-meow--prefix (binding)
    "Return (TITLE . KEYMAP) when BINDING denotes a prefix map."
    (cond
     ((and (consp binding)
           (stringp (car binding))
           (keymapp (cdr binding)))
      binding)
     ((keymapp binding)
      (cons (or (keymap-prompt binding) "Prefix")
            binding))))

  (defun eon-meow--keypad-title (definition)
    "Return a Meow keypad title for DEFINITION."
    (if (and (consp definition)
             (stringp (car definition)))
        (intern (car definition))
      (meow-keypad-get-title definition)))

  (defun eon-meow--display-keymap (keymap title)
    "Display KEYMAP in Meow keypad style under TITLE."
    (when meow-keypad-describe-keymap-function
      (let ((display-map (make-sparse-keymap))
            (meow-keypad-message-prefix (concat title ": ")))
        (map-keymap
         (lambda (event binding)
           (unless (eq event 'remap)
             (define-key
              display-map
              (vector event)
              (funcall meow-keypad-get-title-function binding))))
         keymap)
        (funcall meow-keypad-describe-keymap-function display-map))))

  (defun eon-meow--dispatch-map (keymap)
    "Return a transient dispatcher for KEYMAP.

Commands retain their original bindings. Prefix maps re-enter the EON
Meow frontend so their bindings are displayed in Meow keypad style."
    (let ((dispatch-map (copy-keymap keymap)))
      (map-keymap
       (lambda (event binding)
         (when-let* ((prefix (eon-meow--prefix binding)))
           (let ((title (car prefix))
                 (prefix-map (cdr prefix)))
             (define-key
              dispatch-map
              (vector event)
              (lambda ()
                (interactive)
                (eon-meow--enter-keymap prefix-map title))))))
       keymap)
      dispatch-map))

  (defun eon-meow--enter-keymap (keymap title)
    "Enter KEYMAP transiently and display it under TITLE."
    (let ((keymap (keymap-canonicalize keymap)))
      (set-transient-map (eon-meow--dispatch-map keymap))
      (eon-meow--display-keymap keymap title)))

  (defun eon-meow--leader ()
    "Enter the EON leader from Meow."
    (interactive)
    (eon-leader--sync-prefix-parent)
    (eon-localleader--sync-local-prefix-parent)
    (eon-meow--enter-keymap eon-leader-map "Leader"))

  (defun eon-meow--localleader ()
    "Enter the EON local leader from Meow."
    (interactive)
    (eon-localleader--sync-local-prefix-parent)
    (eon-meow--enter-keymap eon-localleader-map "Local"))

  (defun eon-meow--bind-entry (old new command)
    "Replace OLD with NEW in Meow's leader map, bound to COMMAND."
    (let ((leader-map (alist-get 'leader meow-keymap-alist)))
      (when (eon-meow--valid-key-p old)
        (keymap-unset leader-map old t))
      (when (and (bound-and-true-p eon-leader-mode)
                 (eon-meow--valid-key-p new))
        (meow-leader-define-key
         (cons new command)))))

  (defun eon-meow--sync-leaders ()
    "Synchronize the Meow frontend with `eon-leader-mode'."
    (eon-meow--bind-entry eon-meow-leader-key
                          eon-meow-leader-key
                          #'eon-meow--leader)
    (eon-meow--bind-entry eon-meow-localleader-key
                          eon-meow-localleader-key
                          #'eon-meow--localleader))

  (defun eon-meow--set-leaders (symbol value)
    "Set SYMBOL to VALUE and update its Meow leader binding."
    (let ((old (and (boundp symbol)
                    (default-value symbol))))
      (set-default symbol value)
      (when (featurep 'meow)
        (pcase symbol
          ('eon-meow-leader-key (eon-meow--bind-entry
                                 old value #'eon-meow--leader))
          ('eon-meow-localleader-key (eon-meow--bind-entry
                                      old value #'eon-meow--localleader))))))

  (defcustom eon-meow-leader-key "SPC"
    "Key for entering the EON leader from Meow's keypad."
    :group 'eon-leader
    :type 'string
    :set #'eon-meow--set-leaders
    :initialize 'custom-initialize-set)

  (defcustom eon-meow-localleader-key ","
    "Key for entering the EON local leader from Meow's keypad."
    :group 'eon-leader
    :type 'string
    :set #'eon-meow--set-leaders
    :initialize 'custom-initialize-set)

  (defun eon-meow--filter-keypad-description (keymap)
    "Return KEYMAP adapted for the EON Meow keypad display.

Hide command remappings and restore Meow's literal-prefix key when it
has an actual leader binding."
    (if (not (keymapp keymap))
        keymap
      (let ((filtered-map (make-sparse-keymap)))
        (map-keymap
         (lambda (event binding)
           (unless (or (eq event 'remap)
                       (null binding))
             (define-key filtered-map (vector event) binding)))
         keymap)
        (when (null meow--keypad-keys)
          (let* ((leader-map (alist-get 'leader meow-keymap-alist))
                 (event meow-keypad-literal-prefix)
                 (binding (lookup-key leader-map (vector event))))
            (when binding
              (define-key
               filtered-map
               (vector event)
               (funcall meow-keypad-get-title-function binding)))))
        filtered-map)))

  :config

  ;; Meow hides its literal-prefix key from the initial keypad popup.
  ;; Restore it when that key has an actual leader binding.
  (advice-add 'meow--keypad-get-keymap-for-describe
              :filter-return
              #'eon-meow--filter-keypad-description)

  (defun eon-meow-setup-qwerty ()
    "Set up Meow for a QWERTY keyboard."
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
          meow-keypad-get-title-function #'eon-meow--keypad-title)

    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))

    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  (eon-meow-setup-qwerty)

  ;; Add the configurable EON entries after the ordinary Meow bindings
  (eon-meow--sync-leaders)
  (add-hook 'eon-leader-mode-hook #'eon-meow--sync-leaders)

  ;; Enable Meow
  (meow-global-mode 1))

;; _____________________________________________________________________________
;;; MEOW TREE SITTER

(use-package meow-tree-sitter :ensure t
  :after meow
  :config
  (meow-tree-sitter-register-defaults))

;; _____________________________________________________________________________
(provide 'eon-meow)
;;; eon-meow.el ends here
