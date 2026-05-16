;;; eon-meow.el --- Modal editing: Meow keybindings -*- lexical-binding: t; no-byte-compile: t; -*-

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
;;; Code:

(eon-module-metadata
 :conflicts '(eon-evil eon-god eon-helix)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; MEOW
;; <https://github.com/meow-edit/meow>

(use-package meow :ensure t

  :init

  ;; Let Meow handle the cursor style
  (eon-cursor-mode -1)

  (defun eon-meow--keypad-title (definition)
    "Return a Meow keypad title for DEFINITION.

Understands labelled bindings of the form \(LABEL . VALUE),
where LABEL is a string. VALUE may be a command, a keymap object,
or a symbol whose value is a keymap."
    (cond
     ;; EON / which-key style labels: `(\"Label\" . VALUE)'.
     ;; Preserve the label, regardless of whether VALUE is a command or keymap.
     ((and (consp definition)
           (stringp (car definition)))
      (intern (car definition)))

     ;; Fallback for keymaps that have a prompt/name.  Use sparingly in your
     ;; own maps, because keymap prompts can affect command-loop behavior.
     ((and (keymapp definition)
           (keymap-prompt definition))
      (intern (keymap-prompt definition)))

     ;; Fallback to Meow's normal title logic
     (t
      (meow-keypad-get-title definition))))

  (defun eon-meow--hide-remap-entry (keymap)
    "Return a copy of KEYMAP without display-noise entries.

Meow builds a temporary keymap for keypad help. Command remappings
can appear there as the dummy event `remap', which is not a real
user-facing key. Do not merely bind it to nil; skip it entirely."
    (if (not (keymapp keymap))
        keymap
      (let ((filtered (make-sparse-keymap)))
        (set-keymap-parent filtered (keymap-parent keymap))
        (map-keymap
         (lambda (event binding)
           (unless (or (eq event 'remap)
                       (null binding))
             (define-key filtered (vector event) binding)))
         keymap)
        filtered)))

  :config

  (advice-add 'meow--keypad-get-keymap-for-describe
              :filter-return
              #'eon-meow--hide-remap-entry)

  ;; QWERTY layout
  (defun meow-setup-qwerty ()
    "Meow setup for QWERTY layout."
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
          meow-keypad-get-title-function #'eon-meow--keypad-title)
    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; EON leader inside Meow keypad
     `("," . ("Leader" . ,eon-leader-map))
     ;; Use SPC (0-9) for digit arguments.
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

  ;; Run setup
  (meow-setup-qwerty)
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
