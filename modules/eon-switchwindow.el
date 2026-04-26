;;; eon-switchwindow.el --- Navigate windows -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
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
 :conflicts '()
 :requires  '(eon))

;;  ___________________________________________________________________________
;;; SWITCH-WINDOW
;; <https://github.com/dimitri/switch-window>

(use-package switch-window :ensure t

  :custom

  (switch-window-background t)
  (switch-window-multiple-frames nil)
  (switch-window-threshold 2)
  (switch-window-mvborder-increment 1)
  (switch-window-shortcut-style 'qwerty)
  (switch-window-minibuffer-shortcut ?m)
  (switch-window-qwerty-shortcuts '("s" "d" "f"
                                    "w" "e" "r"
                                    "z" "x" "c"))

  :config

  (setq switch-window-extra-map
        (let ((map (make-sparse-keymap)))
          ;; Set Vim-like keybindings for window resizing
          (define-key map (kbd "k") 'switch-window-mvborder-up)
          (define-key map (kbd "j") 'switch-window-mvborder-down)
          (define-key map (kbd "h") 'switch-window-mvborder-left)
          (define-key map (kbd "l") 'switch-window-mvborder-right)
          (define-key map (kbd "b") 'balance-windows)
          (define-key map (kbd "SPC") 'switch-window-resume-auto-resize-window)
          map))
  (set-face-attribute 'switch-window-background nil
                      :foreground 'unspecified
                      :inherit 'shadow)
  (set-face-attribute 'switch-window-label nil
                      :inherit 'show-paren-match-expression
                      :height 1.5)

  :bind

  ;; Bind `switch-window' commands to regular Emacs keybindings
  ("C-x o"   . switch-window)
  ("C-x 1"   . switch-window-then-maximize)
  ("C-x 2"   . switch-window-then-split-below)
  ("C-x 3"   . switch-window-then-split-right)
  ("C-x 0"   . switch-window-then-delete)
  ("C-x 4 0" . switch-window-then-kill-buffer)
  ("C-x 4 b" . switch-window-then-display-buffer)
  ("C-x 4 d" . switch-window-then-dired)
  ("C-x 4 f" . switch-window-then-find-file)
  ("C-x 4 s" . switch-window-then-swap-buffer)

  ;; Bind `switch-window' commands in the leader keymap
  (:map ctl-z-w-map
        ("b" . switch-window-then-display-buffer)
        ("c" . switch-window-then-delete)
        ("d" . switch-window-then-dired)
        ("f" . switch-window-then-find-file)
        ("k" . switch-window-then-kill-buffer)
        ("m" . switch-window-then-maximize)
        ("s" . switch-window-then-split-below)
        ("v" . switch-window-then-split-right)
        ("w" . switch-window)
        ("x" . switch-window-then-swap-buffer)))

;; _____________________________________________________________________________
(provide 'eon-switchwindow)
;;; eon-switchwindow.el ends here
