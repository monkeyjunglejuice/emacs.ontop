;;; eon-switchwindow.el --- Navigate windows -*- lexical-binding: t; no-byte-compile: t; -*-

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
 :conflicts '()
 :requires  '(eon))

;;  ___________________________________________________________________________
;;; SWITCH-WINDOW
;; <https://github.com/dimitri/switch-window>

(use-package switch-window :ensure t

  :custom

  (switch-window-background t)
  (switch-window-multiple-frames t)
  (switch-window-threshold 2)
  (switch-window-mvborder-increment 1)
  (switch-window-shortcut-style 'qwerty)
  (switch-window-minibuffer-shortcut ?m)
  (switch-window-qwerty-shortcuts '("s" "d" "f"
                                    "w" "e" "r"
                                    "z" "x" "c"))

  :config

  ;; Remove hard-coded right split for commands like `switch-window-then-dired',
  ;; in order to respect user preference. Uses `split-window-sensibly' when just
  ;; one single window exists.
  ;; Upstream pull request: <https://github.com/dimitri/switch-window/pull/97>

  (defun eon-switch-window--then-other-window (prompt function)
    "PROMPT for a target window and call FUNCTION there.
  
  This override uses `split-window-sensibly' instead of the
  upstream hardcoded `split-window-right'. When a new window is
  created speculatively, it is deleted again if FUNCTION quits or
  signals an error."
    (let ((f (switch-window--get-preferred-function function)))
      (switch-window--then
       prompt
       (lambda ()
         (let* ((original-window (selected-window))
                (created-window
                 (and (one-window-p)
                      (or (split-window-sensibly)
                          (user-error
                           "Cannot split selected window sensibly"))))
                (target-window
                 (or created-window
                     (next-window))))
           (select-window target-window)
           (condition-case signal-data
               (call-interactively f)
             ((quit error)
              (when (and created-window
                         (window-live-p created-window))
                (delete-window created-window))
              (when (window-live-p original-window)
                (select-window original-window))
              (signal (car signal-data) (cdr signal-data))))))
       (lambda () (call-interactively f))
       nil
       2)))

  (advice-add 'switch-window--then-other-window
              :override
              #'eon-switch-window--then-other-window)
  
  ;; Keybindings when `switch-window' UI is active

  (setq switch-window-extra-map
        (let ((map (make-sparse-keymap)))
          ;; Set Vim-like keybindings for window resizing
          (define-key map (kbd "k") 'switch-window-mvborder-up)
          (define-key map (kbd "j") 'switch-window-mvborder-down)
          (define-key map (kbd "h") 'switch-window-mvborder-left)
          (define-key map (kbd "l") 'switch-window-mvborder-right)
          (define-key map (kbd "=") 'balance-windows)
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
        ("S" . switch-window-then-split-right)
        ("w" . switch-window)
        ("x" . switch-window-then-swap-buffer)))

;; _____________________________________________________________________________
(provide 'eon-switchwindow)
;;; eon-switchwindow.el ends here
