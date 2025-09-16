;;; eon-switchwindow.el --- Auto package update  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-switchwindow.el")'.

;;; Code:

;;  ___________________________________________________________________________
;;; WINDOW MANAGEMENT
;; <https://github.com/dimitri/switch-window>

(use-package switch-window :ensure t
  :custom
  (switch-window-background t)
  (switch-window-multiple-frames nil)
  (switch-window-threshold 1)
  (switch-window-mvborder-increment 1)
  (switch-window-minibuffer-shortcut 109) ; "m"
  :config
  ;; Set Vim/Xmonad-like keybindings for window resizing
  (setq switch-window-extra-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "k") 'switch-window-mvborder-up)
          (define-key map (kbd "j") 'switch-window-mvborder-down)
          (define-key map (kbd "h") 'switch-window-mvborder-left)
          (define-key map (kbd "l") 'switch-window-mvborder-right)
          (define-key map (kbd "b") 'balance-windows)
          (define-key map (kbd "SPC") 'switch-window-resume-auto-resize-window)
          map))
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "g"
          "q" "w" "e" "r" "t" "y"
          "u" "i" "o" "p"
          "z" "x" "c" "v"
          "b" "n"))
  (set-face-attribute 'switch-window-background nil
                      :foreground 'unspecified
                      :inherit 'shadow)
  (set-face-attribute 'switch-window-label nil
                      :inherit 'show-paren-match-expression
                      :height 1.0)
  :bind
  ;; Bind `switch-window' commands to regular Emacs keybindings
  ("C-x o"   . switch-window)
  ("C-x 1"   . switch-window-then-maximize)
  ("C-x 2"   . switch-window-then-split-below)
  ("C-x 3"   . switch-window-then-split-right)
  ("C-x 0"   . switch-window-then-delete)
  ("C-x 4 0" . switch-window-then-kill-buffer)
  ("C-x 4 d" . switch-window-then-dired)
  ("C-x 4 f" . switch-window-then-find-file)
  ("C-x 4 b" . switch-window-then-display-buffer)
  ("C-x 4 s" . switch-window-then-swap-buffer))

;;  ____________________________________________________________________________
(provide 'eon-switchwindow)
;;; eon-switchwindow.el ends here
