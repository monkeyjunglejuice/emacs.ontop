;;; eon-eat.el --- Emulate a terminal -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; TERMINAL
;; <https://codeberg.org/akib/emacs-eat>
;; <https://elpa.nongnu.org/nongnu-devel/doc/eat.html>
;; Faster terminal emulator than `term' or `ansi-term'

;; To setup shell integration for GNU Bash, insert at the end of your .bashrc:
;; [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
;; source "$EAT_SHELL_INTEGRATION_DIR/bash"
;;
;; For Zsh, put the following in your .zshrc:
;; [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
;; source "$EAT_SHELL_INTEGRATION_DIR/zsh"

(use-package eat :ensure t

  :custom

  (eat-enable-auto-line-mode t)
  (eat-kill-buffer-on-exit t)

  :hook

  ;; Run Eshell always in Eat; this gives Eshell full terminal capabilities.
  ;; `eshell-mode-hook' seems the most robust trigger.
  (eshell-mode . eat-eshell-mode)
  (eshell-mode . eat-eshell-visual-command-mode)

  ;; Start Eat always in line mode, so it blends into Emacs like `shell'
  (eat-exec . (lambda (_)
                (eat-line-mode)
                (eon-cursor-update)))

  :bind

  (:map ctl-z-e-map
        ;; Set Eat as the default terminal emulator
        ("t" . eat)))

;; _____________________________________________________________________________
(provide 'eon-eat)
;;; eon-eat.el ends here
