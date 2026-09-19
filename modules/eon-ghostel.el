;;; eon-ghostel.el --- Terminal emulator powered by libghostty -*- lexical-binding: t; no-byte-compile: t; -*-

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
;; Requirements: Emacs with dynamic module support, on macOS, Linux, FreeBSD,
;; Android/Termux, or native Windows. The native module is a prebuilt binary
;; that auto-downloads on first use. No toolchain or build step required.
;;
;; Windows release binaries are built for common native Windows Emacs builds on
;; x86_64 and aarch64. Releases include optional ConPTY support files from
;; Microsoft's redistributable console runtime, which can improve latency and
;; correctness compared with older inbox Windows ConPTY versions.
;;
;;; Shell integration
;;
;; Bash - add to ~/.bashrc:
;; [[ "${INSIDE_EMACS%%,*}" = 'ghostel' ]] &&
;;     source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.bash"
;;
;; Zsh - add to ~/.zshrc:
;; [[ "${${INSIDE_EMACS-}%%,*}" = 'ghostel' ]] &&
;;     source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.zsh"
;;
;; Fish - add to ~/.config/fish/config.fish:
;; string match -qr '^ghostel(,|$)' -- "$INSIDE_EMACS";
;; and source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.fish"
;;
;;; Website:
;; <https://dakra.github.io/ghostel>
;; <https://github.com/dakra/ghostel
;;
;;; Code:

(eon-module-metadata
 :conflicts '(eon-vterm)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; GHOSTEL

(use-package ghostel :ensure t

  :init

  (defun eon-ghostel-send-q ()
    "Pass the 'q' key to Ghostel, commonly used to quit TUI programs."
    (interactive)
    (ghostel-send-key "q"))

  (defun eon-ghostel-send-spc ()
    "Pass the 'SPC' key to Ghostel."
    (interactive)
    (ghostel-send-key "SPC"))

  (defun eon-ghostel-send-esc ()
    "Pass the 'ESC' key to Ghostel."
    (interactive)
    (ghostel-send-key "ESC"))

  (eon-localleader-defkeymap ghostel-mode eon-localleader-ghostel-map
    :doc "Local leader keymap for Ghostel buffers."
    "\\"  #'ghostel-send-next-key
    "q"   #'eon-ghostel-send-q
    "SPC" #'eon-ghostel-send-spc
    "ESC" #'eon-ghostel-send-esc)

  :custom

  (ghostel-max-scrollback (* 32 1024 1024))  ; MiB

  ;; Start Ghostel in line mode instead of semi-char mode
  (ghostel-initial-input-mode 'line)
  ;; The shell that gets run in Ghostel for Tramp
  (ghostel-tramp-shells '(("ssh" login-shell "/bin/bash")
                          ("scp" login-shell "/bin/bash")
                          ("docker" "/bin/sh")))

  :bind

  (:map ghostel-mode-map
        ;; Send next key directly to the terminal, regardless Emacs keybindings
        ("C-q" . ghostel-send-next-key))
  (:map ctl-z-e-map
        ;; Set Ghostel as the default terminal emulator
        ("t" . ghostel)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ESHELL INTEGRATION
;; Allows Eshell to use Ghostel for visual commands

(use-package ghostel-eshell :ensure nil
  :hook
  (eshell-load . ghostel-eshell-visual-command-mode))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; EVIL INTEGRATION

(when (eon-modulep 'eon-evil)
  (use-package evil-ghostel :ensure t
    :after (ghostel evil)
    :hook (ghostel-mode . evil-ghostel-mode)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; MEOW INTEGRATION
;; <https://github.com/dakra/meow-ghostel>

(when (eon-modulep 'eon-meow)
  (use-package meow-ghostel
  :vc (:url "https://github.com/dakra/meow-ghostel" :rev :newest)
  :after (ghostel meow)
  :hook (ghostel-mode . meow-ghostel-mode)))

;; _____________________________________________________________________________
(provide 'eon-ghostel)
;;; eon-ghostel.el ends here
