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
 :requires  '(eon eon-base))

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

  (ghostel-max-scrollback (* 8 1024 1024))  ; MiB
  (ghostel-ignore-cursor-change t)
  (ghostel-readonly-fake-cursor nil)

  ;; The shell that gets run in Ghostel for Tramp connections;
  ;; prefer the user's login shell, fall back to /bin/sh.
  (ghostel-tramp-shells '((t login-shell "/bin/sh")
                          ("docker" "/bin/sh")))

  ;; Automatically transfer integration scripts to the remote host.
  ;; This creates small temporary files on the remote host;
  ;; cleaned up when the terminal exits.
  (ghostel-tramp-shell-integration t)

  :config

  ;; Login-shell discovery on MacOS
  (defun eon-ghostel-tramp-shell-spec (function method)
    "Use MacOS login-shell discovery around FUNCTION for METHOD."
    (let ((spec (cdr (assoc method ghostel-tramp-shells))))
      (if (eq (car spec) 'login-shell)
          (if-let* ((shell (eon-tramp-macos-login-shell)))
              (cons shell (cddr spec))
            (funcall function method))
        (funcall function method))))

  (advice-add 'ghostel--tramp-shell-spec
              :around #'eon-ghostel-tramp-shell-spec)

  ;; Don't pass "<escape>" to the terminal in `semi-char-mode';
  ;; to send ESC, use "<localleader> ESC" instead.
  (setopt ghostel-keymap-exceptions
          (eon-adjoin ghostel-keymap-exceptions "<escape>"))

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
    :hook
    (ghostel-mode . evil-ghostel-mode)))

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
