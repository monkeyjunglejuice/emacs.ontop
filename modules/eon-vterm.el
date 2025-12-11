;;; eon-vterm.el --- Faster terminal emulator in Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Emacs-libvterm (vterm) is fully-fledged terminal emulator based on the
;; external library libvterm loaded as a dynamic module. As a result of using
;; compiled code (instead of elisp), emacs-libvterm is fully capable, fast, and
;; it can seamlessly handle large outputs.
;;
;; Emacs-libvterm requires Emacs support for loading modules. You can check if
;; your Emacs supports modules by inspecting the variable `module-file-suffix'.
;; If it is nil, then you need to recompile Emacs or obtain a copy of Emacs with
;; this option enabled.
;;
;; Emacs-libvterm requires CMake and libvterm. If libvterm is not available,
;; emacs-libvterm will be downloaded and compiled. In this case, libtool is
;; needed.
;;
;;; Shell configuration
;;
;; You'll need to configure your shell in order to make full use of Vterm.
;; You can insert one of these snippets at the end of your .zshrc, .bashrc
;; or config.fish:
;;
;;; For ZSH:
;; if [[ "$INSIDE_EMACS" = 'vterm' ]] \
;;     && [[ -n ${EMACS_VTERM_PATH} ]] \
;;     && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
;;  source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
;; fi
;;
;;; For BASH:
;; if [[ "$INSIDE_EMACS" = 'vterm' ]] \
;;     && [[ -n ${EMACS_VTERM_PATH} ]] \
;;     && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
;;  source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
;; fi
;;
;;; For FISH:
;; if test "$INSIDE_EMACS" = vterm \
;;     and test -n "$EMACS_VTERM_PATH" \
;;     and test -f "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"
;;     source "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"
;; end
;;
;;; Code:

;; _____________________________________________________________________________
;;; VTERM
;; <https://github.com/akermu/emacs-libvterm>

(use-package vterm :ensure t

  :custom

  (vterm-max-scrollback 65536)

  :config

  (defvar eon-vterm-escape-command nil
    "Command to run when ESC is handled by Emacs in `vterm-mode'.
If nil, ESC is always sent directly to the Vterm process.")

  ;; Ensure ESC is not sent raw by vterm when we intercept it
  (add-to-list 'vterm-keymap-exceptions
               (key-description (kbd "<escape>")))

  (defvar-local eon-vterm-send-escape-to-vterm t
    "Non-nil means ESC goes to the vterm process.
When nil, ESC runs `eon-vterm-escape-command'.")

  (defun eon-vterm-update-escape ()
    "Install ESC behavior for `vterm-mode'."
    (when eon-vterm-escape-command
      (keymap-set vterm-mode-map "<escape>"
                  (if eon-vterm-send-escape-to-vterm
                      #'vterm--self-insert
                    eon-vterm-escape-command))))

  (defun eon-vterm-toggle-escape ()
    "Toggle whether ESC in vterm goes to vterm or Emacs."
    (interactive)
    (unless eon-vterm-escape-command
      (user-error "Set `eon-vterm-escape-command' first"))
    (setq eon-vterm-send-escape-to-vterm
          (not eon-vterm-send-escape-to-vterm))
    (eon-vterm-update-escape)
    (message "ESC → %s"
             (if eon-vterm-send-escape-to-vterm
                 "vterm process"
               (symbol-name eon-vterm-escape-command))))

  ;; Toggle where ESC goes in vterm
  (keymap-set vterm-mode-map "C-c C-q" #'eon-vterm-toggle-escape)

  :bind

  (:map vterm-mode-map
        ;; Send next key directly to the terminal, regardless Emacs keybindings
        ("C-q" . vterm-send-next-key))
  (:map ctl-z-e-map
        ;; Set Vterm as the default terminal emulator
        ("t" . vterm)))

;; _____________________________________________________________________________
;;; ESHELL VTERM
;; <https://github.com/iostapyshyn/eshell-vterm>
;; Allows Eshell to use `vterm' for visual commands

(use-package eshell-vterm :ensure t
  :after eshell
  :config
  (eshell-vterm-mode))

;; _____________________________________________________________________________
(provide 'eon-vterm)
;;; eon-vterm.el ends here
