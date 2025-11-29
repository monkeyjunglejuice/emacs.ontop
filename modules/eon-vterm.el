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
;; Emacs-libvterm needs you to configure your shell, in order to make full
;; use of it. You can insert one of these snippets at the end of your .zshrc,
;; .bashrc or config.fish:
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
;; Faster terminal emulator than `term' or `ansi-term'

(use-package vterm :ensure t
  :custom
  (vterm-max-scrollback 65536)
  :bind
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
