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
;;; Code:

;; _____________________________________________________________________________
;;; VTERM
;; <https://github.com/akermu/emacs-libvterm>
;; Faster terminal emulator than `term' or `ansi-term'

(use-package vterm :ensure t
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
