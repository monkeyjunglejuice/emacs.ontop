;;; eon-corfu.el --- Code (auto)completion -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; CORFU
;; <https://github.com/minad/corfu>
;; <https://github.com/minad/corfu?tab=readme-ov-file#orderless-completion>

(use-package corfu :ensure t
  :init
  ;; Prevent *Completions* buffer from popping up
  (setopt completion-auto-help nil)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :config
  (corfu-history-mode 1)
  (corfu-echo-mode 1)
  :hook
  ((prog-mode conf-mode) . corfu-mode))

;;  ____________________________________________________________________________
(provide 'eon-corfu)
;;; eon-corfu.el ends here
