;;; eon-corfu.el --- Code (auto-)completion -*- lexical-binding: t; no-byte-compile: t; -*-
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
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-max-width 120)
  (corfu-on-exact-match 'nil)
  (corfu-popupinfo-delay '(0.3 . 0.1))
  (corfu-popupinfo-max-height 10)
  (corfu-preselect 'valid)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match 'separator)
  (global-corfu-minibuffer t)
  :config
  (global-corfu-mode)
  (corfu-echo-mode)
  :bind
  (:map corfu-map
        ;; ("TAB" . corfu-next)
        ;; ([tab] . corfu-next)
        ;; ("S-TAB" . corfu-previous)
        ;; ([backtab] . corfu-previous)
        ;; ("S-RET" . corfu-insert)
        ;; ("S-<return>" . corfu-insert)
        ("RET" . nil)))

(use-package corfu-history :ensure nil
  :config
  (eon-add-to-list-setopt 'savehist-additional-variables 'corfu-history)
  :hook
  (corfu-mode . corfu-history-mode))

;;  ____________________________________________________________________________
(provide 'eon-corfu)
;;; eon-corfu.el ends here
