;;; eon-corfu.el --- Code (auto-)completion -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; CORFU
;; <https://github.com/minad/corfu>
;; <https://github.com/minad/corfu?tab=readme-ov-file#orderless-completion>

(use-package corfu :ensure t
  :init

  ;; Prevent *Completions* buffer from popping up
  (setopt completion-auto-help nil)

  ;; Enable Corfu globally
  (global-corfu-mode)

  :custom

  (corfu-auto nil)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-max-width 120)
  (corfu-on-exact-match 'nil)
  (corfu-popupinfo-delay '(0.3 . 0.1))
  (corfu-popupinfo-max-height 12)
  (corfu-preselect 'valid)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (global-corfu-minibuffer t)

  :bind

  (:map corfu-map
        ;; ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ;; ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ;; ("S-RET" . corfu-insert)
        ;; ("S-<return>" . corfu-insert)
        ;; ("RET" . nil)
        ))

;; Update Corfu history and sort completions by history
(use-package corfu-history :ensure nil
  :config
  (eon-add-to-list* 'savehist-additional-variables 'corfu-history)
  :hook
  (corfu-mode . corfu-history-mode))

;; Show candidate documentation in the echo area
(use-package corfu-echo :ensure nil
  :hook
  (corfu-mode . corfu-echo-mode))

;; _____________________________________________________________________________
(provide 'eon-corfu)
;;; eon-corfu.el ends here
