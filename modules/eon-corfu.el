;;; eon-corfu.el --- Code (auto-)completion -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; CORFU
;; <https://github.com/minad/corfu>
;; <https://github.com/minad/corfu/wiki>
;; <https://github.com/minad/corfu?tab=readme-ov-file#orderless-completion>

(use-package corfu :ensure t
  :init

  ;; Prevent *Completions* buffer from popping up
  ;; (setopt completion-auto-help nil)

  ;; Enable Corfu globally
  (global-corfu-mode)

  :custom

  ;; Autocompletion on?
  (corfu-auto t)
  (corfu-auto-prefix 2)

  ;; Enable Corfu completion in the minibuffer too?
  (global-corfu-minibuffer t)

  (corfu-cycle t)
  (corfu-count 12)
  (corfu-max-width 120)
  (corfu-scroll-margin 1)
  (corfu-preselect 'valid)
  (corfu-preview-current nil)
  (corfu-on-exact-match 'nil)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)

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

;; Show candidate autodoc in the echo area
(use-package corfu-echo :ensure nil
  :custom
  (corfu-echo-delay '(0.5 . 0.2))
  :hook
  (corfu-mode . corfu-echo-mode))

;; To display documentation in a popup overlay only via "M-h" on demand,
;; it's initial timer is set to 60 seconds.
(use-package corfu-popupinfo :ensure nil
  :custom
  (corfu-popupinfo-delay '(60.0 . 0.1))
  (corfu-popupinfo-max-height 10)
  :hook
  (corfu-mode . corfu-popupinfo-mode))

;; _____________________________________________________________________________
(provide 'eon-corfu)
;;; eon-corfu.el ends here
