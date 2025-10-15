;;; eon-corfu.el --- Code (auto)completion -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; CORFU
;; <https://github.com/minad/corfu>

(use-package corfu :ensure t
  :init
  (global-completion-preview-mode -1)  ; Disable Emacs ONBOARD standard
  :custom
  (global-corfu-minibuffer t)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.3 . 0.1))
  (corfu-popupinfo-max-height 10)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-preselect 'valid)
  (corfu-on-exact-match 'nil)
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  ;; ((eat-eshell-mode eshell-mode shell-mode) . corfu-mode)
  :bind
  (:map corfu-map
        ;; ("TAB" . corfu-next)
        ;; ([tab] . corfu-next)
        ;; ("S-TAB" . corfu-previous)
        ;; ([backtab] . corfu-previous)
        ;; ("S-RET" . corfu-insert)
        ;; ("S-<return>" . corfu-insert)
        ("RET" . nil)))

;;  ____________________________________________________________________________
(provide 'eon-corfu)
;;; eon-corfu.el ends here
