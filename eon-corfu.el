;;; eon-corfu.el --- Code (auto)completion -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; CORFU
;; <https://github.com/minad/corfu>

;; (use-package corfu :ensure t
;;   :init
;;   ;; Disable Emacs ONBOARD standard first
;;   (global-completion-preview-mode -1)
;;   :custom
;;   (global-corfu-minibuffer t)
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.2)
;;   (corfu-popupinfo-delay '(0.3 . 0.1))
;;   (corfu-popupinfo-max-height 10)
;;   (corfu-quit-at-boundary nil)
;;   (corfu-quit-no-match 'separator)
;;   (corfu-preview-current nil)
;;   (corfu-preselect 'valid)
;;   (corfu-on-exact-match 'nil)
;;   :config
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   ;; (corfu-popupinfo-mode)
;;   :bind
;;   (:map corfu-map
;;         ;; ("TAB" . corfu-next)
;;         ;; ([tab] . corfu-next)
;;         ;; ("S-TAB" . corfu-previous)
;;         ;; ([backtab] . corfu-previous)
;;         ;; ("S-RET" . corfu-insert)
;;         ;; ("S-<return>" . corfu-insert)
;;         ("RET" . nil)))

;; TODO Corfu config needs an overhaul
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
