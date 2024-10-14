;;; ontop-corfu.el --- Code-completion  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;; <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package nil))

(eval-when-compile
  (require 'use-package))

;;  ____________________________________________________________________________
;;; CORFU
;; <https://github.com/minad/corfu>

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.1))
  (corfu-popupinfo-max-height 10)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-preselect 'valid)
  (corfu-on-exact-match 'nil)
  :hook
  ((eat-eshell-mode eshell-mode shell-mode) . corfu-mode)
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
;;; CAPE
;; <https://github.com/minad/cape>

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Continuously update the candidates - deactivate if lagging
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; ;; Compose own completion at point for Eglot
  ;; (defun eon-eglot-capf-super ()
  ;;   (setq-local completion-at-point-functions
  ;;               (list (cape-capf-super
  ;;                      #'eglot-completion-at-point
  ;;                      #'cape-file))))
  ;; :hook
  ;; (eglot-managed-mode . #'eon-eglot-capf-super)
  :bind
  ("C-c p f" . cape-file))

;;  ____________________________________________________________________________
;;; YASNIPPET

;; <https://github.com/joaotavora/yasnippet>
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :config
  (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand)
  :hook
  ((prog-mode text-mode) . yas-minor-mode))

;; <https://github.com/AndreaCrotti/yasnippet-snippets>
(use-package yasnippet-snippets
  :ensure t
  :defer t)

;; <https://github.com/elken/yasnippet-capf>
;; (use-package yasnippet-capf
;;   :ensure t
;;   :after cape
;;   :config
;;   (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;;  ____________________________________________________________________________
;;; EMACS (built-in)

;; A few more useful configurations
(use-package emacs
  :ensure nil
  :init
  ;; Enable indentation + completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Use Dabbrev with Corfu
(use-package dabbrev
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;;; ___________________________________________________________________________
(provide 'ontop-corfu)
;;; ontop-corfu.el ends here
