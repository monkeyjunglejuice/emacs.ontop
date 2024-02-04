;;; ontop-corfu.el --- Auto-completion  -*- lexical-binding: t; -*-
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
  ;; Enable Corfu globally. This is recommended since Dabbrev
  ;; can be used globally (M-/). See also `corfu-excluded-modes'.
  (global-corfu-mode)
  ;; Optional customizations
  :custom
  ;; Enable auto completion
  (corfu-auto t)
  ;; Enable cycling for `corfu-next/previous'
  (corfu-cycle t))

;;  ____________________________________________________________________________
;;; EMACS (built-in)

;; A few more useful configurations
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation + completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Use Dabbrev with Corfu
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dynamic-Abbrevs>
(use-package dabbrev
  :ensure nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package eshell
  :ensure nil
  :hook
  ;; Behave more like shell autocompletion
  (eshell-mode . (lambda ()
                   (setq-local corfu-auto nil))))

;;; ___________________________________________________________________________
(provide 'ontop-corfu)
;;; ontop-corfu.el ends here
