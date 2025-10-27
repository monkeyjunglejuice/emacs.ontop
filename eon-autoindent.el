;;; eon-autoindent.el --- Indent code immediately -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;; AGGRESSIVE INDENT
;; <https://github.com/Malabarba/aggressive-indent-mode>

(use-package aggressive-indent :ensure t
  :diminish aggressive-indent-mode
  :init
  ;; Enable auto-indentation
  (global-aggressive-indent-mode 1)
  :custom
  (aggressive-indent-dont-electric-modes t)
  (aggressive-indent-sit-for-time 0.05))

;;  ____________________________________________________________________________
(provide 'eon-autoindent)
;;; eon-autoindent.el ends here
