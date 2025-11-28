;;; eon-indent.el --- Indent code immediately -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;; AGGRESSIVE INDENT
;; <https://github.com/Malabarba/aggressive-indent-mode>

;; TODO Aggressive-indent causes weird issues, either fix them or switch
;; to another package, e.g. <https://github.com/jeffvalk/snap-indent>

;; Aggressive indent shouldn't be enabled globally via
;; `aggressive-indent-global-mode' or via hook for `prog-mode', but individually
;; per major mode.

(use-package aggressive-indent :ensure t
  :diminish aggressive-indent-mode
  :custom
  ;; Actually seems to work better when `electric-indent-mode' is enabled
  (aggressive-indent-dont-electric-modes nil)
  (aggressive-indent-sit-for-time 0.01))

;; _____________________________________________________________________________
(provide 'eon-indent)
;;; eon-indent.el ends here
