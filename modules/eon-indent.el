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

(use-package aggressive-indent :ensure t
  :diminish aggressive-indent-mode
  :custom
  ;; Actually seems to work better when `electric-indent-mode' is enabled
  (aggressive-indent-dont-electric-modes nil)
  (aggressive-indent-sit-for-time 0.01)
  :hook
  ;; Don't use `aggressive-indent-global-mode'; it causes to weird problems
  (prog-mode . aggressive-indent-mode))

;; _____________________________________________________________________________
(provide 'eon-indent)
;;; eon-indent.el ends here
