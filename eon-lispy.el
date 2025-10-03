;;; eon-evil.el --- Structural editing -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop
;;
;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;; LISPYVILLE
;; <https://github.com/noctuid/lispyville>

(use-package lispyville :ensure t
  :when (not (featurep 'eon-smartparens))
  :diminish
  :init
  (setq lispyville-key-theme
        '(operators
          c-w
          (prettify insert)
          (atom-movement t)
          slurp/barf-lispy
          additional
          additional-insert))
  :config
  (lispyville-set-key-theme)
  :hook
  ((prog-mode conf-mode) . lispyville-mode))

;;  ____________________________________________________________________________
(provide 'eon-lispy)
;;; eon-lispy.el ends here
