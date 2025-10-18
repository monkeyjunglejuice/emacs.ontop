;;; eon-lispy.el --- Structural editing -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; LISPY
;; <https://github.com/abo-abo/lispy>

;; Not required for `evil-mode', `lispyville' will be loaded instead
(when (not (eon-modulep 'eon-evil))
  (use-package lispy :ensure t
    :config
    ;; Compatibility with other modes
    (when (eon-modulep 'eon-god)
      (eon-add-to-list-setopt 'lispy-compat 'god-mode))
    ;; Activate Lispy-mode for all known Lisp source code modes
    (mapc (lambda (mode)
            (add-hook mode #'lispy-mode))
          (eon-lisp-src-modes 'hook))))

;; _____________________________________________________________________________
;; LISPYVILLE
;; <https://github.com/noctuid/lispyville>

(when (eon-modulep 'eon-evil)
  (use-package lispyville :ensure t
    :diminish
    :after evil
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
    ((prog-mode conf-mode) . lispyville-mode)))

;; _____________________________________________________________________________
(provide 'eon-lispy)
;;; eon-lispy.el ends here
