;;; eon-todo.el --- Highlight todo keywords in comments -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; HIGHLIGHT KEYWORDS IN COMMENTS
;; <https://github.com/tarsius/hl-todo>

(use-package hl-todo :ensure t
  :config
  (global-hl-todo-mode))

(when (eon-modulep 'eon-consult)
  (use-package consult-todo :ensure t
    :after (hl-todo consult)
    :bind
    (:map ctl-z-g-map
          ("t" . consult-todo)
          ("T" . consult-todo-all))))

;;  ____________________________________________________________________________
(provide 'eon-todo)
;;; eon-todo.el ends here
