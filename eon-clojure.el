;;; eon-clojure.el --- Clojure configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-clojure.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; CLOJURE MODE
;; <https://github.com/clojure-emacs/clojure-ts-mode>

(use-package clojure-ts-mode
  :custom
  (clojure-ts-indent-style 'fixed)
  (clojure-ts-comment-macro-font-lock-body t)
  (clojure-ts-ensure-grammars t))

;;  ____________________________________________________________________________
;;; CIDER
;; <https://github.com/clojure-emacs/cider>

(use-package cider
  :hook
  (clojure-ts-mode . cider-mode))

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

;; Smartparens non-strict mode is already enabled globally
;; and configured in `eon-core.el'

;; Enable strict mode in Lisp buffers
(use-package smartparens
  :hook
  (clojure-ts-mode . smartparens-strict-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters
  :hook
  (cider-repl-mode . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   ((clojure-ts-mode cider-repl-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs

;; Evaluate Clojure code in Org source code blocks via "C-c C-c"
(use-package org
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 (add-to-list 'org-babel-load-languages '(clojure . t))))))

;; _____________________________________________________________________________
(provide 'eon-clojure)
;;; eon-clojure.el ends here
