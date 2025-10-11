;;; eon-lang-clojure.el --- Clojure -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; CLOJURE MODE
;; <https://github.com/clojure-emacs/clojure-ts-mode>

(use-package clojure-ts-mode :ensure t
  :init (eon-treesitter-ensure-grammar
         '(clojure "https://github.com/sogaiu/tree-sitter-clojure"))
  :custom
  (clojure-ts-indent-style 'fixed)
  (clojure-ts-comment-macro-font-lock-body t)
  (clojure-ts-ensure-grammars t))

;;  ____________________________________________________________________________
;;; CIDER
;; <https://github.com/clojure-emacs/cider>

(use-package cider :ensure t
  :hook
  (clojure-ts-mode . cider-mode))

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>
(when (eon-modulep 'eon-smartparens)
  (use-package smartparens :ensure t
    :hook
    (clojure-ts-mode . smartparens-strict-mode)))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters :ensure t
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
(use-package org :ensure nil
  
  :config
  (add-to-list 'org-babel-load-languages '(clojure . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

;; _____________________________________________________________________________
(provide 'eon-lang-clojure)
;;; eon-lang-clojure.el ends here
