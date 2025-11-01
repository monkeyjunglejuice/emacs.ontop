;;; eon-lang-clojure.el --- Clojure -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; CLOJURE MODE
;; <https://github.com/clojure-emacs/clojure-ts-mode>

(use-package clojure-ts-mode :ensure t
  :init (eon-treesitter-ensure-grammar
         '(clojure "https://github.com/sogaiu/tree-sitter-clojure"))
  :custom
  (clojure-ts-indent-style 'fixed)
  (clojure-ts-comment-macro-font-lock-body t)
  (clojure-ts-ensure-grammars t))

;; _____________________________________________________________________________
;;; CIDER
;; <https://github.com/clojure-emacs/cider>

(use-package cider :ensure t
  :hook
  (clojure-ts-mode . cider-mode))

;; _____________________________________________________________________________
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
