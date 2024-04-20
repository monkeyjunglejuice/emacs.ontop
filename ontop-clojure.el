;;; ontop-clojure.el --- Clojure configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-clojure.el")'.

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
;;; CLOJURE MODE
;; <https://github.com/clojure-emacs/clojure-ts-mode>

(use-package clojure-ts-mode
  :ensure t
  :custom
  (clojure-ts-indent-style 'fixed)
  (clojure-ts-comment-macro-font-lock-body t)
  (clojure-ts-ensure-grammars t))

;;  ____________________________________________________________________________
;;; CIDER
;; <https://github.com/clojure-emacs/cider>

(use-package cider
  :ensure t
  :hook
  (clojure-ts-mode . cider-mode))

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

;; Smartparens is configured and enabled globally in `ontop-core.el'

;; Enable strict mode in Lisp buffers
(use-package smartparens
  :ensure t
  :hook
  (clojure-ts-mode . smartparens-strict-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Color-code nested parens
;; <https://github.com/Fanael/rainbow-delimiters>
(use-package rainbow-delimiters
  :ensure t
  :hook
  (cider-repl-mode . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :ensure t
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
(provide 'ontop-clojure)
;;; ontop-clojure.el ends here
