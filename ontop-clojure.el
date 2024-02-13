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
;; <https://github.com/clojure-emacs/clojure-mode/>

(use-package clojure-mode
  :ensure t)

;;  ____________________________________________________________________________
;;; CIDER
;; <https://github.com/clojure-emacs/cider>

(use-package cider
  :ensure t)

;;  ____________________________________________________________________________
;;; INDENTATION
;; <https://github.com/Malabarba/aggressive-indent-mode>

;; Reindent immediately after change
(use-package aggressive-indent
  :ensure t
  :hook
  ((clojure-mode clojurescript-mode clojurec-mode) . aggressive-indent-mode))

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
