;;; eon-lang-scheme.el --- Scheme / Geiser -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Common definitions and functionality for all Scheme implementation modules.
;; This module will be loaded automatically by the implementation-specific
;; Scheme modules, even if commented out in `eon-setup-modules'.
;; 
;;; Code:

;;  ____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(use-package geiser :ensure t
  :custom
  (geiser-repl-send-on-return-p t)
  (geiser-repl-use-other-window t)
  (scheme-mit-dialect nil))

;;  ____________________________________________________________________________
;;; SRFI BROWSER
;; <https://github.com/srfi-explorations/emacs-srfi>

(use-package srfi :ensure t)

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

(when (eon-modulep 'eon-smartparens)
  (use-package smartparens :ensure t
    :hook
    (scheme-mode . smartparens-strict-mode)
    ((inferior-scheme-mode geiser-repl-mode) . smartparens-mode)))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters :ensure t
  :hook
  ((inferior-scheme-mode geiser-repl-mode) . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   ((scheme-mode inferior-scheme-mode geiser-repl-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate Scheme code in Org source code blocks via "C-c C-c"

;; TODO This seems not to work; neither with Chicken nor Racket

;; <https://www.orgmode.org/worg/org-contrib/babel/languages/ob-doc-scheme.html>
;; (use-package org :ensure nil
;;   :config
;;   (add-to-list 'org-babel-load-languages '(scheme . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                org-babel-load-languages))

;;  ____________________________________________________________________________
(provide 'eon-lang-scheme)
;;; eon-lang-scheme.el ends here
