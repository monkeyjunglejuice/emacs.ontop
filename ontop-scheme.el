;;; ontop-scheme.el --- Scheme with Geiser  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-scheme.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(use-package geiser
  :custom
  (geiser-repl-send-on-return-p t)
  (geiser-repl-use-other-window nil)
  (scheme-mit-dialect nil)
  ;; Set Geiser's default implementation?
  ;; (geiser-default-implementation 'guile)
  )

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; GEISER IMPLEMENTATION PACKAGES

;; Unquote and configure the corresponding package(s) for your Scheme(s) below,
;; and then then evaluate the expression(s) or restart Emacs.

;; <https://gitlab.com/emacs-geiser/chez>
;; (use-package geiser-chez)

;; <https://gitlab.com/emacs-geiser/chicken>
;; (use-package geiser-chicken)

;; <https://gitlab.com/emacs-geiser/chibi>
;; (use-package geiser-chibi)

;; <https://gitlab.com/emacs-geiser/gambit>
;; (use-package geiser-gambit)

;; <https://gitlab.com/emacs-geiser/gauche>
;; (use-package geiser-gauche)

;; <https://gitlab.com/emacs-geiser/guile>
(use-package geiser-guile)

;; <https://gitlab.com/emacs-geiser/kawa>
;; (use-package geiser-kawa)

;; <https://gitlab.com/emacs-geiser/mit>
;; (use-package geiser-mit
;;   :config
;;   (use-package scheme
;;     :custom
;;     (scheme-mit-dialect t)))

;; <https://gitlab.com/emacs-geiser/racket>
;; (use-package geiser-racket)

;; <https://gitlab.com/emacs-geiser/stklos>
;; (use-package geiser-stklos)

;;  ____________________________________________________________________________
;;; SRFI BROWSER
;; <https://github.com/srfi-explorations/emacs-srfi>

(use-package srfi)

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

;; Smartparens non-strict mode is already enabled globally
;; and configured in `ontop-core.el'

(use-package smartparens
  :hook
  (scheme-mode . smartparens-strict-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `ontop-core.el'
(use-package rainbow-delimiters
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
(use-package org
  :hook
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 (add-to-list 'org-babel-load-languages '(scheme . t))))))

;;  ____________________________________________________________________________
(provide 'ontop-scheme)
;;; ontop-scheme.el ends here
