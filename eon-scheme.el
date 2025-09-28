;;; eon-scheme.el --- Scheme / Geiser -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-scheme.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(use-package geiser :ensure t
  :custom
  (geiser-repl-send-on-return-p t)
  (geiser-repl-use-other-window t)
  (scheme-mit-dialect nil)
  ;; Set Geiser's default implementation?
  (geiser-default-implementation 'guile))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; GEISER IMPLEMENTATION PACKAGES

;; Unquote and configure the corresponding package(s) for your Scheme(s) below,
;; and then then evaluate the expression(s) or restart Emacs.

;; <https://gitlab.com/emacs-geiser/chez>
;; (use-package geiser-chez :ensure t)

;; <https://gitlab.com/emacs-geiser/chicken>
;; (use-package geiser-chicken :ensure t)

;; <https://gitlab.com/emacs-geiser/chibi>
;; (use-package geiser-chibi :ensure t)

;; <https://gitlab.com/emacs-geiser/gambit>
;; (use-package geiser-gambit :ensure t)

;; <https://gitlab.com/emacs-geiser/gauche>
;; (use-package geiser-gauche :ensure t)

;; <https://gitlab.com/emacs-geiser/guile>
(use-package geiser-guile :ensure t)

;; <https://gitlab.com/emacs-geiser/kawa>
;; (use-package geiser-kawa)

;; <https://gitlab.com/emacs-geiser/mit>
;; (use-package geiser-mit :ensure t
;;   :config
;;   (use-package scheme
;;     :ensure nil
;;     :custom
;;     (scheme-mit-dialect t)))

;; <https://gitlab.com/emacs-geiser/racket>
;; (use-package geiser-racket :ensure t)

;; <https://gitlab.com/emacs-geiser/stklos>
;; (use-package geiser-stklos :ensure t)

;;  ____________________________________________________________________________
;;; SRFI BROWSER
;; <https://github.com/srfi-explorations/emacs-srfi>

(use-package srfi :ensure t)

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

(use-package smartparens :ensure t
  :when (featurep 'eon-smartparens)
  :hook
  (scheme-mode . smartparens-strict-mode)
  ((inferior-scheme-mode geiser-repl-mode) . smartparens-mode))

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
(provide 'eon-scheme)
;;; eon-scheme.el ends here
