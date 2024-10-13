;;; ontop-scheme.el --- Scheme with Geiser  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-scheme.el")'.

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
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(use-package geiser
  :ensure t
  :custom
  (geiser-repl-send-on-return-p t)
  (geiser-repl-use-other-window t)
  ;; Set Geiser's default implementation?
  ;; (geiser-default-implementation 'guile)
  )

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; GEISER IMPLEMENTATION PACKAGES

;; Unquote and configure the corresponding package(s) for your Scheme(s) below,
;; and then then evaluate the expression(s) or restart Emacs.

;; <https://gitlab.com/emacs-geiser/chez>
;; (use-package geiser-chez
;;   :ensure t)

;; <https://gitlab.com/emacs-geiser/chicken>
;; (use-package geiser-chicken
;;   :ensure t)

;; <https://gitlab.com/emacs-geiser/chibi>
;; (use-package geiser-chibi
;;   :ensure t)

;; <https://gitlab.com/emacs-geiser/gambit>
;; (use-package geiser-gambit
;;   :ensure t)

;; <https://gitlab.com/emacs-geiser/gauche>
;; (use-package geiser-gauche
;;   :ensure t)

;; <https://gitlab.com/emacs-geiser/guile>
;; (use-package geiser-guile
;;   :ensure t)

;; <https://gitlab.com/emacs-geiser/kawa>
;; (use-package geiser-kawa
;;   :ensure t)

;; <https://gitlab.com/emacs-geiser/mit>
;; (use-package geiser-mit
;;   :ensure t
;;   :config
;;   (use-package scheme
;;     :custom
;;     (scheme-mit-dialect t)))

;; <https://gitlab.com/emacs-geiser/racket>
;; (use-package geiser-racket
;;  :ensure t)

;; <https://gitlab.com/emacs-geiser/stklos>
;; (use-package geiser-stklos
;;   :ensure t)

;;  ____________________________________________________________________________
;;; SRFI BROWSER
;; <https://github.com/srfi-explorations/emacs-srfi>

(use-package srfi
  :ensure t)

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

;; Smartparens non-strict mode is already enabled globally
;; and configured in `ontop-core.el'

(use-package smartparens
  :ensure t
  :hook
  (scheme-mode . smartparens-strict-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `ontop-core.el'
(use-package rainbow-delimiters
  :ensure t
  :hook
  ((inferior-scheme-mode geiser-repl-mode) . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :ensure t
;;   :hook
;;   ((scheme-mode inferior-scheme-mode geiser-repl-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://www.orgmode.org/worg/org-contrib/babel/languages/ob-doc-scheme.html>

;; TODO This seems not to work; neither with Chicken nor Racket

;; Support literate programming in Emacs with Scheme
;; Evaluate Scheme code in Org blocks via "C-c C-c"
(use-package org
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 (add-to-list 'org-babel-load-languages '(scheme . t))))))

;;  ____________________________________________________________________________
(provide 'ontop-scheme)
;;; ontop-scheme.el ends here
