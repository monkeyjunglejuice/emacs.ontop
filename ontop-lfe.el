;;; ontop-lfe.el --- Lisp Flavoured Erlang  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-lfe.el")'.

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
;;; LFE
;; <https://github.com/lfe/lfe>
;; <https://github.com/lfe/rebar3>

(use-package lfe-mode
  :ensure t
  :config
  (setq inferior-lfe-check-if-rebar-project t)
  (setq inferior-lfe-indent-on-Cj t)
  :custom
  (inferior-lfe-program "lfe")
  (inferior-lfe-program-options '("-nobanner")))

;;  ____________________________________________________________________________
;;; ERLANG
;; <https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html>

(use-package erlang
  :defer t
  :ensure t)

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

;; Smartparens non-strict mode is already enabled globally
;; and configured in `ontop-core.el'

;; Enable strict mode in Lisp buffers
(use-package smartparens
  :ensure t
  :hook
  (lfe-mode . smartparens-strict-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `ontop-core.el'
(use-package rainbow-delimiters
  :ensure t
  :hook
  ((inferior-lfe-mode) . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :ensure t
;;   :hook
;;   ((lfe-mode inferior-lfe-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate LFE code in Org source code blocks via "C-c C-c"

;; TODO -- not working, package might be outdated
;; <https://github.com/zweifisch/ob-lfe>
;; (use-package ob-lfe
;;   :ensure t)

;; (use-package org
;;   :ensure nil
;;   :hook
;;   (org-mode . (lambda ()
;;                 (org-babel-do-load-languages
;;                  'org-babel-load-languages
;;                  (add-to-list 'org-babel-load-languages '(lfe . t))))))

;;  ____________________________________________________________________________
(provide 'ontop-lfe)
;;; ontop-lfe.el ends here
