;;; eon-lfe.el --- Lisp Flavoured Erlang  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-lfe.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; LFE
;; <https://github.com/lfe/lfe>
;; <https://github.com/lfe/rebar3>

;; Personal Elisp directory
(add-to-list 'load-path (expand-file-name "~/code/lfe/emacs/"))
(require 'lfe-start)

(use-package inferior-lfe
  :load-path "~/code/lfe/emacs/"
  :custom
  (inferior-lfe-program "lfe")
  (inferior-lfe-program-options '("-nobanner"))
  :config
  (setq inferior-lfe-check-if-rebar-project t)
  (setq inferior-lfe-indent-on-Cj t)
  (setq comint-process-echoes nil))

(use-package lfe-indent
  :load-path "~/code/lfe/emacs/")

(use-package lfe-mode
  :load-path "~/code/lfe/emacs/")

(use-package lfe-start
  :load-path "~/code/lfe/emacs/"
  :requires (inferior-lfe lfe-indent lfe-mode))

;;  ____________________________________________________________________________
;;; ERLANG
;; <https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html>

(use-package erlang
  :defer t)

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

;; Smartparens non-strict mode is already enabled globally
;; and configured in `eon-core.el'

;; Enable strict mode in Lisp buffers
(use-package smartparens
  :config
  ;; As of version 20240713.1002, smartparens doesn't recognize LFE as a lisp,
  ;; so let's add it manually
  (add-to-list 'sp-lisp-modes 'lfe-mode)
  (add-to-list 'sp-lisp-modes 'inferior-lfe-mode)
  :hook
  (lfe-mode . smartparens-strict-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters
  :hook
  (inferior-lfe-mode . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   ((lfe-mode inferior-lfe-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate LFE code in Org source code blocks via "C-c C-c"

;; TODO -- not working, package might be outdated
;; <https://github.com/zweifisch/ob-lfe>
;; (use-package ob-lfe)

;; (use-package org
;;   :ensure nil
;;   :hook
;;   (org-mode . (lambda ()
;;                 (org-babel-do-load-languages
;;                  'org-babel-load-languages
;;                  (add-to-list 'org-babel-load-languages '(lfe . t))))))

;;  ____________________________________________________________________________
(provide 'eon-lfe)
;;; eon-lfe.el ends here
