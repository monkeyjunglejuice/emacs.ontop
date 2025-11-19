;;; eon-lang-lfe.el --- Lisp Flavoured Erlang -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; LFE
;; <https://github.com/lfe/lfe>
;; <https://github.com/lfe/rebar3>

(use-package lfe-mode :ensure t
  :custom
  (inferior-lfe-program "lfe")
  (inferior-lfe-program-options '("-nobanner"))
  :config
  (setq inferior-lfe-check-if-rebar-project t)
  (setq inferior-lfe-indent-on-Cj t))

;; _____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

(when (eon-modulep 'eon-smartparens)
  (use-package smartparens
    :config
    ;; As of version 20240713.1002, smartparens doesn't recognize LFE as a Lisp,
    ;; so let's add it manually
    (eon-add-to-list* 'sp-lisp-modes 'lfe-mode)
    (eon-add-to-list* 'sp-lisp-modes 'inferior-lfe-mode)))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate LFE code in Org source code blocks via "C-c C-c"

;; TODO Doesn't work, package might be outdated
;; <https://github.com/zweifisch/ob-lfe>
(use-package ob-lfe :ensure t
  :after org)

;; _____________________________________________________________________________
(provide 'eon-lang-lfe)
;;; eon-lang-lfe.el ends here
