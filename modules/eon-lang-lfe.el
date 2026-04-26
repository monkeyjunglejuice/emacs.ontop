;;; eon-lang-lfe.el --- Lisp Flavoured Erlang -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience languages lfe
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

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
  (use-package smartparens :ensure t
    :config
    ;; As of version 20240713.1002, smartparens doesn't recognize LFE as a Lisp,
    ;; so let's add it manually
    (eon-add-to-list* 'sp-lisp-modes 'lfe-mode)
    (eon-add-to-list* 'sp-lisp-modes 'inferior-lfe-mode)))

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    (lfe-mode . aggressive-indent-mode)))

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
