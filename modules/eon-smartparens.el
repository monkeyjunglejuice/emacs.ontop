;;; eon-smartparens.el --- Edit parenthesis structurally -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience
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
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

(use-package smartparens :ensure t

  :init

  ;; Disable conflicting modes
  (electric-pair-mode -1)
  (show-paren-mode -1)

  :config

  ;; Enable language-specific configurations
  (require 'smartparens-config)

  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)

  ;; Only use the pseudo-quote inside strings where it serves as hyperlink
  (sp-with-modes 'emacs-lisp-mode
                 (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))

  ;; Minibuffer
  (defun eon-smartparens-minibuffer-setup ()
    "Enable `smartparens-mode' in the minibuffer during `eval-expression'."
    (sp-local-pair 'minibuffer-pairs "'" nil :actions nil)
    (sp-local-pair 'minibuffer-pairs "`" nil :actions nil)
    (sp-update-local-pairs 'minibuffer-pairs)
    (smartparens-mode 1))

  :hook

  (eval-expression-minibuffer-setup . eon-smartparens-minibuffer-setup)

  :bind

  ;; Custom keybinding set, a blend of standard Emacs sexp keybindings
  ;; and Paredit keybindings
  (:map smartparens-mode-map
        ;; Navigation
        ("C-M-f"           . sp-forward-sexp)
        ("C-M-b"           . sp-backward-sexp)
        ("C-M-u"           . sp-up-sexp)
        ("C-M-d"           . sp-down-sexp)
        ("C-M-a"           . sp-beginning-of-sexp)
        ("C-M-e"           . sp-end-of-sexp)
        ;; Depth-changing commands
        ("C-M-S-u"         . sp-unwrap-sexp)
        ("C-M-S-s"         . sp-splice-sexp)
        ;; Forward slurp/barf
        ("C-M-)"           . sp-forward-slurp-sexp)
        ("C-M-}"           . sp-forward-barf-sexp)
        ;; Backward slurp/barf
        ("C-M-("           . sp-backward-slurp-sexp)
        ("C-M-{"           . sp-backward-barf-sexp)
        ;; Misc
        ("C-M-k"           . sp-kill-sexp)
        ("C-M-DEL"         . sp-backward-kill-sexp)
        ("C-M-SPC"         . sp-mark-sexp)
        ("C-M-@"           . sp-mark-sexp)
        ("C-M-w"           . sp-copy-sexp)
        ("C-M-t"           . sp-transpose-sexp)))

;; _____________________________________________________________________________
(provide 'eon-smartparens)
;;; eon-smartparens.el ends here
