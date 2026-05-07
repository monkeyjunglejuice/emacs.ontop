;;; eon-paredit.el --- Edit Lisp code structurally -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.1
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
;; Paredit keeps your parentheses balanced while editing.
;;
;;; Code:

(eon-module-metadata
 :conflicts '(eon-smartparens)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; PAREDIT
;; <https://paredit.org/cgit/paredit>
;; <https://emacsrocks.com/e14.html>
;; <http://danmidwood.com/content/2014/11/21/animated-paredit.html>

(use-package paredit :ensure t
  :diminish paredit-mode

  :init

  ;; Enable Paredit in all known Lisp source code buffers
  (mapc (lambda (mode) (add-hook mode #'paredit-mode))
        (eon-lisp-src-modes 'hook))

  ;; Enable Paredit in all known Lisp REPLs
  (mapc (lambda (mode) (add-hook mode #'paredit-mode))
        (eon-lisp-src-modes 'hook))

  :hook

  ;; Enable Paredit in the minibuffer
  (eval-expression-minibuffer-setup . paredit-mode)

  ;; Disable conflicting modes
  (paredit-mode . (lambda ()
                    (electric-indent-mode -1)
                    (electric-pair-mode -1)))

  :bind

  (:map paredit-mode-map
        ;; Change keybindings clashing with Emacs default keybindings;
        ;; <https://emacsredux.com/blog/2026/03/27/paredit-keybinding-conflicts>
        ;; Don't shadow `search-map' prefix
        ("M-s"   . nil)
        ("M-D"   . paredit-splice-sexp)
        ;; Don't shadow `xref-find-references'
        ("M-?"   . nil)
        ;; Alternative forward slurp/barf keybindings
        ("C-M-)" . paredit-forward-slurp-sexp)
        ("C-M-}" . paredit-forward-barf-sexp)
        ;; Alternative backward slurp/barf keybindings
        ("C-M-(" . paredit-backward-slurp-sexp)
        ("C-M-{" . paredit-backward-barf-sexp)))

;; _____________________________________________________________________________
(provide 'eon-paredit)
;;; eon-paredit.el ends here
