;;; eon-vertico.el --- Vertical minibuffer completion UI -*- lexical-binding: t; no-byte-compile: t; -*-

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
;;; VERTICO
;; <https://github.com/minad/vertico>
;; <https://github.com/minad/vertico/wiki>
;; <https://github.com/minad/vertico#key-bindings>

(use-package vertico :ensure t
  :init

  ;; Disable Emacs ONBOARD default completion UIs
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)

  ;; Enable Vertico
  (vertico-mode)
  ;; How to display Vertico per default?
  (vertico-multiform-mode)

  :custom

  ;; Display certain listings in another form?
  (vertico-multiform-commands '((consult-imenu buffer)
                                (consult-imenu-multi buffer)
                                (consult-outline buffer)
                                (consult-grep buffer)
                                (consult-ripgrep buffer)))

  ;; In the minibuffer
  (vertico-count 12)
  (vertico-resize 'grow-only)

  ;; Enable cycling for `vertico-next' and `vertico-previous'?
  (vertico-cycle nil)

  :config

  ;; Enable indexed candidates, select via "M-<number> RET"
  ;; (vertico-indexed-mode)

  ;; Save `vertico-repeat' history across Emacs sessions
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)

  :hook

  ;; Enable repeatable Vertico sessions
  (minibuffer-setup . vertico-repeat-save)

  :bind

  (:map ctl-z-map
        ("." . vertico-repeat))
  (:map vertico-map
        ("M-P" . vertico-repeat-previous)
        ("M-N" . vertico-repeat-next)
        ("M-g" . vertico-quick-jump)
        ("M-i" . vertico-quick-insert)
        ("M-j" . vertico-quick-exit)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; EMACS (built-in)

;; A few more useful configurations
(use-package emacs :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;; _____________________________________________________________________________
(provide 'eon-vertico)
;;; eon-vertico.el ends here
