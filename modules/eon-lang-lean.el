;;; eon-lang-lean.el --- Lean -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience languages lean
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; LEAN MODE
;; <https://codeberg.org/mekeor/nael>

(use-package nael :ensure t)

;; _____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-core.el'

(use-package eglot :ensure nil

  :custom

  ;; A longer timeout seems required for the first run in a new project
  (eglot-connect-timeout 60)  ; default: 30

  :config

  (add-to-list 'eglot-server-programs
               '(nael-mode . ("lake" "serve")))

  :hook

  ;; Start language server automatically
  (nael-mode . eglot-ensure)

  ;; Tell the language server to format the buffer before saving
  (nael-mode . (lambda ()
                 (add-hook 'before-save-hook
                           #'eglot-format-buffer nil 'local))))

;; _____________________________________________________________________________
(provide 'eon-lang-lean)
;;; eon-lang-lean.el ends here
