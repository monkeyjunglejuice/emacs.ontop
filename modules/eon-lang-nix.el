;;; eon-lang-nix.el --- Nix -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience languages lean
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
;;; NIX MODE
;; <https://github.com/nix-community/nix-ts-mode>

(use-package nix-ts-mode :ensure t
  :init
  (eon-treesitter-ensure-grammar
   '(nix "https://github.com/nix-community/tree-sitter-nix"))
  :mode "\\.nix\\'")

;; _____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>

(use-package eglot :ensure nil

  :custom

  ;; A longer timeout seems required for the first run in a new project
  (eglot-connect-timeout 60)  ; default: 30

  :config

  (add-to-list 'eglot-server-programs
               '(nix-ts-mode . ("nil")))

  :hook

  ;; Start language server automatically
  (nix-ts-mode . eglot-ensure)

  ;; Tell the language server to format the buffer before saving
  (nix-ts-mode . (lambda ()
                   (add-hook 'before-save-hook
                             #'eglot-format-buffer nil 'local))))

;; _____________________________________________________________________________
(provide 'eon-lang-nix)
;;; eon-lang-nix.el ends here
