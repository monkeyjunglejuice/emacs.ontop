;;; eon-lang-unison.el --- Unison -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience languages unison
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; UNISON TS MODE
;; <https://github.com/fmguerreiro/unison-ts-mode>
;; <https://github.com/fmguerreiro/tree-sitter-unison>

(use-package unison-ts-mode :ensure t

  :init

  (eon-localleader-defkeymap unison-ts-mode eon-localleader-unison-scratch-map
    :doc "Local leader keymap for Unison scratch buffer.")

  :mode

  ("\\.u\\'" "\\.unison\\'")

  :custom

  (unison-ts-grammar-install 'auto)
  (unison-ts-grammar-repository
   "https://github.com/fmguerreiro/tree-sitter-unison")

  :bind

  (:map eon-localleader-unison-scratch-map
        ("a" . unison-ts-add)
        ("g" . unison-ts-repl)
        ("r" . unison-ts-run)
        ("l" . unison-ts-load)
        ("t" . unison-ts-test)
        ("w" . unison-ts-watch)
        ("u" . unison-ts-update)
        ("e" . unison-ts-send-region)
        ("d" . unison-ts-send-definition)))

;; _____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in './eon-core.el'

(use-package eglot :ensure nil

  :custom

  ;; A longer timeout may be required for the first run in a new project
  (eglot-connect-timeout 60)  ; default: 30

  ;; Unison LSP currently does not handle semantic token requests
  (eglot-ignored-server-capabilities '(:semanticTokensProvider))

  :config

  ;; Set up the Unison-specific eglot integration once
  (unison-ts-mode-setup-eglot)

  :hook

  ;; Start language server automatically
  (unison-ts-mode . eglot-ensure)

  ;; Tell the language server to format the buffer before saving
  (unison-ts-mode . (lambda ()
                      (add-hook 'before-save-hook
                                #'eglot-format-buffer nil 'local))))

;; _____________________________________________________________________________
(provide 'eon-lang-unison)
;;; eon-lang-unison.el ends here
