;;; eon-lang-lua.el --- Lua -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience languages lua
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
;;; LUA TS MODE

(use-package lua-ts-mode :ensure nil
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua"

  :init

  (eon-treesitter-ensure-grammar
   '(lua "https://github.com/tree-sitter-grammars/tree-sitter-lua"))
  (add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))

  :bind

  (:map lua-ts-mode-map
        ("C-c C-z" . lua-ts-inferior-lua)))

;; _____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon.el'

;; <https://github.com/LuaLS/lua-language-server>
;; <https://github.com/LuaLS/lua-language-server/wiki>

(use-package eglot :ensure nil

  :config

  (add-to-list 'eglot-server-programs
               `(lua-ts-mode . ,(eglot-alternatives
                                 '(("lua-language-server")))))

  :hook

  (lua-ts-mode . eglot-ensure))

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    (lua-ts-mode . aggressive-indent-mode)))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate Lua code in Org source code blocks via "C-c C-c".

(use-package ob-lua :ensure nil
  :after org)

;; _____________________________________________________________________________
(provide 'eon-lang-lua)
;;; eon-lang-lua.el ends here
