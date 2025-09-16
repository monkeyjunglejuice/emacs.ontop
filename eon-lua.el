;;; eon-lua.el --- Lua configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-lua.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; LUA TS MODE

(eon-treesitter-ensure-grammar
 '(lua "https://github.com/tree-sitter-grammars/tree-sitter-lua"))

(use-package lua-ts-mode :ensure nil
  :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))
  :mode "\\.lua\\'"
  :interpreter "lua"
  :bind
  (:map lua-ts-mode-map
        ("C-c C-z" . lua-ts-inferior-lua)))

;;  ____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-eglot.el'

(use-package eglot :ensure nil
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 ;; <https://github.com/LuaLS/lua-language-server/wiki>
                 `((lua-ts-mode) . ,(eglot-alternatives
                                     '(("lua-language-server"))))))
  :hook
  ((lua-ts-mode) . eglot-ensure))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate Lua code in Org source code blocks via "C-c C-c".

;; <https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lua.html>
(use-package org :ensure nil
  :config
  (add-to-list 'org-babel-load-languages '(lua . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

;;  ____________________________________________________________________________
(provide 'eon-lua)
;;; eon-lua.el ends here
