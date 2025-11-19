;;; eon-lang-lua.el --- Lua -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; LUA TS MODE

(use-package lua-ts-mode :ensure nil
  :defer t
  :init
  (eon-treesitter-ensure-grammar
   '(lua "https://github.com/tree-sitter-grammars/tree-sitter-lua"))
  (add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))
  :mode "\\.lua\\'"
  :interpreter "lua"
  :bind
  (:map lua-ts-mode-map
        ("C-c C-z" . lua-ts-inferior-lua)))

;; _____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon.el'

(use-package eglot :ensure nil
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 ;; <https://github.com/LuaLS/lua-language-server/wiki>
                 `((lua-ts-mode) . ,(eglot-alternatives
                                     '(("lua-language-server"))))))
  :hook
  ((lua-ts-mode) . eglot-ensure))

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
