;;; eon-lua.el --- Lua configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-lua.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; LUA TS MODE

(use-package lua-ts-mode :ensure nil
  :custom
  (lua-default-application "lua")
  (lua-default-command-switches '("-i"))
  (lua-documentation-url "http://www.lua.org/manual/5.4/manual.html")
  :bind
  (:map lua-mode-map
        ("C-c C-b" . lua-send-buffer)
        ("C-c C-l" . lua-send-current-line)
        ("C-c C-r" . lua-send-region)
        ("C-c C-d" . lua-send-defun)
        ("C-c C-s" . lua-send-string)
        ("C-M-f" . lua-forward-sexp)
        ("C-M-b" . lua-backwards-to-block-begin-or-end)))

;;  ____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-eglot.el'

(use-package eglot :ensure nil
  :hook
  (lua-ts-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 ;; <https://github.com/LuaLS/lua-language-server/wiki>
                 `(lua-ts-mode . ,(eglot-alternatives
                                   '(("lua-language-server")))))))

;;  ____________________________________________________________________________
;;; SYNTAX-CHECKER / LINTER
;; <https://www.gnu.org/software/emacs/manual/html_mono/flymake.html>
;; Depends on luacheck: `luarocks --local install luacheck'

(use-package flymake :ensure nil
  :hook
  (lua-ts-mode . flymake-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate Lua code in Org source code blocks via "C-c C-c".

;; <https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lua.html>
(use-package org :ensure nil
  :hook
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 (add-to-list 'org-babel-load-languages '(lua . t))))))

;;  ____________________________________________________________________________
(provide 'eon-lua)
;;; eon-lua.el ends here
