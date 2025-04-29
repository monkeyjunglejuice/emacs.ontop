;;; eon-erlang.el --- Erlang configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-erlang.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; ERLANG MODE
;; <https://github.com/erlang/otp/tree/master/lib/tools/emacs>
;; <https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html>

(use-package erlang)

;; In order to use Tree Sitter, install the tree-sitter binary with your
;; OS package manager. Then install the language grammar via
;; 'M-x treesit-install-language-grammar'
(use-package treesit
  :ensure nil
  :config
  (add-to-list 'treesit-language-source-alist
               '(erlang "https://github.com/WhatsApp/tree-sitter-erlang")))

;;  ____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-core.el'

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ERLANG_LS
;; <https://github.com/erlang-ls/erlang_ls>

(use-package eglot
  :ensure nil
  :custom
  ;; A longer timeout seems required for the first run in a new project
  (eglot-connect-timeout 30)            ; default: 30
  :config
  ;; Make sure to adapt the path and use the .bat script for Windows
  (add-to-list 'eglot-server-programs
               '((erlang-mode) . ("erlang_ls")))
  :hook
  ;; Start language server automatically
  (erlang-mode . eglot-ensure)
  ;; Tell the language server to format the buffer before saving
  (erlang-mode . (lambda ()
                   (add-hook 'before-save-hook
                             #'eglot-format-buffer nil 'local))))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters
  :hook
  (erlang-shell-mode . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   ((erlang-mode erlang-shell-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs
;; Evaluate Erlang code in Org source code blocks via "C-c C-c"

;; Only available from Github, therefore not installed
;; <https://github.com/xfwduke/ob-erlang>
;; (use-package ob-erlang)

;; (use-package org
;;   :hook
;;   (org-mode . (lambda ()
;;                 (org-babel-do-load-languages
;;                  'org-babel-load-languages
;;                  (add-to-list 'org-babel-load-languages '(erlang . t))))))

;; _____________________________________________________________________________
(provide 'eon-erlang)
;;; eon-erlang.el ends here
