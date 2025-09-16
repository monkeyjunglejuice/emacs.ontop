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

;; In order to use Tree Sitter, install the tree-sitter binary with your
;; OS package manager

(eon-treesitter-ensure-grammar
 '(erlang "https://github.com/WhatsApp/tree-sitter-erlang"))

(use-package erlang-ts :ensure t
  :defer t
  :mode ("\\.erl\\'" . erlang-ts-mode))

;;  ____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-core.el'

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ERLANG LANGUAGE PLATFORM
;; <https://github.com/WhatsApp/erlang-language-platform>

(use-package eglot :ensure nil
  :custom
  ;; A longer timeout seems required for the first run in a new project
  (eglot-connect-timeout 30)            ; default: 30
  :config
  ;; Make sure to adapt the path and use the .bat script for Windows
  (add-to-list 'eglot-server-programs
               '((erlang-mode) . ("elp" "server")))
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
(use-package rainbow-delimiters :ensure t
  :hook
  (erlang-shell-mode . rainbow-delimiters-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs
;; Evaluate Erlang code in Org source code blocks via "C-c C-c"

;; Only available from Github, therefore not installed
;; <https://github.com/xfwduke/ob-erlang>
;; (use-package ob-erlang)

;; (use-package org :ensure nil
;;   :config
;;   (add-to-list 'org-babel-load-languages '(erlang . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                org-babel-load-languages))

;; _____________________________________________________________________________
(provide 'eon-erlang)
;;; eon-erlang.el ends here
