;;; eon-lang-erlang.el --- Erlang -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; ERLANG MODE
;; <https://github.com/erlang/otp/tree/master/lib/tools/emacs>
;; <https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html>

(use-package erlang-ts :ensure t
  :defer t
  :init
  (eon-treesitter-ensure-grammar
   '(erlang "https://github.com/WhatsApp/tree-sitter-erlang"))
  :mode ("\\.erl\\'" . erlang-ts-mode))

;; _____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-core.el'

;; Erlang Language Platform
;; <https://github.com/WhatsApp/erlang-language-platform>

(use-package eglot :ensure nil
  :custom
  ;; A longer timeout seems required for the first run in a new project
  (eglot-connect-timeout 60)  ; default: 30
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

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs
;; Evaluate Erlang code in Org source code blocks via "C-c C-c"

;; Only available from Github, therefore not installed
;; <https://github.com/xfwduke/ob-erlang>
;; (use-package ob-erlang :ensure t)

;; (use-package org :ensure nil
;;   :config
;;   (add-to-list 'org-babel-load-languages '(erlang . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                org-babel-load-languages))

;; _____________________________________________________________________________
(provide 'eon-lang-erlang)
;;; eon-lang-erlang.el ends here
