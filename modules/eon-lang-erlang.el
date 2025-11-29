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

  ;; A longer timeout may be required for the first run in a new project
  (eglot-connect-timeout 30)  ; default: 30

  :config

  ;; Make sure to adapt the path and use the .bat script for Windows
  (add-to-list 'eglot-server-programs
               '((erlang-mode erlang-ts-mode) . ("elp" "server")))

  :hook

  ;; Start language server automatically
  ((erlang-mode erlang-ts-mode) . eglot-ensure)

  ;; Tell the language server to format the buffer before saving
  ((erlang-mode erlang-ts-mode)
   . (lambda ()
       (add-hook 'before-save-hook
                 #'eglot-format-buffer nil 'local))))

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    ((erlang-mode erlang-ts-mode) . aggressive-indent-mode)))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs
;; Evaluate Erlang code in Org source code blocks via "C-c C-c"

;; Only available from Github
;; <https://github.com/xfwduke/ob-erlang>
(use-package ob-erlang
  :vc (:url "https://github.com/xfwduke/ob-erlang.git"
            :rev :newest)
  :after org)

;; _____________________________________________________________________________
(provide 'eon-lang-erlang)
;;; eon-lang-erlang.el ends here
