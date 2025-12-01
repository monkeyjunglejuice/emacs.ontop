;;; eon-lang-janet.el --- Janet -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; JANET MODE
;; <https://github.com/sogaiu/janet-ts-mode>

(use-package janet-ts-mode
  :vc (:url "https://github.com/sogaiu/janet-ts-mode.git"
            :rev :newest)
  :init
  (eon-treesitter-ensure-grammar
   '(janet-simple "https://github.com/sogaiu/tree-sitter-janet-simple")))

;; _____________________________________________________________________________
;;; REPL
;; <https://github.com/sogaiu/ajrepl>

(use-package ajrepl
  :vc (:url "https://github.com/sogaiu/ajrepl.git"
            :rev :newest)
  :hook
  (janet-ts-mode . ajrepl-interaction-mode))

;; _____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-core.el'

;; Setup Eglot for Janet
;; <https://github.com/CFiggers/janet-lsp>
(use-package eglot :ensure nil
  :custom
  ;; A longer timeout may be required for the first run in a new project
  (eglot-connect-timeout 60)  ; default: 30
  :config
  (add-to-list 'eglot-server-programs
               '((janet-ts-mode) . ("janet-lsp")))
  :hook
  ;; Start language server automatically
  (janet-ts-mode . eglot-ensure)
  ;; Tell the language server to format the buffer before saving
  (janet-ts-mode . (lambda ()
                     (add-hook 'before-save-hook
                               #'eglot-format-buffer nil 'local))))

;; _____________________________________________________________________________
;;; FLYCHECK
;; <https://github.com/sogaiu/flycheck-janet>

(when (eon-modulep 'eon-flycheck)
  (use-package flycheck-janet
    :vc (:url "https://github.com/sogaiu/flycheck-janet.git"
              :rev :newest)
    :hook
    (janet-ts-mode . flycheck-mode)))

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    (janet-ts-mode . aggressive-indent-mode)))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs

;; Evaluate Janet code in Org source code blocks via "C-c C-c"
;; <https://github.com/DEADB17/ob-janet>
(use-package ob-janet
  :vc (:url "https://github.com/DEADB17/ob-janet.git"
            :rev :newest)
  :after org)

;; _____________________________________________________________________________
(provide 'eon-lang-janet)
;;; eon-lang-janet.el ends here
