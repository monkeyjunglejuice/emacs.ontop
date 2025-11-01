;;; eon-lang-haskell.el --- Haskell -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; HASKELL-MODE
;;  <http://haskell.github.io/haskell-mode>

(use-package haskell-mode
  :init
  (eon-treesitter-ensure-grammar
   '(haskell "https://github.com/tree-sitter/tree-sitter-haskell"))
  :custom
  (haskell-completing-read-function 'completing-read)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-suggest-hoogle-imports t)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-type 'auto)
  ;; Don't use hasktags; the language server provides that functionality
  (haskell-tags-on-save nil)
  ;; Don't use haskell-mode to show docs; the language server will do that
  (haskell-doc-show-global-types nil)
  (haskell-doc-show-prelude nil)
  (haskell-doc-show-reserved nil)
  (haskell-doc-show-strategy nil)
  (haskell-doc-show-user-defined nil)
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package haskell :ensure nil
  :bind
  (:map interactive-haskell-mode-map
        ("C-c C-c" . haskell-compile)
        ("C-c C-e" . haskell-process-load-file)))

;; _____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;;  <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;;  <https://haskell-language-server.readthedocs.io/en/latest/configuration.html>
;;  Common keybindings are configured in `./eon-core.el'

(use-package eglot :ensure nil
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (formatting-provider . "ormolu"))))
  :custom
  ;; Shutdown language server after closing last file?
  (eglot-autoshutdown t)
  ;; Allow edits without confirmation?
  (eglot-confirm-server-initiated-edits nil)
  :hook
  ;; Start language server automatically when opening a Haskell file?
  (haskell-mode . eglot-ensure)
  ;; Format the buffer before saving?
  (haskell-mode . (lambda ()
                    (add-hook 'before-save-hook
                              #'eglot-format-buffer t 'local))))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs

;; Starts a GHCi REPL in the background
(use-package ob-haskell :ensure nil)

;; Evaluate Haskell code in Org source code blocks via "C-c C-c"
(use-package org :ensure nil
  :config
  (add-to-list 'org-babel-load-languages '(haskell . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

;; _____________________________________________________________________________
(provide 'eon-lang-haskell)
;;; eon-lang-haskell.el ends here
