;;; eon-haskell.el --- Haskell configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-haskell.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; HASKELL-MODE
;;  <http://haskell.github.io/haskell-mode>

(use-package haskell-mode
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

(use-package haskell
  :bind
  (:map interactive-haskell-mode-map
        ("C-c C-c" . haskell-compile)
        ("C-c C-e" . haskell-process-load-file)))

;;  ____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;;  <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;;  <https://haskell-language-server.readthedocs.io/en/latest/configuration.html>
;;  Common keybindings are configured in `./eon-core.el'

(use-package eglot
  :ensure nil
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

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters
  :hook
  (interactive-haskell-mode . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   ((haskell-mode interactive-haskell-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs

;; Starts a GHCi REPL in the background
(use-package ob-haskell)

;; Evaluate Haskell code in Org source code blocks via "C-c C-c"
(use-package org
  :hook
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 (add-to-list 'org-babel-load-languages '(haskell . t))))))

;;  ____________________________________________________________________________
(provide 'eon-haskell)
;;; eon-haskell.el ends here
