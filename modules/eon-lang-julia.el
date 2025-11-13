;;; eon-lang-julia.el --- Julia -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-julia.el")'.

;;; Code:

;; _____________________________________________________________________________
;;; JULIA MODE
;; <https://github.com/ronisbr/julia-ts-mode>

(use-package julia-ts-mode :ensure t
  :init
  (eon-treesitter-ensure-grammar
   '(julia "https://github.com/tree-sitter/tree-sitter-julia"))
  :mode "\\.jl$")

;; _____________________________________________________________________________
;;; JULIA SNAIL
;; <https://github.com/gcv/julia-snail/>
;; Interactive Julia with REPL

(use-package julia-snail :ensure t
  :custom
  ;; Julia Snail requires a terminal emulator within Emacs for the REPL
  (julia-snail-terminal-type :eat)
  ;; Print the result of evaluating code to the REPL
  (julia-snail-repl-display-eval-results t)  ; nil to disable
  ;; Show result of evaluating code in the source buffer
  (julia-snail-popup-display-eval-results nil)  ; :command, :change or nil
  ;; The default works with Consult and Helm
  (julia-snail-imenu-style :module-tree)  ; :module-tree, :flat or nil
  :hook
  (julia-ts-mode . julia-snail-mode))

;; _____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-core.el'

;; Setup Eglot for Julia and install the language server binary if necessary
;; <https://github.com/non-Jedi/eglot-jl>
(use-package eglot-jl :ensure t
  :init
  (eglot-jl-init))

(use-package eglot :ensure nil
  :hook
  ;; Start language server automatically
  (julia-ts-mode . eglot-ensure)
  ;; Tell the language server to format the buffer before saving
  (julia-ts-mode . (lambda ()
                     (add-hook 'before-save-hook
                               #'eglot-format-buffer nil 'local))))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate Julia code in Org source code blocks via "C-c C-c".

;; 2 Julia packages must be added for this to work: DataFrames and CSV.
;; <https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-julia.html>
(use-package org :ensure nil
  :config
  (add-to-list 'org-babel-load-languages '(julia . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

;; _____________________________________________________________________________
(provide 'eon-lang-julia)
;;; eon-lang-julia.el ends here
