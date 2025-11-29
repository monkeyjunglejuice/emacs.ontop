;;; eon-lang-julia.el --- Julia -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; JULIA MODE
;; <https://github.com/JuliaEditorSupport/julia-emacs>
;; There's also a tree-sitter enabled Julia mode, but it seems less maintained.
;; <https://github.com/ronisbr/julia-ts-mode>
;; That's why we're using the original Julia mode.

(use-package julia-mode :ensure t)

;; _____________________________________________________________________________
;;; JULIA REPL

;; Julia Snail requires a terminal emulator within Emacs for the REPL, both
;; `vterm' and `eat' are supported. In order to use Snail, the module
;; `eon-vterm' must be enabled.
;; <https://github.com/gcv/julia-snail/>
(when (eon-modulep 'eon-vterm)
  (use-package julia-snail :ensure t
    :custom
    (julia-snail-terminal-type :vterm)
    ;; Print the result of evaluating code to the REPL
    (julia-snail-repl-display-eval-results t)  ; nil to disable
    ;; Show result of evaluating code in the source buffer
    (julia-snail-popup-display-eval-results nil)  ; :command, :change or nil
    ;; The default works with Consult and Helm
    (julia-snail-imenu-style :module-tree)  ; :module-tree, :flat or nil
    :hook
    ((julia-mode julia-ts-mode) . julia-snail-mode)))

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
  :custom
  ;; A longer timeout may be required for the first run in a new project
  (eglot-connect-timeout 60)  ; default: 30
  :hook
  ;; Start language server automatically
  ((julia-mode julia-ts-mode) . eglot-ensure)
  ;; Tell the language server to format the buffer before saving
  ((julia-mode julia-ts-mode) . (lambda ()
                     (add-hook 'before-save-hook
                               #'eglot-format-buffer nil 'local))))

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    ((julia-mode julia-ts-mode) . aggressive-indent-mode)))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate Julia code in Org source code blocks via "C-c C-c".

;; 2 Julia packages must be added for this to work: DataFrames and CSV.
;; <https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-julia.html>
(use-package ob-julia :ensure nil
  :after org)

;; _____________________________________________________________________________
(provide 'eon-lang-julia)
;;; eon-lang-julia.el ends here
