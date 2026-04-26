;;; eon-lang-julia.el --- Julia -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience languages julia
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; JULIA MODE
;; <https://github.com/JuliaEditorSupport/julia-emacs>
;; There's also a tree-sitter enabled Julia mode, but it seems less maintained.
;; <https://github.com/ronisbr/julia-ts-mode>
;; That's why we're using the original Julia mode.

(use-package julia-mode :ensure t)

;; _____________________________________________________________________________
;;; JULIA REPL
;; <https://github.com/gcv/julia-snail>

(use-package julia-snail :ensure t

  :init

  ;; In order to use Snail, the module `eon-vterm' is required
  (eon-load-module 'eon-vterm)

  (eon-localleader-defkeymap
      julia-snail-mode
      eon-localleader-julia-snail-map
    :doc "Local leader keymap for Julia Snail src buffers.")

  (eon-localleader-defkeymap
      julia-snail-repl-mode
      eon-localleader-julia-snail-repl-map
    :doc "Local leader keymap for Julia Snail REPL.")

  :custom

  ;; Print the result of evaluating code to the REPL
  (julia-snail-repl-display-eval-results t)  ; nil to disable
  ;; Show result of evaluating code in the source buffer
  (julia-snail-popup-display-eval-results nil)  ; :command, :change or nil
  ;; The default works with Consult and Helm
  (julia-snail-imenu-style :module-tree)  ; :module-tree, :flat or nil

  :config
  
  ;; Set the terminal Vterm
  (setopt julia-snail-terminal-type :vterm)

  :hook

  ;; Enable the minor mode
  ((julia-mode julia-ts-mode) . julia-snail-mode)

  :bind

  (:map eon-localleader-julia-snail-map
        ("x" . julia-snail-send-top-level-form)
        ("p" . julia-snail-package-activate)
        ("c" . julia-snail-send-top-level-form)
        ("d" . julia-snail-doc-lookup)
        ("e" . julia-snail-send-dwim)
        ("b" . julia-snail-send-buffer-file)
        ("l" . julia-snail-send-line)
        ("r" . julia-snail-send-region)
        ("w" . julia-snail-copy-last-eval-result)
        ("g" . julia-snail)
        ("u" . julia-snail-update-module-cache))

  (:map eon-localleader-julia-snail-repl-map
        ("g" . julia-snail-repl-go-back)
        ("i" . julia-snail-interrupt-task)
        ("k" . julia-snail-repl-terminal-kill-line)))

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
