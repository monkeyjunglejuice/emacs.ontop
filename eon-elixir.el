;;; eon-elixir.el --- Elixir -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop
;;
;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; ELIXIR TS MODE
;; <https://github.com/wkirschbaum/elixir-ts-mode>

;; In order to use Tree Sitter, install the tree-sitter binary with your
;; OS package manager

(use-package elixir-ts-mode :ensure nil
  :init
  (eon-treesitter-ensure-grammar '(elixir heex)))

;;  ____________________________________________________________________________
;;; REPL

(use-package inf-elixir :ensure t
  :custom
  (inf-elixir-switch-to-repl-on-send nil)
  :config
  (defun inf-elixir-recompile ()
    "Send `IEx.Helpers.recompile/1' to recompile the current Mix project.
Note this function simply recompiles Elixir modules, without reloading
configuration or restarting applications."
    (interactive)
    (inf-elixir--send (format "recompile()"))
    (if inf-elixir-switch-to-repl-on-send
        (goto-char (point-max))))
  (defun inf-elixir-observer ()
    "Start the Erlang Observer in IEx."
    (interactive)
    (inf-elixir--send (format ":observer.start()")))
  :hook
  ((elixir-ts-mode heex-ts-mode) . inf-elixir-minor-mode)
  :bind
  (:map elixir-ts-mode-map
        ("C-c C-z" . inf-elixir-project)
        ("C-c C-l" . inf-elixir-send-line)
        ("C-c C-r" . inf-elixir-send-region)
        ("C-c C-b" . inf-elixir-send-buffer)
        ("C-c C-c" . inf-elixir-recompile)
        ("C-c c"   . inf-elixir-reload-module)
        ("C-c C-o" . inf-elixir-observer)))

;;  ____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-core.el'

;; Elixir-ls language server is used per default in this setup.
;; Here's an Elixir language server comparison:
;; <https://gist.github.com/Nezteb/dc63f1d5ad9d88907dd103da2ca000b1>

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ELIXIR-LS
;; <https://github.com/elixir-lsp/elixir-ls>

(use-package eglot :ensure nil
  :custom
  ;; A longer timeout seems required for the first run in a new project
  (eglot-connect-timeout 60)            ; default: 30
  :config
  ;; Make sure to adapt the path and use the .bat script for Windows
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode heex-ts-mode) . ("elixir-ls")))
  :hook
  ;; Start language server automatically
  ((elixir-ts-mode heex-ts-mode) . eglot-ensure)
  ;; Tell the language server to format the buffer before saving
  ((elixir-ts-mode heex-ts-mode)
   . (lambda ()
       (add-hook 'before-save-hook
                 #'eglot-format-buffer nil 'local))))

;;  ____________________________________________________________________________
;;; MIX
;; <https://hexdocs.pm/mix/1.12/Mix.html>

(use-package mix :ensure t
  :diminish mix-minor-mode
  :hook
  ((elixir-ts-mode heex-ts-mode) . mix-minor-mode))

;;  ____________________________________________________________________________
;;; FLYCHECK CREDO
;; <http://credo-ci.org/>
;; Static analyzer and syntax checker. Must be installed first as a
;; project dependency in `mix.exs' and will be available afterwards:
;;
;;  defp deps do
;;    [
;;      {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
;;    ]
;;  end
;;
;; Run `mix deps.get' afterwards, and then `mix credo'

;; (use-package flycheck :ensure t
;;   :hook
;;   (elixir-ts-mode . flycheck-mode))

;; (use-package flycheck-credo :ensure t
;;   :custom
;;   (flycheck-elixir-credo-strict t)
;;   :hook
;;   (flycheck-mode . flycheck-credo-setup))

;;  ____________________________________________________________________________
;;; EXUNIT
;; <https://github.com/ananthakumaran/exunit.el>

(use-package exunit :ensure t
  :diminish exunit-mode
  :custom
  (transient-default-level 4)
  :hook
  (elixir-ts-mode . exunit-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters :ensure t
  :hook
  (inf-elixir-mode . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   ((elixir-ts-mode inf-elixir-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate Elixir code in Org source code blocks via "C-c C-c"

;; TODO -- not working, package might be outdated
;; <https://github.com/zweifisch/ob-elixir>
(use-package ob-elixir :ensure t)

(use-package org :ensure nil
  :config
  (add-to-list 'org-babel-load-languages '(elixir . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

;; _____________________________________________________________________________
(provide 'eon-elixir)
;;; eon-elixir.el ends here
