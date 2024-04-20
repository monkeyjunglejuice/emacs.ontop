;;; ontop-elixir.el --- Elixir configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-elixir.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;; <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package nil))

(eval-when-compile
  (require 'use-package))

;;  ____________________________________________________________________________
;;; ELIXIR MODE
;; <https://github.com/wkirschbaum/elixir-mode>

(use-package elixir-mode
  :ensure t
  :bind
  (:map elixir-mode-map
        ("C-M-a" . #'elixir-beginning-of-defun)
        ("C-M-e" . #'elixir-end-of-defun)
        ("M-q" . #'elixir-mode-fill-doc-string)
        ("C-c C-d" . #'elixir-mode-open-docs-stable)))

;;  ____________________________________________________________________________
;;; REPL

(use-package inf-elixir
  :ensure t
  :custom
  (inf-elixir-switch-to-repl-on-send nil)
  :config
  (defun inf-elixir-recompile ()
    "Send command to recompile the current Mix project using `IEx.Helpers.recompile/1'.
Note this function simply recompiles Elixir modules, without reloading configuration
or restarting applications."
    (interactive)
    (inf-elixir--send (format "recompile()")))
  :hook
  ((elixir-mode heex-mode) . inf-elixir-minor-mode)
  :bind
  ;; Reach the REPL from anywhere via global key binding
  (:map ctl-z-x-map
        ("e" . 'inf-elixir))
  (:map elixir-mode-map
        ("C-c C-z" . #'inf-elixir-project)
        ("C-c C-l" . #'inf-elixir-send-line)
        ("C-c C-r" . #'inf-elixir-send-region)
        ("C-c C-b" . #'inf-elixir-send-buffer)
        ("C-c C-c" . #'inf-elixir-reload-module)
        ("C-c C-k" . #'inf-elixir-recompile)))

;;  ____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./ontop-core.el'

;; Elixir-ls language server is used per default in this setup.
;; Here's an Elixir language server comparison:
;; <https://gist.github.com/Nezteb/dc63f1d5ad9d88907dd103da2ca000b1>

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ELIXIR-LS
;; <https://github.com/elixir-lsp/elixir-ls>

(use-package eglot
  :ensure t
  :custom
  ;; A longer timeout seems required for the first run in a new project
  (eglot-connect-timeout 60)            ; default: 30
  :config
  ;; Make sure to adapt the path and use the .bat script for Windows
  (add-to-list 'eglot-server-programs
               '((elixir-mode heex-mode) . ("elixir-ls")))
  :hook
  ;; Start language server automatically
  ((elixir-mode heex-mode) . eglot-ensure)
  ;; Tell the language server to format the buffer before saving
  ((elixir-mode heex-mode) .
   (lambda ()
     (add-hook 'before-save-hook
               #'eglot-format-buffer nil 'local))))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; NEXT-LS
;; <https://www.elixir-tools.dev/docs/next-ls/quickstart/>
;; Credo-support is already built in <https://hex.pm/packages/credo>.
;; Add Credo to your project file `mix.exs' and issue the shell command
;; 'mix deps.get'

;; (use-package eglot
;;   :ensure t
;;   :config
;;   ;; Make sure to edit the path appropriately, use the .bat script for Windows
;;   (add-to-list 'eglot-server-programs
;;                '((elixir-mode heex-mode) .
;;                  ("nextls" "--stdio=true"
;;                   :initializationOptions
;;                   (:experimental (:completions (:enable t))))))
;;   :hook
;;   ;; Start language server automatically
;;   ((elixir-mode heex-mode) . eglot-ensure)
;;   ;; Tell the language server to format the buffer before saving
;;   ((elixir-mode heex-mode) .
;;    (lambda ()
;;      (add-hook 'before-save-hook
;;                #'eglot-format-buffer nil 'local))))

;;  ____________________________________________________________________________
;;; MIX
;; <https://hexdocs.pm/mix/1.12/Mix.html>

(use-package mix
  :ensure t
  :bind
  (:map elixir-mode-map
        ("C-c M-k" . mix-compile)))

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

;; (use-package flycheck
;;   :ensure t
;;   :hook
;;   (elixir-mode . flycheck-mode))

;; (use-package flycheck-credo
;;   :ensure t
;;   :custom
;;   (flycheck-elixir-credo-strict t)
;;   :hook
;;   (flycheck-mode . flycheck-credo-setup))

;;  ____________________________________________________________________________
;;; EXUNIT
;; <https://github.com/ananthakumaran/exunit.el>

(use-package exunit
  :ensure t
  :diminish exunit-mode
  :custom
  (transient-default-level 4)
  :hook
  (elixir-mode . exunit-mode))

;;  ____________________________________________________________________________
;;; ERLANG
;; <https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html>

(use-package erlang
  :ensure t)

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs

;; <https://github.com/zweifisch/ob-elixir>
(use-package ob-elixir
  :ensure t)

;; Evaluate Elixir code in Org source code blocks via "C-c C-c"
(use-package org
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 (add-to-list 'org-babel-load-languages '(elixir . t))))))

;; _____________________________________________________________________________
(provide 'ontop-elixir)
;;; ontop-elixir.el ends here
