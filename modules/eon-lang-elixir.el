;;; eon-lang-elixir.el --- Elixir -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience languages elixir
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
;;; ELIXIR TS MODE
;; <https://github.com/wkirschbaum/elixir-ts-mode>

;; In order to use Tree Sitter, install the tree-sitter binary with your
;; OS package manager

(use-package elixir-ts-mode :ensure nil

  :init

  (eon-treesitter-ensure-grammar '(elixir heex))

  (eon-localleader-defkeymap elixir-ts-mode eon-localleader-elixir-map
    :doc "Local leader keymap for the Elixir major mode."))

;; _____________________________________________________________________________
;;; REPL

(use-package inf-elixir :load-path "~/code/inf-elixir"

  :init

  (eon-localleader-defkeymap inf-elixir-mode eon-localleader-inf-elixir-map
    :doc "Local leader keymap for the Elixir REPL.")

  (defun eon-inf-elixir-run ()
    "Starts IEx in the project context when a Mix project is detected.
The commands are `inf-elixir-project-command' (defaults to \"iex -S mix\") and
`inf-elixir-base-command' (defaults to \"iex\")."
    (interactive)
    ;; Return the project root directory if a mix.exs file is found
    (if (inf-elixir--find-project-root)
        (progn (message "inf-elixir: IEx running in project context...")
               (inf-elixir-project))
      (progn (message "inf-elixir: IEx running standalone...")
             (inf-elixir))))

  :custom

  (inf-elixir-switch-to-repl-on-send nil)

  :config

  (defun eon-inf-elixir-switch-to-repl ()
    "Switch to IEx buffer."
    (interactive)
    (when-let* ((buf (inf-elixir--determine-repl-buf)))
      (pop-to-buffer buf)))

  (defun eon-inf-elixir-recompile ()
    "Send `IEx.Helpers.recompile/1' to recompile the current Mix project.
Note this function simply recompiles Elixir modules, without reloading
configuration or restarting applications."
    (interactive)
    (inf-elixir--send (format "recompile()"))
    (if inf-elixir-switch-to-repl-on-send
        (goto-char (point-max))))

  (defun eon-inf-elixir-observer ()
    "Start the Erlang Observer in IEx."
    (interactive)
    (inf-elixir--send (format ":observer.start()")))

  (defun eon-inf-elixir-self ()
    "Return the PID of the IEx process."
    (interactive)
    (inf-elixir--send (format "self()")))

  (defun eon-inf-elixir-flush ()
    "Flush the IEx mailbox."
    (interactive)
    (inf-elixir--send (format "flush()")))

  :hook

  ((elixir-ts-mode heex-ts-mode) . inf-elixir-minor-mode)

  :bind

  (:map eon-localleader-elixir-map
        ("b" . inf-elixir-send-buffer)
        ("c" . inf-elixir-reload-module)
        ("C" . eon-inf-elixir-recompile)
        ("f" . eon-inf-elixir-flush)
        ("g" . eon-inf-elixir-switch-to-repl)
        ("l" . inf-elixir-send-line)
        ("o" . eon-inf-elixir-observer)
        ("p" . eon-inf-elixir-self)
        ("r" . inf-elixir-send-region)
        ("s" . inf-elixir-set-repl))

  (:map eon-localleader-inf-elixir-map
        ("c" . inf-elixir-reload-module)
        ("C" . eon-inf-elixir-recompile)
        ("f" . eon-inf-elixir-flush)
        ("g" . eon-inf-elixir-switch-to-repl)
        ("o" . eon-inf-elixir-observer)
        ("p" . eon-inf-elixir-self)))

;; _____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in './eon-core.el'
;; <https://github.com/elixir-lang/expert>

(use-package eglot :ensure nil

  :init

  ;; The language server needs a snippet engine to provide most completions.
  ;; Therefore, the module `eon-yasnippet' is required.
  (eon-load-module 'eon-yasnippet)

  :custom

  ;; A longer timeout may be required for the first run in a new project
  (eglot-connect-timeout 60)  ; default: 30

  :config

  ;; Make sure to adapt the path and use the appropriate executable names for
  ;; Linux, macOS or Windows
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode heex-ts-mode) . ("expert" "--stdio")))

  :hook

  ;; Start language server automatically
  ((elixir-ts-mode heex-ts-mode) . eglot-ensure)

  ;; Tell the language server to format the buffer before saving
  ((elixir-ts-mode heex-ts-mode)
   . (lambda ()
       (add-hook 'before-save-hook
                 #'eglot-format-buffer nil 'local))))

;; _____________________________________________________________________________
;;; MIX
;; <https://hexdocs.pm/mix/1.12/Mix.html>

(use-package mix :ensure t
  :diminish mix-minor-mode
  :hook
  ((elixir-ts-mode heex-ts-mode) . mix-minor-mode))

;; _____________________________________________________________________________
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
;; Then run 'mix deps.get' and 'mix credo'

(when (eon-modulep 'eon-flycheck)
  (use-package flycheck-credo :ensure t
    :custom
    (flycheck-elixir-credo-strict t)
    :hook
    (flycheck-mode . flycheck-credo-setup)))

;; _____________________________________________________________________________
;;; EXUNIT
;; <https://github.com/ananthakumaran/exunit.el>

(use-package exunit :ensure t
  :diminish exunit-mode
  :custom
  (transient-default-level 4)
  :hook
  (elixir-ts-mode . exunit-mode))

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    (elixir-ts-mode . aggressive-indent-mode)))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate Elixir code in Org source code blocks via "C-c C-c"

;; TODO Garbage output, package might be outdated
;; <https://github.com/zweifisch/ob-elixir>
(use-package ob-elixir :ensure t
  :after org)

;; _____________________________________________________________________________
(provide 'eon-lang-elixir)
;;; eon-lang-elixir.el ends here
