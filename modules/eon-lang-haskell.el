;;; eon-lang-haskell.el --- Haskell -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; HASKELL-MODE
;; <https://github.com/haskell/haskell-mode>
;; <http://haskell.github.io/haskell-mode/manual/latest>

(use-package haskell-mode :ensure t

  :init

  ;; Not actually used by Haskell mode, but install it anyway
  (eon-treesitter-ensure-grammar
   '(haskell "https://github.com/tree-sitter/tree-sitter-haskell"))

  (eon-localleader-defkeymap
      interactive-haskell-mode eon-localleader-interactive-haskell-mode-map
    :doc "Local leader keymap for `interactive-haskell-mode' (SRC).")

  (eon-localleader-defkeymap
      haskell-interactive-mode eon-localleader-haskell-interactive-mode-map
    :doc "Local leader keymap for `haskell-interactive-mode' (REPL).")

  :custom

  ;; Hoogle binary name
  (haskell-hoogle-command "hoogle")

  ;; Use `completing-read' instead of `ido-mode'
  (haskell-completing-read-function 'completing-read)
  ;; Enable process log buffer, useful for debugging
  (haskell-process-log t)
  ;; Disable info header in REPL?
  (haskell-process-show-debug-tips nil)

  ;; Ask wether to remove unused imports
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-suggest-hoogle-imports t)

  ;; Select either 'cabal-repl, 'cabal-new-repl, 'cabal-dev, 'cabal-ghci,
  ;; 'stack-ghci, 'ghci or 'auto.
  ;; Check the documentation for `haskell-process-type' to see how the real
  ;; type is guessed when it is set to 'auto.
  (haskell-process-type 'auto)

  ;; Don't use hasktags; the language server provides this functionality
  (haskell-tags-on-save nil)
  
  ;; Don't use haskell-mode to show docs; the language server will do that
  (haskell-doc-show-global-types nil)
  (haskell-doc-show-prelude nil)
  (haskell-doc-show-reserved nil)
  (haskell-doc-show-strategy nil)
  (haskell-doc-show-user-defined nil)

  :hook

  ;; Start minor mode in source buffers to interact with a REPL
  (haskell-mode . interactive-haskell-mode)

  :bind

  ;; Source code buffer
  (:map eon-localleader-interactive-haskell-mode-map
        ("c"   . haskell-process-cabal-build)
        ("C"   . haskell-process-cabal)
        ("C-c" . haskell-cabal-visit-file)
        ("g"   . haskell-interactive-switch)
        ("i"   . haskell-navigate-imports)
        ("k"   . haskell-process-interrupt)
        ("l"   . haskell-process-load-file)
        ("s"   . haskell-session-change)
        ("S"   . haskell-session-change-target)
        ("t"   . haskell-mode-show-type-at)
        ("T"   . haskell-doc-show-type)
        ("C-r" . haskell-process-restart))

  ;; REPL buffer
  (:map eon-localleader-haskell-interactive-mode-map
        ("c"   . haskell-process-cabal-build)
        ("C"   . haskell-process-cabal)
        ("C-c" . haskell-cabal-visit-file)
        ("g"   . haskell-interactive-switch-back)
        ("k"   . haskell-process-interrupt)
        ("l"   . haskell-process-load-file)
        ("s"   . haskell-session-change)
        ("S"   . haskell-session-change-target)
        ("t"   . haskell-doc-show-type)
        ("C-r" . haskell-process-restart)))

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
;;; HOOGLE FRONTEND
;; <https://codeberg.org/rahguzar/consult-hoogle>

(when (eon-modulep 'eon-consult)
  (use-package consult-hoogle :ensure t

    :bind

    ;; SRC buffer
    (:map eon-localleader-interactive-haskell-mode-map
          ("h" . consult-hoogle)
          ("H" . consult-hoogle-project))

    ;; REPL buffer
    (:map eon-localleader-haskell-interactive-mode-map
          ("h" . consult-hoogle)
          ("H" . consult-hoogle-project))))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs
;; Evaluate Haskell code in Org source code blocks via "C-c C-c"
;; Starts a GHCi REPL in the background

(use-package ob-haskell :ensure nil
  :after org)

;; _____________________________________________________________________________
(provide 'eon-lang-haskell)
;;; eon-lang-haskell.el ends here
