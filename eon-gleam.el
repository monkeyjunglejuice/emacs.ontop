;;; eon-gleam.el --- Gleam configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-gleam.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; GLEAM TS MODE
;; <https://github.com/gleam-lang/gleam-mode>

(use-package gleam-ts-mode
  :mode (rx ".gleam" eos))

;; Unless you have the Gleam tree-sitter grammar installed and treesit knows
;; where to find it, you'll want to run M-x gleam-ts-install-grammar. It should
;; only take a moment, but does require that your OS has a C compiler available.

;;  ____________________________________________________________________________
;;; REPL

;; Gleam has no REPL yet!
;; <https://github.com/gleam-lang/gleam/discussions/1305>
;; <https://github.com/gleam-lang/gleam/issues/25>

;;  ____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-core.el'

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; GLEAM LSP
;; <https://gleam.run/language-server>

(use-package eglot
  :ensure nil
  :custom
  ;; A longer timeout seems required for the first run in a new project
  (eglot-connect-timeout 60)            ; default: 30
  :config
  (add-to-list 'eglot-server-programs
               '((gleam-ts-mode) . ("gleam" "lsp")))
  :hook
  ;; Start language server automatically
  ((gleam-ts-mode) . eglot-ensure)
  ;; Tell the language server to format the buffer before saving
  ((gleam-ts-mode) .
   (lambda ()
     (add-hook 'before-save-hook
               #'eglot-format-buffer nil 'local))))

;;  ____________________________________________________________________________
;;; MIX GLEAM
;; <https://github.com/gleam-lang/mix_gleam>

;; (use-package mix
;;   :diminish mix-minor-mode
;;   :hook
;;   ((gleam-ts-mode) . mix-minor-mode))

;;  ____________________________________________________________________________
;;; ERLANG
;; <https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html>

(use-package erlang)

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   (gleam-ts-mode . paren-face-mode))

;; _____________________________________________________________________________
(provide 'eon-gleam)
;;; eon-gleam.el ends here
