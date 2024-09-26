;;; ontop-gleam.el --- Gleam configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-gleam.el")'.

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
;;; GLEAM TS MODE
;; <https://github.com/gleam-lang/gleam-mode>

(use-package gleam-ts-mode
  :ensure t
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
;; Common keybindings are configured in `./ontop-core.el'

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; GLEAM LSP
;; <https://gleam.run/language-server>

(use-package eglot
  :ensure t
  :custom
  ;; A longer timeout seems required for the first run in a new project
  (eglot-connect-timeout 60)            ; default: 30
  :config
  (add-to-list 'eglot-server-programs
               '((gleam-ts-mode) . ("gleam lsp")))
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
;;   :ensure t
;;   :diminish mix-minor-mode
;;   :hook
;;   ((gleam-ts-mode) . mix-minor-mode))

;;  ____________________________________________________________________________
;;; ERLANG
;; <https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html>

(use-package erlang
  :ensure t)

;; _____________________________________________________________________________
(provide 'ontop-gleam)
;;; ontop-gleam.el ends here
