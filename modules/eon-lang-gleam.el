;;; eon-lang-gleam.el --- Gleam -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GLEAM TS MODE
;; <https://github.com/gleam-lang/gleam-mode>

(use-package gleam-ts-mode :ensure t
  :init
  (eon-treesitter-ensure-grammar
   '(gleam "https://github.com/gleam-lang/tree-sitter-gleam"))
  :mode (rx ".gleam" eos))

;; _____________________________________________________________________________
;;; REPL

;; Gleam has no REPL yet!
;; <https://github.com/gleam-lang/gleam/discussions/1305>
;; <https://github.com/gleam-lang/gleam/issues/25>

;; _____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./eon-core.el'

;; <https://gleam.run/language-server>

(use-package eglot :ensure nil
  :custom
  ;; A longer timeout seems required for the first run in a new project
  (eglot-connect-timeout 60)  ; default: 30
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

;; _____________________________________________________________________________
;;; MIX GLEAM
;; <https://github.com/gleam-lang/mix_gleam>

;; (use-package mix :ensure t
;;   :diminish mix-minor-mode
;;   :hook
;;   ((gleam-ts-mode) . mix-minor-mode))

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    (gleam-ts-mode . aggressive-indent-mode)))

;; _____________________________________________________________________________
(provide 'eon-lang-gleam)
;;; eon-lang-gleam.el ends here
