;;; eon-lang-unison.el --- Unison -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; UNISON TS MODE
;; <https://github.com/fmguerreiro/unison-ts-mode>
;; <https://github.com/fmguerreiro/tree-sitter-unison>

(use-package unison-ts-mode :ensure t

  :init

  (eon-localleader-defkeymap unison-ts-mode eon-localleader-unison-scratch-map
    :doc "Local leader keymap for Unison scratch buffer.")

  :mode

  ("\\.u\\'" "\\.unison\\'")

  :custom

  (unison-ts-grammar-install 'auto)
  (unison-ts-grammar-repository
   "https://github.com/fmguerreiro/tree-sitter-unison")

  :hook

  ;; Start a headless UCM process to provide the LSP server
  (unison-ts-mode . unison-ts-mode-setup-eglot)

  :bind

  (:map eon-localleader-unison-scratch-map
        ("a" . unison-ts-add)
        ("g" . unison-ts-repl)
        ("r" . unison-ts-run)
        ("l" . unison-ts-load)
        ("t" . unison-ts-test)
        ("w" . unison-ts-watch)
        ("u" . unison-ts-update)
        ("e" . unison-ts-send-region)
        ("d" . unison-ts-send-definition)))

;; _____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in './eon-core.el'

(use-package eglot :ensure nil

  :custom

  ;; A longer timeout may be required for the first run in a new project
  (eglot-connect-timeout 60)  ; default: 30

  :config

  ;; Connection to the headless UCM process
  (add-to-list 'eglot-server-programs '(unison-ts-mode . ("127.0.0.1" 5757)))

  :hook

  ;; Start language server automatically
  (unison-ts-mode . eglot-ensure)

  ;; Tell the language server to format the buffer before saving
  (unison-ts-mode . (lambda ()
                      (add-hook 'before-save-hook
                                #'eglot-format-buffer nil 'local))))

;; _____________________________________________________________________________
(provide 'eon-lang-unison)
;;; eon-lang-unison.el ends here
