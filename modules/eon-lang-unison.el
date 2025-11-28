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

(use-package unison-ts-mode
  :vc (:url "https://github.com/fmguerreiro/unison-ts-mode.git"
            :rev :newest)
  :mode
  ("\\.u\\'" "\\.unison\\'")
  :custom
  (unison-ts-grammar-install 'auto)
  (unison-ts-grammar-repository
   "https://github.com/fmguerreiro/tree-sitter-unison" ))

;; _____________________________________________________________________________
;;; LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in './eon-core.el'

(use-package eglot :ensure nil
  :custom
  ;; A longer timeout may be required for the first run in a new project
  (eglot-connect-timeout 30)  ; default: 30
  :hook
  ;; Start language server automatically (UCM in headless mode)
  (unison-ts-mode . eglot-ensure)
  ;; Tell the language server to format the buffer before saving
  (unison-ts-mode . (lambda ()
                      (add-hook 'before-save-hook
                                #'eglot-format-buffer nil 'local))))

;; _____________________________________________________________________________
(provide 'eon-lang-unison)
;;; eon-lang-unison.el ends here
