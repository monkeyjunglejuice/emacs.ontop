;;; ontop-haskell.el --- Haskell configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-haskell.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;;  <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package nil))

(eval-when-compile
  (require 'use-package))

;;  ____________________________________________________________________________
;;; HASKELL-MODE
;;  <http://haskell.github.io/haskell-mode>

(use-package haskell-mode
  :ensure t
  :custom
  (haskell-completing-read-function 'completing-read)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-suggest-hoogle-imports t)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-type 'auto)
  ;; Don't use hasktags; the language server provides that functionality
  (haskell-tags-on-save nil)
  ;; Don't use haskell-mode to show docs; the language server will do that
  (haskell-doc-show-global-types nil)
  (haskell-doc-show-prelude nil)
  (haskell-doc-show-reserved nil)
  (haskell-doc-show-strategy nil)
  (haskell-doc-show-user-defined nil)
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package haskell
  :ensure nil
  :bind
  ;; Reach REPL from anywhere via global key binding
  (:map ctl-z-x-map
        ("h" . haskell-interactive-bring))
  (:map interactive-haskell-mode-map
        ("C-c C-c" . haskell-compile)
        ("C-c C-e" . haskell-process-load-file)))

;;  ____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;;  <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;;  <https://haskell-language-server.readthedocs.io/en/latest/configuration.html>
;;  Common keybindings are configured in `./ontop-shared.el'

(use-package eglot
  :ensure t
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (formatting-provider . "ormolu")
                   )))
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

;;  ____________________________________________________________________________
(provide 'ontop-haskell)
;;; ontop-haskell.el ends here
