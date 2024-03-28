;;; ontop-julia.el --- Julia configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-julia.el")'.

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
;;; JULIA TREESITTER MODE
;; <https://github.com/ronisbr/julia-ts-mode>

(use-package julia-ts-mode
  :ensure t
  :mode "\\.jl$")

;; In order to use Tree Sitter, install the tree-sitter binary with your OS
;; package manager. Then install the language grammar via
;; 'M-x treesit-install-language-grammar'
(use-package treesit
  :ensure nil
  :config
  (add-to-list 'treesit-language-source-alist
               '(julia "https://github.com/tree-sitter/tree-sitter-julia")))

;;  ____________________________________________________________________________
;;; JULIA SNAIL
;; <https://github.com/gcv/julia-snail/>
;; Interactive Julia with REPL

(use-package julia-snail
  :ensure t
  :custom
;; Needs a terminal emulator within Emacs; alternative: vterm
  (julia-snail-terminal-type :eat)
  ;; Print the result of evaluating code to the REPL
  (julia-snail-repl-display-eval-results t)  ; `nil' to disable
  ;; Show result of evaluating code in the source buffer
  (julia-snail-popup-display-eval-results nil)  ; `:command', `:change' or `nil'
  ;; The default works with Consult and Helm
  (julia-snail-imenu-style :module-tree)  ; `:module-tree', `:flat' or `nil'
  :hook
  (julia-ts-mode . julia-snail-mode))

;;  ____________________________________________________________________________
;;; EAT
;; <https://codeberg.org/akib/emacs-eat>
;; <https://elpa.nongnu.org/nongnu-devel/doc/eat.html>
;; Julia Snail requires a terminal emulator within Emacs for the REPL

(use-package eat
  :ensure t
  :custom
  (eat-kill-buffer-on-exit t)
  :config
  ;; make C-u work in Eat terminals like in normal terminals
  (delete [?\C-u] eat-semi-char-non-bound-keys)
  ;; ditto for C-g
  (delete [?\C-g] eat-semi-char-non-bound-keys)
  (eat-update-semi-char-mode-map)
  ;; XXX: Awkward workaround for the need to call eat-reload after changing
  ;; Eat's keymaps, but reloading from :config section causes infinite recursion
  ;; because :config wraps with-eval-after-load.
  (defvar eat--prevent-use-package-config-recursion nil)
  (unless eat--prevent-use-package-config-recursion
    (setq eat--prevent-use-package-config-recursion t)
    (eat-reload))
  (makunbound 'eat--prevent-use-package-config-recursion))

;;  ____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md>
;; Common keybindings are configured in `./ontop-shared.el'

;; Setup Eglot for Julia and install the language server binary if necessary
;; <https://github.com/non-Jedi/eglot-jl>
(use-package eglot-jl
  :ensure t
  :init
  (eglot-jl-init))

(use-package eglot
  :ensure t
  :hook
  ;; Start language server automatically
  (julia-ts-mode . eglot-ensure)
  ;; Tell the language server to format the buffer before saving
  (julia-ts-mode . (lambda ()
                     (add-hook 'before-save-hook
                               #'eglot-format-buffer nil 'local))))

;;  ____________________________________________________________________________
;;; INDENTATION
;; <https://github.com/Malabarba/aggressive-indent-mode>

;; ;; Reindent immediately after change
;; (use-package aggressive-indent
;;   :ensure t
;;   :hook
;;   (julia-ts-mode . aggressive-indent-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://www.orgmode.org/worg/org-contrib/babel/languages/ob-doc-scheme.html>
;; Notebook-like literate programming in Emacs

;; Evaluate Julia code in Org source code blocks via "C-c C-c".
;; 2 Julia packages must be added for this to work: DataFrames and CSV.
;; <https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-julia.html>
(use-package org
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 (add-to-list 'org-babel-load-languages '(julia . t))))))

;;  ____________________________________________________________________________
(provide 'ontop-julia)
;;; ontop-julia.el ends here
