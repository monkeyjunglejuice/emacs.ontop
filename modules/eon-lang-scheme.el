;;; eon-lang-scheme.el --- Scheme / Geiser -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Common definitions and functionality for all Scheme implementation modules.
;; This module will be loaded automatically by the implementation-specific
;; Scheme modules, even if commented out in `eon-setup-modules'.
;; 
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(use-package geiser :ensure t
  :custom
  (geiser-repl-send-on-return-p t)
  (geiser-repl-use-other-window t)
  (scheme-mit-dialect nil))

;; _____________________________________________________________________________
;;; MACRO STEPPER
;; <https://github.com/nbfalcon/macrostep-geiser>

(use-package macrostep-geiser :ensure t
  :after geiser-mode
  :config (add-hook 'geiser-mode-hook #'macrostep-geiser-setup))

(use-package macrostep-geiser :ensure t
  :after geiser-repl
  :config (add-hook 'geiser-repl-mode-hook #'macrostep-geiser-setup))

;; _____________________________________________________________________________
;;; SRFI BROWSER
;; <https://github.com/srfi-explorations/emacs-srfi>

(use-package srfi :ensure t)

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    (scheme-mode . aggressive-indent-mode)))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate Scheme code in Org source code blocks via "C-c C-c"

(use-package ob-scheme :ensure nil
  :after org)

;; _____________________________________________________________________________
(provide 'eon-lang-scheme)
;;; eon-lang-scheme.el ends here
