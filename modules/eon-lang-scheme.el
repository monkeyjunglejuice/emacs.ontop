;;; eon-lang-scheme.el --- Scheme / Geiser -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Common definitions and functionality for all Scheme implementation modules.
;; This module will be loaded automatically by the implementation-specific
;; Scheme modules, even if commented out in your 'eon-setup-modules.el'.
;;
;;; Code:

;; _____________________________________________________________________________
;;; SCHEME

(use-package scheme :ensure nil
  :custom
  ;; Don't enable functionality for all Schemes; will be set by the module
  ;; 'eon-lang-scheme-mit.el'.
  (scheme-mit-dialect nil))

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(use-package geiser :ensure t

  :init

  (eon-localleader-defkeymap geiser-mode eon-localleader-geiser-map
    :doc "Local leader keymap for the Geiser minor mode in Scheme src buffers.")

  (eon-localleader-defkeymap geiser-repl-mode eon-localleader-geiser-repl-map
    :doc "Local leader keymap for the Geiser REPL.")

  :custom

  (geiser-repl-send-on-return-p t)
  (geiser-repl-use-other-window t)
  (geiser-repl-history-size 1024)
  (geiser-mode-smart-tab-p t)
  (geiser-mode-smart-repl-p t)
  ;; Normally Geiser sets this according to what implementation packages are
  ;; installed. We set it to nil, so that it can be set according to what
  ;; modules are actually enabled.
  (geiser-active-implementations nil)

  :config
  ;; Interactive helper to set the default scheme implementation.
  (defun eon-geiser-set-default-scheme ()
  "Select and set `geiser-default-implementation' interactively.
The list of candidates comes from `geiser-active-implementations'."
  (interactive)
  (let* ((impls geiser-active-implementations)
         (name  (completing-read "Default Scheme implementation: "
                                 (mapcar #'symbol-name impls)
                                 nil t))
         (impl  (intern name)))
    (setopt geiser-default-implementation impl)
    (message "Default Scheme set to: %s" impl))))

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

;; `ob-scheme' uses the `geiser-default-implementation' to execute src blocks.
;; In case your src blocks don't work, make sure that this variable is set
;; to your desired Scheme implementation - either permanently or temporarily
;; via "M-x eon-geiser-set-default-scheme".
(use-package ob-scheme :ensure nil
  :after org)

;; _____________________________________________________________________________
(provide 'eon-lang-scheme)
;;; eon-lang-scheme.el ends here
