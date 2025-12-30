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

  :custom

  ;; Launch a REPL whenever a Scheme file is visited?
  (geiser-mode-start-repl-p nil)
  ;; Complete or indent by hitting "TAB" depending on the cursor position?
  (geiser-mode-smart-tab-p t)
  ;; Normally Geiser sets this according to what implementation packages are
  ;; installed. We set it to nil, so that it can be set by the Eon Scheme
  ;; modules that are actually enabled.
  (geiser-active-implementations nil)

  :config

  ;; Interactive helper to set the default scheme implementation.
  ;; Helpful & quick workaround when something isn't working as expected,
  ;; and a local `geiser-set-scheme' doesn't resolve the issue.
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
    ;; Set Emacs' default scheme implementation too; for `run-scheme' command.
    (setopt scheme-program-name impl)
    (message "Default Scheme set to: %s" impl)))

  :bind

  (:map eon-localleader-geiser-map
        ("c"   . geiser-compile-definition)
        ("C"   . geiser-compile-current-buffer)
        ("C-c" . geiser-compile-file)
        ("e"   . geiser-eval-last-sexp)
        ("E"   . geiser-eval-buffer)
        ("r"   . geiser-eval-region)
        ("l"   . geiser-load-file)
        ("L"   . geiser-load-current-buffer)
        ("C-l" . geiser-add-to-load-path)
        ("h"   . geiser-doc-symbol-at-point)
        ("H"   . geiser-doc-look-up-manual)
        ("C-h" . geiser-doc-module)
        ("C-r" . geiser-restart-repl)
        ("x"   . geiser-eval-definition)
        ("<"   . geiser-xref-callers)
        (">"   . geiser-xref-callees)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; GEISER REPL

(use-package geiser-repl :ensure nil

  :init

  (eon-localleader-defkeymap geiser-repl-mode eon-localleader-geiser-repl-map
    :doc "Local leader keymap for Geiser REPLs.")

  :custom

  ;; Evaluate Scheme form via "RET" regardless where the cursor currently is?
  ;; Use "C-j" to just open a new line in the REPL without evaluation.
  (geiser-repl-send-on-return-p t)
  ;; Open the REPL in a separate window?
  (geiser-repl-use-other-window t)
  ;; Whether to spawn a separate REPL per project
  (geiser-repl-per-project-p t)
  ;; Roughly double the REPL history; defaults to 500 entries.
  ;; History is saved in the file `geievser-repl-history-filename'.
  (geiser-repl-history-size 1024)

  :bind

  (:map eon-localleader-geiser-repl-map
        ("h"   . geiser-doc-symbol-at-point)
        ("C-r" . geiser-repl-restart-repl)))

;; _____________________________________________________________________________
;;; MACRO STEPPER
;; <https://github.com/nbfalcon/macrostep-geiser>

;; Macro stepper in Scheme src buffers
(use-package macrostep-geiser :ensure t
  :after geiser-mode
  :config (add-hook 'geiser-mode-hook #'macrostep-geiser-setup)
  :bind
  (:map eon-localleader-geiser-map
        ("m" . macrostep-expand)
        ("M" . macrostep-geiser-expand-all)))

;; Macro stepper in Scheme REPLs
(use-package macrostep-geiser :ensure t
  :after geiser-repl
  :config (add-hook 'geiser-repl-mode-hook #'macrostep-geiser-setup)
  :bind
  (:map eon-localleader-geiser-repl-map
        ("m" . macrostep-expand)
        ("M" . macrostep-geiser-expand-all)))

;; _____________________________________________________________________________
;;; SRFI BROWSER
;; <https://github.com/srfi-explorations/emacs-srfi>

(use-package srfi :ensure t
  :bind
  (:map eon-localleader-geiser-map
        ;; SRFI search interface
        ("C-s" . srfi))
  (:map eon-localleader-geiser-repl-map
        ;; SRFI search interface
        ("C-s" . srfi)))

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
