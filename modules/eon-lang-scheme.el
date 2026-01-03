;;; eon-lang-scheme.el --- Scheme / Geiser -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Common definitions and functionality for all Scheme implementation modules.
;; This module will be loaded automatically if an implementation-specific
;; module is enabled, even if commented out in your 'eon-setup-modules.el'.
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
  :defer t

  :init

  (eon-localleader-defkeymap geiser-mode eon-localleader-geiser-map
    :doc "Local leader keymap for the Geiser minor mode in Scheme src buffers.")

  (eon-localleader-defkeymap geiser-repl-mode eon-localleader-geiser-repl-map
    :doc "Local leader keymap for Geiser REPLs.")

  (eon-localleader-defkeymap geiser-doc-mode eon-localleader-geiser-doc-map
    :doc "Local leader keymap for Geiser docs.")

  (eon-localleader-defkeymap geiser-debug-mode eon-localleader-geiser-debug-map
    :doc "Local leader keymap for Geiser debugger.")

  :custom

  ;;; - Source code buffers
  ;; Launch a REPL whenever a Scheme file is visited?
  (geiser-mode-start-repl-p nil)
  ;; Complete or indent by hitting "TAB" depending on the cursor position?
  (geiser-mode-smart-tab-p t)
  ;; Normally Geiser sets this according to what implementation packages are
  ;; installed. We set it to nil, so that it can be set by the Eon Scheme
  ;; modules that are actually enabled.
  (geiser-active-implementations nil)

  ;;; - REPL
  ;; Evaluate Scheme form via "RET" regardless where the cursor currently is?
  ;; Use "C-j" to just open a new line in the REPL without evaluation.
  (geiser-repl-send-on-return-p t)
  ;; Open the REPL in a separate window?
  (geiser-repl-use-other-window t)
  ;; Whether to spawn a separate REPL per project
  (geiser-repl-per-project-p t)
  ;; Roughly double the REPL history; defaults to 500 entries.
  ;; History is saved in the file `geiser-repl-history-filename'.
  (geiser-repl-history-size 1024)

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
      ;; Also set Emacs' default scheme implementation for `run-scheme' command
      (setopt scheme-program-name impl)
      (message "Default Scheme implementation set to: %s" impl)))

  :bind

  (:map eon-localleader-geiser-map
        ;; REPL
        ("RET" . geiser-repl-switch-to-module)
        ("g"   . geiser-mode-switch-to-repl)
        ("i"   . geiser-repl-import-module)
        ("k"   . geiser-repl-interrupt)
        ("C-r" . geiser-restart-repl)
        ;; Compilation
        ("c"   . geiser-compile-definition)
        ("C-b" . geiser-compile-current-buffer)
        ("C-f" . geiser-compile-file)
        ;; Evaluation
        ("e"   . geiser-eval-last-sexp)
        ("b"   . geiser-eval-buffer)
        ("r"   . geiser-eval-region)
        ("x"   . geiser-eval-definition)
        ("K"   . geiser-eval-interrupt)
        ;; Documentation
        ("h"   . geiser-doc-symbol-at-point)
        ("H"   . geiser-doc-look-up-manual)
        ("C-h" . geiser-doc-module)
        ;; Loading
        ("l"   . geiser-load-current-buffer)
        ("L"   . geiser-load-file)
        ("C-l" . geiser-add-to-load-path)
        ;; References
        ("<"   . geiser-xref-callers)
        (">"   . geiser-xref-callees)
        ;; Implementation
        ("C-s" . geiser-set-scheme)
        ("M-s" . eon-geiser-set-default-scheme))

  (:map eon-localleader-geiser-repl-map
        ("RET" . geiser-repl-switch-to-module)
        ("C-r" . geiser-repl-restart-repl)
        ("g"   . geiser-repl-switch)
        ("h"   . geiser-doc-symbol-at-point)
        ("i"   . geiser-repl-import-module)
        ("k"   . geiser-repl-interrupt)
        ("l"   . geiser-load-file)
        ("C-l" . geiser-add-to-load-path))

  (:map eon-localleader-geiser-doc-map
        ("m" . geiser-doc-goto-manual)
        ("s" . geiser-doc-goto-source)
        ("g" . geiser-doc-switch-to-repl))

  (:map eon-localleader-geiser-debug-map
        ("g" . geiser-debug-switch-to-buffer)))

;; _____________________________________________________________________________
;;; MACRO STEPPER
;; <https://github.com/nbfalcon/macrostep-geiser>

;; Macro stepper in Scheme src buffers
(use-package macrostep-geiser :ensure t
  :after geiser-mode

  :hook

  ((geiser-mode geiser-repl-mode) . macrostep-geiser-setup)

  :bind

  (:map eon-localleader-geiser-map
        ("m" . macrostep-expand)
        ("M" . macrostep-geiser-expand-all))
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
        ("s" . srfi))
  (:map eon-localleader-geiser-repl-map
        ;; SRFI search interface
        ("s" . srfi)))

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
