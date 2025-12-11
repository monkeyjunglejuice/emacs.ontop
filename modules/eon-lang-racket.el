;;; eon-lang-racket.el --- Racket -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; RACKET-MODE
;; <https://www.racket-mode.com/>

(use-package racket-mode :ensure t

  :init

  ;; Use `racket-hash-lang-mode' for other syntax
  (add-to-list 'auto-mode-alist '("\\.scrbl\\'" . racket-hash-lang-mode))
  (add-to-list 'auto-mode-alist '("\\.rhm\\'" . racket-hash-lang-mode))

  (eon-localleader-defkeymap racket-mode eon-localleader-racket-map
    :doc "Local leader keymap for the Racket major mode.")

  :custom

  ;; Depending on your OS environment, the full path may be required
  (racket-program (if (eon-winp) "racket.exe" "racket"))

  ;; List of command-line arguments to supply to the Racket program
  ;; (racket-user-command-line-arguments ("-f" "file.rkt"))
  ;; Terminate the Racket process if memory use exceeds this value in MiB
  (racket-memory-limit 1024)  ; 0 = unlimited
  (racket-documentation-search-location 'local)

  :bind

  (:map eon-localleader-racket-map
        ("g"   . racket-edit-switch-to-repl)
        ("e"   . racket-send-last-sexp)
        ("r"   . racket-send-region)
        ("x"   . racket-send-definition)
        ("c"   . racket-run-module-at-point)
        ("C"   . racket-run)
        ("h"   . racket-xp-describe)
        ("H"   . racket-describe-search)
        ("p"   . describe-racket-package)
        ("P"   . list-racket-packages)
        ("C-p" . racket-package-refresh)
        ("y"   . racket-insert-lambda)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; HASH-LANG MODE

(use-package racket-hash-lang :ensure nil

  :init

  (eon-localleader-defkeymap
      racket-hash-lang-mode eon-localleader-racket-hash-lang-map
    :doc "Local leader keymap for the Racket hash-lang major mode.")

  :bind

  (:map eon-localleader-racket-hash-lang-map
        ("g"   . racket-edit-switch-to-repl)
        ("e"   . racket-send-last-sexp)
        ("r"   . racket-send-region)
        ("x"   . racket-send-definition)
        ("c"   . racket-run-module-at-point)
        ("C"   . racket-run)
        ("h"   . racket-xp-describe)
        ("H"   . racket-describe-search)
        ("p"   . describe-racket-package)
        ("P"   . list-racket-packages)
        ("C-p" . racket-package-refresh)
        ("y"   . racket-insert-lambda)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; REPL

(use-package racket-repl :ensure nil

  :init

  (eon-localleader-defkeymap racket-repl-mode eon-localleader-racket-repl-map
    :doc "Local leader keymap for the Racket REPL major mode.")

  :bind
  (:map eon-localleader-racket-repl-map
        ("g" . racket-repl-switch-to-edit)
        ("h" . racket-repl-describe)
        ("H" . racket-describe-search)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; SMART BRACKET MODE
;; Use the easy-to-reach '[' and ']' keys for everything.

(use-package racket-smart-open :ensure nil

  :hook

  ((racket-hash-lang-mode racket-mode racket-repl-mode)
   . racket-smart-open-bracket-mode))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; CODE AUGMENTATION

(use-package racket-xp :ensure nil

  :config

  (setq-local racket-xp-add-binding-faces t)

  :hook

  ((racket-mode racket-hash-lang-mode) . racket-xp-mode))

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode.
;; Not enabled in `racket-hash-lang-mode' per default,
;; as this is a source code buffer mode for arbitrary languages/syntaxes.
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    (racket-mode . aggressive-indent-mode)))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://org-babel.readthedocs.io/en/latest/eval/>
;; Notebook-like literate programming in Emacs

;; Evaluate Racket code in Org source code blocks via "C-c C-c"
;; <https://github.com/hasu/emacs-ob-racket>
(use-package ob-racket
  :vc (:url "https://github.com/hasu/emacs-ob-racket.git"
            :rev :newest)
  :after org
  :hook
  (ob-racket-pre-runtime-library-load . ob-racket-raco-make-runtime-library))

;; _____________________________________________________________________________
(provide 'eon-lang-racket)
;;; eon-lang-racket.el ends here
