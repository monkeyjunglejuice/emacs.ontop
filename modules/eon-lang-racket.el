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

  :custom

  ;; Depending on your OS environment, the full path may be required
  (racket-program "racket")

  ;; List of command-line arguments to supply to the Racket program
  ;; (racket-user-command-line-arguments ("-f" "file.rkt"))
  ;; Terminate the Racket process if memory use exceeds this value in MB
  (racket-memory-limit 1024)  ; 0 = unlimited
  (racket-documentation-search-location 'local)

  :hook

  ;; Use the easy-to-reach `[' and `]' keys for everything.
  ((racket-hash-lang-mode racket-mode racket-repl-mode)
   . racket-smart-open-bracket-mode))

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
