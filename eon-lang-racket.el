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
  ;; Use `racket-hash-lang-mode' for other syntaxes
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
;;; ORG-MODE BABEL
;; <https://www.orgmode.org/worg/org-contrib/babel/languages/ob-doc-scheme.html>

;; Support literate programming in Emacs with Racket
;; Evaluate Racket code in Org blocks via "C-c C-c"
;; (use-package org :ensure nil
;;   :config
;;   (add-to-list 'org-babel-load-languages '(racket . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                org-babel-load-languages))

;; _____________________________________________________________________________
(provide 'eon-lang-racket)
;;; eon-lang-racket.el ends here
