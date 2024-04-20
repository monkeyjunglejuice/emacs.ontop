;;; ontop-racket.el --- Racket configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-racket.el")'.

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
;;; RACKET-MODE
;; <https://www.racket-mode.com/>

(use-package racket-mode
  :ensure t
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

(use-package racket-xp
  :ensure nil
  :config
  (setq-local racket-xp-add-binding-faces t)
  :hook
  ((racket-mode racket-hash-lang-mode) . racket-xp-mode))

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

;; Smartparens non-strict mode is already enabled globally
;; and configured in `ontop-core.el'

(use-package smartparens
  :ensure t
  :hook
  (racket-mode . smartparens-strict-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `ontop-core.el'

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :ensure t
;;   :hook
;;   ((racket-mode racket-hash-lang-mode racket-repl-mode)
;;    . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://www.orgmode.org/worg/org-contrib/babel/languages/ob-doc-scheme.html>

;; Support literate programming in Emacs with Racket
;; Evaluate Racket code in Org blocks via "C-c C-c"
'(use-package org
   :ensure nil
   :hook
   (org-mode . (lambda ()
                 (org-babel-do-load-languages
                  'org-babel-load-languages
                  (add-to-list 'org-babel-load-languages '(racket . t))))))

;;  ____________________________________________________________________________
(provide 'ontop-racket)
;;; ontop-racket.el ends here
