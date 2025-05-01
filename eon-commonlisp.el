;;; eon-commonlisp.el --- Common Lisp with Sly  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-commonlisp.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; COMMON LISP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

(use-package inf-lisp
  :ensure nil
  :custom
  ;; Set default Lisp implementation
  (inferior-lisp-program "ros -Q run"))

;;  ____________________________________________________________________________
;;; SLY
;; <http://joaotavora.github.io/sly/>
;; <https://github.com/joaotavora/sly>

(use-package sly
  :init
  ;; Set Sly Lisp implementations
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--no-inform") :coding-system utf-8-unix)))
  :custom
  ;; Select Sly default Lisp implementation
  (sly-default-lisp 'sbcl)
  ;; General configuration
  (sly-net-coding-system 'utf-8-unix)
  ;; :hook
  ;; Automatically start a Lisp REPL when opening a Lisp buffer
  ;; (sly-mode . (lambda ()
  ;;               (unless (sly-connected-p) (save-excursion (sly)))))
  :config
  ;; Prevent these buffers from cluttering certain buffer lists:
  (when (boundp 'eon-boring-buffers)
    (add-to-list 'eon-boring-buffers "\\`\\*sly-inferior-lisp")
    (add-to-list 'eon-boring-buffers "\\`\\*sly-events")))

;; Common Lisp documentation
;; The hyperspec must be installed on your computer. Adapt the path below:
(use-package hyperspec
  :ensure nil
  :after sly
  :custom
  (common-lisp-hyperspec-root
   ;; Location when installed on MacOS via Homebrew
   "/usr/local/share/doc/hyperspec/HyperSpec/")
  (common-lisp-hyperspec-symbol-table
   (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
  (common-lisp-hyperspec-issuex-table
   (concat common-lisp-hyperspec-root "Data/Map_IssX.txt")))

;; <https://github.com/mmgeorge/sly-asdf>
(use-package sly-asdf)

;; <https://github.com/joaotavora/sly-macrostep>
(use-package sly-macrostep)

;; <https://github.com/joaotavora/sly-named-readtables>
(use-package sly-named-readtables)

;; <https://github.com/joaotavora/sly-quicklisp>
(use-package sly-quicklisp
  :after sly-mrepl
  :config
  (add-to-list 'sly-mrepl-shortcut-alist '("quickload" . sly-quickload)))

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

;; Smartparens non-strict mode is already enabled globally
;; and configured in `eon-core.el'

;; Enable strict mode in Lisp buffers
(use-package smartparens
  :hook
  (lisp-mode . smartparens-strict-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters
  :hook
  (sly-mrepl-mode . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   ((lisp-mode inferior-lisp-mode sly-mrepl-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/intro.html>
;; Support literate programming in Emacs with Common Lisp
;; Evaluate Common Lisp code in Org source code blocks via "C-c C-c"

;; Make the function aware of Sly (defaults to Slime)
;; <https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lisp.html>
(use-package ob-lisp
  :ensure nil
  :custom
  (org-babel-lisp-eval-fn #'sly-eval))

(use-package org
  :ensure nil
  :hook
  (org-mode . (lambda () (org-babel-do-load-languages
                          'org-babel-load-languages
                          (add-to-list 'org-babel-load-languages
                                       '(lisp . t))))))

;;  ____________________________________________________________________________
(provide 'eon-commonlisp)
;;; eon-commonlisp.el ends here
