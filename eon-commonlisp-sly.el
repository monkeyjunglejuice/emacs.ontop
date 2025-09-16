;;; eon-commonlisp-sly.el --- Common Lisp with Sly  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-commonlisp-sly.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; COMMON LISP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

(use-package inf-lisp :ensure nil
  :custom
  ;; Set default Lisp implementation
  (inferior-lisp-program "sbcl"))

;;  ____________________________________________________________________________
;;; SLY
;; <http://joaotavora.github.io/sly/>
;; <https://github.com/joaotavora/sly>

(use-package sly :ensure t
  :init
  ;; Set Sly Lisp implementations
  (setq sly-lisp-implementations
        '((sbcl ("sbcl"
                 "--noinform"
                 "--control-stack-size" "4096"
                 "--binding-stack-size" "512"
                 "--dynamic-space-size" "8192")
                :coding-system utf-8-unix)))
  :custom
  ;; Select Sly default Lisp implementation
  (sly-default-lisp 'sbcl)
  ;; General configuration
  (sly-net-coding-system 'utf-8-unix)
  :config
  ;; Prevent these buffers from cluttering certain buffer lists:
  (when (boundp 'eon-boring-buffers)
    (add-to-list 'eon-boring-buffers "\\`\\*inferior-lisp")
    (add-to-list 'eon-boring-buffers "\\`\\*slime-events")
    (add-to-list 'eon-boring-buffers "\\`\\*slime-description"))
  ;; :hook
  ;; Automatically start a Lisp REPL when opening a Lisp buffer
  ;; (sly-mode . (lambda ()
  ;;               (unless (sly-connected-p) (save-excursion (sly)))))
  )

;; Common Lisp documentation
;; The hyperspec must be installed on your computer. Adapt the path below:
(use-package hyperspec :ensure nil
  :after sly
  :custom
  (common-lisp-hyperspec-root
   (concat "file://" (expand-file-name "~/common-lisp/.hyperspec/HyperSpec/")))
  (common-lisp-hyperspec-symbol-table
   (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
  (common-lisp-hyperspec-issuex-table
   (concat common-lisp-hyperspec-root "Data/Map_IssX.txt")))

;; <https://github.com/mmgeorge/sly-asdf>
(use-package sly-asdf :ensure t)

;; <https://github.com/joaotavora/sly-macrostep>
(use-package sly-macrostep :ensure t)

;; <https://github.com/joaotavora/sly-named-readtables>
(use-package sly-named-readtables :ensure t)

;; <https://github.com/joaotavora/sly-quicklisp>
(use-package sly-quicklisp :ensure t
  :after sly-mrepl
  :config
  (add-to-list 'sly-mrepl-shortcut-alist '("quickload" . sly-quickload)))

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

(use-package smartparens :ensure t
  :when (featurep 'eon-smartparens)
  :hook
  (lisp-mode . smartparens-strict-mode)
  (sly-mrepl-mode . smartparens-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters :ensure t
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
(use-package ob-lisp :ensure nil
  :custom
  (org-babel-lisp-eval-fn #'sly-eval))

(use-package org :ensure nil
  :config
  (add-to-list 'org-babel-load-languages '(lisp . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

;;  ____________________________________________________________________________
(provide 'eon-commonlisp-sly)
;;; eon-commonlisp-sly.el ends here
