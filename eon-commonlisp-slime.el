;;; eon-commonlisp-slime.el --- Common Lisp with Slime  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-commonlisp-slime.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; COMMON LISP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

(use-package inf-lisp :ensure nil
  :custom
  ;; Set default Lisp implementation
  (inferior-lisp-program "sbcl"))

;;  ____________________________________________________________________________
;;; SLIME

(use-package slime :ensure t
  :init
  ;; Set Slime Lisp implementations
  (setq slime-lisp-implementations
        '((sbcl ("sbcl"
                 "--noinform"
                 "--control-stack-size" "4096"
                 "--binding-stack-size" "512"
                 "--dynamic-space-size" "8192")
                :coding-system utf-8-unix)))
  :custom
  ;; Select Slime default Lisp implementation
  (slime-default-lisp 'sbcl)
  ;; General configuration
  (slime-net-coding-system 'utf-8-unix)
  ;; Completion
  (slime-completion-at-point-functions '(slime-c-p-c-completion-at-point
                                         slime-filename-completion
                                         slime-simple-completion-at-point))
  :config
  ;; Slime contribs
  ;; Loaded by `slime-fancy' meta-contrib:
  ;; slime-repl
  ;; slime-autodoc
  ;; slime-c-p-c
  ;; slime-editing-commands
  ;; slime-fancy-inspector
  ;; slime-fancy-trace
  ;; slime-fuzzy
  ;; slime-mdot-fu
  ;; slime-macrostep
  ;; slime-presentations
  ;; slime-scratch
  ;; slime-references
  ;; slime-package-fu
  ;; slime-fontifying-fu
  ;; slime-trace-dialog
  ;; slime-indentation
  (setq slime-contribs '(slime-fancy
                         slime-asdf
                         slime-quicklisp
                         slime-mrepl
                         slime-highlight-edits
                         slime-media
                         slime-sbcl-exts
                         slime-scheme
                         ;; BUG https://github.com/slime/slime/issues/887#issue-3414492176
                         ;; slime-snapshot
                         slime-sprof))
  
  ;; Prevent some buffers from cluttering certain buffer lists:
  (when (boundp 'eon-boring-buffers)
    (add-to-list 'eon-boring-buffers "\\`\\*inferior-lisp")
    (add-to-list 'eon-boring-buffers "\\`\\*slime-events")
    (add-to-list 'eon-boring-buffers "\\`\\*slime-description"))

  (add-to-list 'display-buffer-alist
               '("\\*slime-description" (display-buffer-no-window)))

  ;; :hook
  ;; Automatically start a Lisp REPL when opening a Lisp buffer
  ;; (slime-mode . (lambda ()
  ;;               (unless (slime-connected-p) (save-excursion (sly)))))
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

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

(use-package smartparens :ensure t
  :when (featurep 'eon-smartparens)
  :hook
  (lisp-mode . smartparens-strict-mode)
  ((slime-repl-mode slime-mrepl-mode) . smartparens-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters :ensure t
  :hook
  ((slime-repl-mode slime-mrepl-mode) . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   ((lisp-mode inferior-lisp-mode slime-repl-mode slime-mrepl-mode)
;;    . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/intro.html>
;; Support literate programming in Emacs with Common Lisp
;; Evaluate Common Lisp code in Org source code blocks via "C-c C-c"

;; Make the function aware of Sly (defaults to Slime)
;; <https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lisp.html>
(use-package ob-lisp :ensure nil
  :custom
  (org-babel-lisp-eval-fn #'slime-eval))

(use-package org :ensure nil
  :config
  (add-to-list 'org-babel-load-languages '(lisp . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

;;  ____________________________________________________________________________
(provide 'eon-commonlisp-slime)
;;; eon-commonlisp-slime.el ends here
