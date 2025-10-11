;;; eon-lang-commonlisp.el --- Common Lisp / Sly  -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
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
  ;; Create local leader maps
  (eon-localleader-defkeymap
      sly-editing-mode eon-localleader-sly-editing-map
    :doc "Local leader keymap for `sly-editing-mode'.")
  (eon-localleader-defkeymap
      sly-mrepl-mode eon-localleader-sly-mrepl-map
    :doc "Local leader keymap for `sly-mrepl-mode'.")
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
  ;; Prevent these buffers from cluttering certain buffer listings:
  (eon-boring-buffers-add '("\\`\\*sly-inferior-lisp"
                            "\\`\\*sly-compilation"
                            "\\`\\*sly-description"
                            "\\`\\*sly-events"))
  ;; :hook
  ;; Automatically start a Lisp REPL when opening a Lisp buffer
  ;; (sly-editing-mode . (lambda ()
  ;;                       (unless (sly-connected-p) (save-excursion (sly)))))
  :bind
  (:map ctl-z-e-map
        ("l" . sly))
  (:map eon-localleader-sly-editing-map
        ;; TODO Add ergonomic keybindings
        ("c" . sly-compile-defun)))

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

(when (eon-modulep 'eon-smartparens)
  (use-package smartparens :ensure t
    :when (featurep 'eon-smartparens)
    :hook
    (sly-editing-mode . smartparens-strict-mode)
    (sly-mrepl-mode . smartparens-mode)))

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
(provide 'eon-lang-commonlisp)
;;; eon-lang-commonlisp.el ends here
