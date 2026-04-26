;;; eon-lang-commonlisp-slime.el --- Common Lisp / Slime  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience languages lisp slime
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;;; Code:

(eon-module-metadata
 :conflicts '(eon-lang-commonlisp-sly)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; COMMON LISP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

(use-package inf-lisp :ensure nil
  :custom
  ;; Set default Lisp implementation
  (inferior-lisp-program "sbcl"))

;; _____________________________________________________________________________
;;; SLIME
;; <https://github.com/slime/slime>
;; <https://slime.common-lisp.dev>

(use-package slime :ensure t

  :init

  ;; Create local leader maps
  (eon-localleader-defkeymap slime-editing-mode eon-localleader-slime-editing-map
    :doc "Local leader keymap for `slime-editing-mode'.")

  (eon-localleader-defkeymap slime-repl-mode eon-localleader-slime-repl-map
    :doc "Local leader keymap for `slime-repl-mode'.")

  ;; Enable Slime contrib packages
  (setq slime-contribs '(slime-fancy
                         slime-asdf
                         slime-quicklisp
                         slime-sprof
                         slime-xref-browser
                         slime-coalton
                         ))
  
  ;; Set Slime Lisp implementations
  (setq slime-lisp-implementations
        '((sbcl ("sbcl"
                 "--noinform"
                 ;; Size of control stack reserved for each thread - default 2
                 ;; "The Stack" for function calls etc.
                 "--control-stack-size" "16"  ; MiB
                 ;; Size of dynamic space reserved on startup - default 1024
                 ;; "The Heap", global main memory area for Lisp objects
                 "--dynamic-space-size" "8192")  ; MiB
                :coding-system utf-8-unix)
          ))

  :custom

  ;; Select Slime default Lisp implementation
  (slime-default-lisp 'sbcl)
  ;; General configuration
  (slime-net-coding-system 'utf-8-unix)
  ;; Select description window on display
  (slime-description-autofocus t)
  ;; Default formatting style
  (common-lisp-style-default 'modern)

  :config
  
  ;; Setup Emacs so that lisp-mode buffers always use Slime
  (slime-setup slime-contribs)

  ;; Prevent these buffers from cluttering certain buffer listings:
  (eon-boring-buffers-add '("\\`\\*inferior-lisp"
                            "\\`\\*slime-compilation"
                            "\\`\\*slime-description"
                            "\\`\\*slime-events"))

  ;; :hook

  ;; Automatically start a Lisp REPL when opening a Lisp buffer
  ;; (slime-editing-mode . (lambda ()
  ;;                       (unless (slime-connected-p) (save-excursion (slime)))))

  :bind

  (:map eon-localleader-slime-editing-map
        ;; TODO Add ergonomic keybindings to the local leader map
        ("c" . slime-compile-defun)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; Common Lisp documentation

;; The hyperspec must be installed on your computer.
;; To adapt the path, set the variable `common-lisp-hyperspec-root'.
(use-package hyperspec :ensure nil
  :after slime
  :custom
  (common-lisp-hyperspec-root
   (concat "file://" (expand-file-name "~/common-lisp/.hyperspec/HyperSpec/")))
  (common-lisp-hyperspec-symbol-table
   (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
  (common-lisp-hyperspec-issuex-table
   (concat common-lisp-hyperspec-root "Data/Map_IssX.txt")))

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    (lisp-mode . aggressive-indent-mode)))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/intro.html>
;; Support literate programming in Emacs with Common Lisp
;; Evaluate Common Lisp code in Org source code blocks via "C-c C-c"

;; <https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lisp.html>
(use-package ob-lisp :ensure nil
  :after org
  :custom
  ;; Make the function aware of Slime
  (org-babel-lisp-eval-fn #'slime-eval))

;; _____________________________________________________________________________
(provide 'eon-lang-commonlisp-slime)
;;; eon-lang-commonlisp-slime.el ends here
