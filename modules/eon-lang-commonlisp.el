;;; eon-lang-commonlisp.el --- Common Lisp / Sly  -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; COMMON LISP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

(use-package inf-lisp :ensure nil
  :custom
  ;; Set default Lisp implementation
  (inferior-lisp-program "sbcl"))

;; _____________________________________________________________________________
;;; SLY
;; <http://joaotavora.github.io/sly/>
;; <https://github.com/joaotavora/sly>

(use-package sly :ensure t

  :init

  ;; Create local leader maps
  (eon-localleader-defkeymap sly-editing-mode eon-localleader-sly-editing-map
    :doc "Local leader keymap for `sly-editing-mode'.")

  (eon-localleader-defkeymap sly-mrepl-mode eon-localleader-sly-mrepl-map
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
(use-package sly-asdf :ensure t
  :after sly)

;; <https://github.com/joaotavora/sly-macrostep>
(use-package sly-macrostep :ensure t
  :after sly)

;; <https://github.com/joaotavora/sly-named-readtables>
(use-package sly-named-readtables :ensure t
  :after sly)

;; <https://github.com/joaotavora/sly-quicklisp>
(use-package sly-quicklisp :ensure t
  :after sly-mrepl
  :config
  (add-to-list 'sly-mrepl-shortcut-alist '("quickload" . sly-quickload)))

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
  ;; Make the function aware of Sly (defaults to Slime)
  (org-babel-lisp-eval-fn #'sly-eval))

;; _____________________________________________________________________________
(provide 'eon-lang-commonlisp)
;;; eon-lang-commonlisp.el ends here
