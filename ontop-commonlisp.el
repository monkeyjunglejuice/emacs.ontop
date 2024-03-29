;;; ontop-commonlisp.el --- Common Lisp with Sly  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-commonlisp.el")'.

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
;;; COMMON LISP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

(use-package inf-lisp
  :ensure nil
  :custom
  ;; Set default Lisp implementation
  (inferior-lisp-program '("ros" "-Q" "run")))

;; Where is the Common Lisp reference manual?
;; Normally, you can install the Hyperspec with your distro's package manager
(setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")

;;  ____________________________________________________________________________
;;; SLY
;; <http://joaotavora.github.io/sly/>
;; <https://github.com/joaotavora/sly>

(use-package sly
  :ensure t
  :init
  ;; Set Sly Lisp implementations
  (setq sly-lisp-implementations
        '((roswell ("ros" "-Q" "run"))
          (sbcl ("sbcl" "--no-inform") :coding-system utf-8-unix)))
  :custom
  ;; Select Sly default Lisp implementation
  (sly-default-lisp 'roswell)
  ;; General configuration
  (sly-net-coding-system 'utf-8-unix)
  ;; :hook
  ;; Automatically start a Lisp REPL when opening a Lisp buffer
  ;; (sly-mode . (lambda ()
  ;;               (unless (sly-connected-p) (save-excursion (sly)))))
  :config
  ;; Helm integration: don't show these buffers in Helm's buffer list:
  (when (boundp 'eon-boring-buffers)
    (add-to-list 'eon-boring-buffers "\\`\\*sly-inferior-lisp")
    (add-to-list 'eon-boring-buffers "\\`\\*sly-events"))
  :bind
  (:map ctl-z-x-map
        ("l" . sly-mrepl))
  (:map ctl-z-s-map
        ("l" . sly-scratch)))

;; <https://github.com/mmgeorge/sly-asdf>
(use-package sly-asdf
  :ensure t)

;; <https://github.com/joaotavora/sly-macrostep>
(use-package sly-macrostep
  :ensure t)

;; <https://github.com/joaotavora/sly-named-readtables>
(use-package sly-named-readtables
  :ensure t)

;; <https://github.com/joaotavora/sly-quicklisp>
(use-package sly-quicklisp
  :ensure t)

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

;; Lisp buffers
(use-package smartparens
  :ensure t
  :hook
  (lisp-mode . smartparens-strict-mode))

;; Lisp REPLS and other special buffers
(use-package smartparens
  :ensure t
  :hook
  ((inferior-lisp-mode sly-mrepl-mode) . smartparens-mode))

;;  ____________________________________________________________________________
;;; MATCHING PARENTHESIS

;; Emphasize the whole expression enclosed by matching parenthesis
(use-package show-paren
  :ensure nil
  :hook
  ((lisp-mode inferior-lisp-mode sly-mrepl-mode) . show-paren-local-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Color-code nested parens …
;; <https://github.com/Fanael/rainbow-delimiters>
(use-package rainbow-delimiters
  :ensure t
  :hook
  ((lisp-mode inferior-lisp-mode sly-mrepl-mode) . rainbow-delimiters-mode))

;; … and/or make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :ensure t
;;   :hook
;;   ((lisp-mode inferior-lisp-mode sly-mrepl-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; INDENTATION
;; <https://github.com/Malabarba/aggressive-indent-mode>

;; Reindent immediately after change
(use-package aggressive-indent
  :ensure t
  :hook
  ((lisp-mode sly-mrepl-mode) . aggressive-indent-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/intro.html>

;; Support literate programming in Emacs with Common Lisp

;; Make the function aware of Sly (defaults to Slime)
(use-package ob-lisp
  :ensure nil
  :custom
  (org-babel-lisp-eval-fn #'sly-eval))

;; Evaluate Common Lisp code in Org blocks via "C-c C-c"
(use-package org
  :ensure nil
  :hook
  (org-mode . (lambda () (org-babel-do-load-languages
                          'org-babel-load-languages
                          (add-to-list 'org-babel-load-languages
                                       '(lisp . t))))))

;;  ____________________________________________________________________________
(provide 'ontop-commonlisp)
;;; ontop-commonlisp.el ends here
