;;; ontop-gerbil.el --- Gerbil Scheme  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-gerbil.el")'.

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
;;; GERBIL SCHEME
;; <https://cons.io/guide/emacs.html>

(defvar *gerbil-path*
  (shell-command-to-string "gxi -e '(display (path-expand \"~~\"))'\
      -e '(flush-output-port)'")
  "Let Gerbil figure out its installation path.")

(defvar *gerbil-emacs-path*
  (expand-file-name "share/emacs/site-lisp/" *gerbil-path*)
  "Emacs packages coming with your Gerbil installation.")

(use-package scheme
  :ensure nil
  :custom
  ;; Set Gerbil Scheme as the default Scheme implementation for Emacs?
  (scheme-program-name (expand-file-name "bin/gxi" *gerbil-path*)))

(use-package gerbil-mode
  :ensure nil
  :load-path *gerbil-emacs-path*
  :mode
  (("\\.ss\\'"  . gerbil-mode)
   ("\\.pkg\\'" . gerbil-mode))
  :config
  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (let ((tags (expand-file-name "src/TAGS" *gerbil-path*)))
    (when (file-exists-p tags) (visit-tags-table tags)))

  (defun scheme-clear-repl ()
    "Clear the REPL without leaving the code buffer."
    (interactive)
    (with-current-buffer "*scheme*"
	    (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer))))
  :bind
  (:map comint-mode-map
		    (("C-S-n" . comint-next-input)
		     ("C-S-p" . comint-previous-input)
		     ("C-S-l" . scheme-clear-repl))
		    :map gerbil-mode-map
		    (("C-S-l" . scheme-clear-repl))))

;;  ____________________________________________________________________________
;;; GAMBIT REPL SUPPORT

(use-package gambit
  :ensure nil
  :load-path *gerbil-emacs-path*
  :hook
  ;; "Use the Gambit REPL for Gerbil Scheme"
  (inferior-scheme-mode . gambit-inferior-mode))

;;  ____________________________________________________________________________
;;; CONVENIENCE

;; Quickly launch a Gerbil buffer setup from everywhere
(defun gerbil-setup-buffers ()
  "Change current buffer mode to `gerbil-mode' and start a REPL."
  (interactive)
  (gerbil-mode)
  (split-window-below)
  (let ((buf (buffer-name)))
    (other-window 1)
    (run-scheme "gxi")
    (switch-to-buffer-other-window "*scheme*" nil)
    (switch-to-buffer buf)))

(define-key 'ctl-z-x-map (kbd "g") #'gerbil-setup-buffers)

;;  ____________________________________________________________________________
;;; SRFI BROWSER
;; <https://github.com/srfi-explorations/emacs-srfi>

(use-package srfi
  :ensure t)

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
  (gerbil-mode . smartparens-strict-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `ontop-core.el'
(use-package rainbow-delimiters
  :ensure t
  :hook
  (inferior-scheme-mode . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :ensure t
;;   :hook
;;   ((gerbil-mode inferior-scheme-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate Scheme code in Org source code blocks via "C-c C-c"

;; TODO -- This seems not to work
;; <https://www.orgmode.org/worg/org-contrib/babel/languages/ob-doc-scheme.html>
(use-package org
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 (add-to-list 'org-babel-load-languages '(scheme . t))))))

;;  ____________________________________________________________________________
(provide 'ontop-gerbil)
;;; ontop-gerbil.el ends here
