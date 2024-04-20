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
;;; GERBIL
;; <https://cons.io/guide/emacs.html>

(defvar *gerbil-path*
  (shell-command-to-string "gxi -e '(display (path-expand \"~~\"))'\
      -e '(flush-output-port)'"))

(use-package gerbil-mode
  :when (file-directory-p *gerbil-path*)
  :ensure nil
  ;; :straight nil
  :defer t
  :mode (("\\.ss\\'"  . gerbil-mode)
         ("\\.pkg\\'" . gerbil-mode))
  :bind (:map comint-mode-map
		          (("C-S-n" . comint-next-input)
		           ("C-S-p" . comint-previous-input)
		           ("C-S-l" . clear-comint-buffer))
		          :map gerbil-mode-map
		          (("C-S-l" . clear-comint-buffer)))
  :init
  (autoload 'gerbil-mode
    (expand-file-name "share/emacs/site-lisp/gerbil-mode.el" *gerbil-path*)
    "Gerbil editing mode." t)
  :hook
  ((gerbil-mode-hook . linum-mode)
   (inferior-scheme-mode-hook . gambit-inferior-mode))
  :config
  (require 'gambit
           (expand-file-name "share/emacs/site-lisp/gambit.el" *gerbil-path*))
  (setf scheme-program-name (expand-file-name "bin/gxi" *gerbil-path*))

  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (let ((tags (expand-file-name "src/TAGS" *gerbil-path*)))
    (when (file-exists-p tags) (visit-tags-table tags)))

  (when (package-installed-p 'smartparens)
    (sp-pair "'" nil :actions :rem)
    (sp-pair "`" nil :actions :rem))

  (defun clear-comint-buffer ()
    (interactive)
    (with-current-buffer "*scheme*"
	    (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer)))))

(defun gerbil-setup-buffers ()
  "Change current buffer mode to gerbil-mode and start a REPL."
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

(use-package smartparens
  :ensure t
  :hook
  ((gerbil-mode) . smartparens-strict-mode))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Color-code nested parens
;; <https://github.com/Fanael/rainbow-delimiters>
(use-package rainbow-delimiters
  :ensure t
  :hook
  (gerbil-mode . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :ensure t
;;   :hook
;;   ((gerbil-mode inferior-scheme-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://www.orgmode.org/worg/org-contrib/babel/languages/ob-doc-scheme.html>

;; TODO: This seems not to work; neither with Chicken nor Racket

;; Support literate programming in Emacs with Scheme
;; Evaluate Scheme code in Org blocks via "C-c C-c"
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
