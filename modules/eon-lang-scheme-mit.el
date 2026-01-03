;;; eon-lang-scheme-mit.el --- MIT Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/mit>
(use-package geiser-mit :ensure t
  :defer t
  :init
  (add-to-list 'geiser-active-implementations 'mit))

;; Built-in Emacs Scheme package
(use-package scheme :ensure nil
  :custom
  ;; MIT Scheme specific indentation
  (scheme-mit-dialect t))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-mit)
;;; eon-lang-scheme-mit.el ends here
