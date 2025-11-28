;;; eon-lang-scheme-guile.el --- Guile Scheme -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GEISER
;; <https://www.nongnu.org/geiser/>

(require 'eon-lang-scheme)

;; <https://gitlab.com/emacs-geiser/guile>
(use-package geiser-guile :ensure t
  :custom
  (geiser-guile-init-file "~/.guile")
  (geiser-guile-binary '("guile" "--no-auto-compile")))

;; _____________________________________________________________________________
(provide 'eon-lang-scheme-guile)
;;; eon-lang-scheme-guile.el ends here
