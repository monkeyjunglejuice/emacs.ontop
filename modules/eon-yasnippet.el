;;; eon-yasnippet.el --- Code snippets -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; YASNIPPET
;; <https://github.com/joaotavora/yasnippet>

(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;; SNIPPETS COLLECTION
;; <https://github.com/AndreaCrotti/yasnippet-snippets>
;; <https://andreacrotti.pro/yasnippet-snippets/snippets>

(use-package yasnippet-snippets :ensure t
  :after yasnippet)

;; _____________________________________________________________________________
(provide 'eon-yasnippet)
;;; eon-yasnippet.el ends here
