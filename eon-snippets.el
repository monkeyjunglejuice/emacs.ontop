;;; eon-snippets.el --- Code snippets -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; YASNIPPET
;; <https://github.com/joaotavora/yasnippet>

(use-package yasnippet :ensure t
  :defer t
  :diminish yas-minor-mode
  :config
  (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand)
  :hook
  ((prog-mode text-mode) . yas-minor-mode))

;; <https://github.com/AndreaCrotti/yasnippet-snippets>
(use-package yasnippet-snippets :ensure t
  :defer t)

;; <https://github.com/elken/yasnippet-capf>
(use-package yasnippet-capf :ensure t
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;;  ____________________________________________________________________________
(provide 'eon-snippets)
;;; eon-snippets.el ends here
