;;; ontop-projectile.el --- Projectile configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-projectile.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; PROJECTILE
;; <https://docs.projectile.mx/projectile/index.html>
;; Drop-in replacement for Emacs'; built-in project management

(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :custom
  ;; Shorter mode-line
  (projectile-mode-line-prefix " P")
  ;; Don't hide current project
  (projectile-current-project-on-switch 'move-to-end)
  ;; Hide buffers
  (projectile-globally-ignored-buffers eon-boring-buffers)
  :config
  ;; Enable sorting
  (unless (eon-winp)
    (setq projectile-indexing-method 'hybrid  ; no Windows support
          projectile-sort-order 'recently-active))
  :bind-keymap
  ("C-x p" . projectile-command-map))

;;  ____________________________________________________________________________
(provide 'ontop-projectile)
;;; ontop-projectile.el ends here
