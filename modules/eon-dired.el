;;; eon-dired.el --- Classic Dired with improvements -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; DIRED
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

;; Show/hide dotfiles
;; <https://github.com/mattiasb/dired-hide-dotfiles>
(use-package dired-hide-dotfiles :ensure t
  :after dired
  :diminish
  :custom
  (dired-hide-dotfiles-verbose nil)
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map eon-localleader-dired-map
        ("h" . dired-hide-dotfiles-mode)))

;; Filter Dired listings
(use-package dired-narrow :ensure t
  :after dired
  :init
  (require 'dired-narrow)
  :bind
  (:map eon-localleader-dired-map
        ("/" . dired-narrow-regexp)))

;; Ranger-like features
(use-package dired-ranger :ensure t
  :after dired
  :init
  (require 'dired-ranger)
  :bind
  (:map dired-mode-map
        ("w" . dired-ranger-copy)  ; was dired-copy-filename-as-kill
        ("y" . dired-ranger-paste)  ; was dired-show-file-type
        ("Y" . dired-ranger-move)))

(use-package dired-subtree :ensure t
  :after dired
  :init
  (require 'dired-subtree)
  :bind
  (:map dired-mode-map
        ("i" . dired-subtree-insert)
        ("I" . dired-subtree-remove)
        ("<tab>" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-cycle)))

;; <https://github.com/karthink/dired-hist>
(use-package dired-hist :ensure t
  :after dired
  :config
  (dired-hist-mode)
  :bind
  (:map dired-mode-map
        ("l" . dired-hist-go-back)
        ("r" . dired-hist-go-forward)))

;; _____________________________________________________________________________
(provide 'eon-dired)
;;; eon-dired.el ends here
