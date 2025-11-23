;;; eon-icons.el --- Icons everywhere -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; ICONS

(use-package nerd-icons :ensure t)

;; Dired
(use-package nerd-icons-dired :ensure t
  :after dired
  :diminish
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Ibuffer
(use-package nerd-icons-ibuffer :ensure t
  :after ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Completion
(use-package nerd-icons-completion :ensure t
  :config
  (nerd-icons-completion-mode))

;; Marginalia
(when (eon-modulep 'eon-marginalia)
  (use-package nerd-icons-completion :ensure t
    :hook
    (marginalia-mode #'nerd-icons-completion-marginalia-setup)))

;; Corfu
(when (eon-modulep 'eon-corfu)
  (use-package nerd-icons-corfu :ensure t
    :config
    (eon-add-to-list* 'corfu-margin-formatters
                            #'nerd-icons-corfu-formatter)))

;; _____________________________________________________________________________
(provide 'eon-icons)
;;; eon-icons.el ends here
