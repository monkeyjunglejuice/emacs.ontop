;;; eon-evil.el --- Vim keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop
;;
;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; EVIL
;; <https://evil.readthedocs.io/en/latest/settings.html#>
;; <https://github.com/noctuid/evil-guide>

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :custom
  (eon-leader-key "C-,")
  (eon-localleader-key ",")
  :config
  (evil-mode 1)
  ;; Leader key and alternative
  (evil-set-leader '(normal visual motion) (kbd ","))
  (evil-set-leader nil (kbd eon-leader-key))
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader>") ctl-z-map)
  ;; Escape from Evil Emacs state
  (evil-define-key 'emacs 'global [escape] #'evil-normal-state))

(use-package evil-vars :ensure nil
  :after evil
  :custom
  (evil-move-cursor-back nil)
  (evil-undo-system 'undo-fu))

;; <https://github.com/emacs-evil/evil-collection>
(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :custom
  ;; (evil-collection-corfu-key-themes '(magic-backspace))
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'dired-mode-map
    "l" #'dired-find-file
    "h" #'dired-up-directory
    "." #'dired-hide-dotfiles-mode
    "y" #'dired-ranger-copy
    "p" #'dired-ranger-paste
    "P" #'dired-ranger-move))

;;  ____________________________________________________________________________
(provide 'eon-evil)
;;; eon-evil.el ends here
