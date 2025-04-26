;;; eon-evil.el --- EVIL settings  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-evil.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; EVIL
;; <https://evil.readthedocs.io/en/latest/settings.html#>
;; <https://github.com/noctuid/evil-guide>

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  ;; Set leader key in all states
  (evil-set-leader nil (kbd "M-SPC"))
  ;; Set leader key in normal state
  (evil-set-leader 'normal (kbd "SPC"))
  ;; Set local leader
  (evil-set-leader 'normal ",")
  ;; Leader bindings
  (evil-define-key 'normal 'global (kbd "<leader>x") #'execute-extended-command)
  ;; Emacs ONBOARD/EON-specific keybindings
  (evil-define-key 'normal 'global (kbd "<leader>z") #'ctl-z-map)
  ;; Projects
  (evil-define-key 'normal 'global (kbd "<leader>p") #'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>k") #'project-kill-buffers)
  ;; Buffers
  (evil-define-key 'normal 'global (kbd "<leader>b") #'consult-project-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>B") #'consult-buffer)
  ;; Files
  (evil-define-key 'normal 'global (kbd "<leader>f") #'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>F") #'find-file)
  ;; Dired
  (evil-define-key 'normal 'global (kbd "<leader>d") #'project-dired)
  (evil-define-key 'normal 'global (kbd "<leader>D") #'dired)
  (evil-define-key 'normal 'dired-mode-map (kbd ".") #'dired-hide-dotfiles-mode)
  ;; Search
  (evil-define-key 'normal 'global (kbd "<leader>l") #'consult-line)
  (evil-define-key 'normal 'global (kbd "<leader>i") #'consult-imenu)
  (evil-define-key 'normal 'global (kbd "<leader>I") #'consult-imenu-multi)
  (evil-define-key 'normal 'global (kbd "<leader>r") #'rg-menu)
  (evil-define-key 'normal 'global (kbd "<leader>R") #'consult-ripgrep)
  ;; Git
  (evil-define-key 'normal 'global (kbd "<leader>g") #'magit-project-status)
  (evil-define-key 'normal 'global (kbd "<leader>cgk") #'magit-kill-buffers)
  ;; Shell
  (evil-define-key 'normal 'global (kbd "<leader>s") #'eat-project)
  (evil-define-key 'normal 'global (kbd "<leader>S") #'eat)
  (evil-define-key 'normal 'global (kbd "<leader>!") #'shell-command)
  ;; Eshell
  (evil-define-key 'normal 'global (kbd "<leader>e") #'project-eshell)
  (evil-define-key 'normal 'global (kbd "<leader>E") #'eshell)
  (evil-define-key 'normal 'global (kbd "<leader>:") #'eshell-command)
  ;; Org
  (evil-define-key 'normal 'global (kbd "<leader>op") #'org-publish)
  ;; Escape from Evil Emacs state
  (evil-define-key 'emacs 'global [escape] #'evil-normal-state)
  )

;; <https://github.com/emacs-evil/evil-collection>
(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :init
  (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t))

;;  ____________________________________________________________________________
(provide 'eon-evil)
;;; eon-evil.el ends here
