;;; ontop-evil.el --- EVIL settings  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-evil.el")'.

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
;;; EVIL
;; <https://evil.readthedocs.io/en/latest/settings.html#>
;; <https://github.com/noctuid/evil-guide>

(use-package evil
  :ensure t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
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
  ;; Projectssjs
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
  (evil-define-key 'normal 'global (kbd "<leader>op") #'org-publish))


;; <https://github.com/emacs-evil/evil-collection>
(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-cleverparens
  :ensure t
  :after evil)

;;  ____________________________________________________________________________
(provide 'ontop-evil)
;;; ontop-evil.el ends here
