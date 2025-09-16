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

(use-package evil :ensure t

  :init
  ;; Needed for evil-colletion
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  :config
  (evil-mode 1)

  ;; Define the leader keys
  (defvar eon-leader-key "SPC"
    "Leader key in Evil mode.")
  (defvar eon-leader-key-alt "M-SPC"
    "Leader key in all states that are not configured otherwise.")

  ;; Set the leader keys
  (evil-set-leader 'normal (kbd eon-leader-key))
  (evil-set-leader nil (kbd eon-leader-key-alt))

  ;; Bind Emacs ONboard "C-z" prefix categories under the leader
  (evil-define-key 'normal 'global (kbd "<leader>c") #'ctl-z-c-map)
  (evil-define-key 'normal 'global (kbd "<leader>e") #'ctl-z-e-map)
  (evil-define-key 'normal 'global (kbd "<leader>o") #'ctl-z-o-map)
  ;; TODO Add project.el bindings, etc
  ;; ...
  (evil-define-key 'normal 'global (kbd "<leader>s") #'ctl-z-s-map)
  (evil-define-key 'normal 'global (kbd "<leader>w") #'ctl-z-w-map)
  (evil-define-key 'normal 'global (kbd "<leader>x") #'ctl-z-x-map)

  ;; TODO Bind Emacs ONtop extended prefix categories under the leader
  ;; ... introduced by 3rd-party packagges

  ;; Leader toplevel bindings (shortcuts)
  ;; Project-related shorcuts are first-class and should have the
  ;; most accessible keybindings

  ;; M-x
  (evil-define-key 'normal 'global (kbd "<leader>m") #'execute-extended-command)

  ;; Buffer-related commands
  (evil-define-key 'normal 'global (kbd "<leader>B") #'consult-buffer)

  ;; File-related commands
  (evil-define-key 'normal 'global (kbd "<leader>f") #'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>F") #'find-file)

  ;; Dired-related commands
  (evil-define-key 'normal 'global (kbd "<leader>d") #'project-dired)
  (evil-define-key 'normal 'global (kbd "<leader>D") #'dired)

  ;; Search-related commands
  (evil-define-key 'normal 'global (kbd "<leader>/") #'consult-line)
  (evil-define-key 'normal 'global (kbd "<leader>C-/") #'consult-line-multi)
  (evil-define-key 'normal 'global (kbd "<leader>i") #'consult-imenu)
  (evil-define-key 'normal 'global (kbd "<leader>I") #'consult-imenu-multi)
  (evil-define-key 'normal 'global (kbd "<leader>r") #'rg-project)
  (evil-define-key 'normal 'global (kbd "<leader>R") #'rg)
  (evil-define-key 'normal 'global (kbd "<leader>C-r") #'consult-ripgrep)

  ;; Git-related commands
  (evil-define-key 'normal 'global (kbd "<leader>g") #'magit-project-status)

  ;; Shell-related commands
  (evil-define-key 'normal 'global (kbd "<leader>et") #'eat-project)
  (evil-define-key 'normal 'global (kbd "<leader>eT") #'eat)
  (evil-define-key 'normal 'global (kbd "<leader>!") #'shell-command)

  ;; TODO Eshell-related commands

  ;; Eval-related commands
  (evil-define-key 'normal 'global (kbd "<leader>:") #'eval-expression)

  ;; Help/documentation-related commands
  (evil-define-key 'normal 'global (kbd "<leader>hv") #'helpful-variable)
  (evil-define-key 'normal 'global (kbd "<leader>hf") #'helpful-callable)
  (evil-define-key 'normal 'global (kbd "<leader>hF") #'helpful-function)
  (evil-define-key 'normal 'global (kbd "<leader>ho") #'helpful-at-point)

  ;; Escape from Evil Emacs state
  (evil-define-key 'emacs 'global [escape] #'evil-normal-state))

(use-package evil-vars :ensure nil
  :after evil
  :custom
  (evil-move-cursor-back nil)
  (evil-undo-system 'undo-fu))

;; <https://github.com/emacs-evil/evil-collection>
(use-package evil-collection :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'dired-mode-map
    "l" #'dired-find-file
    "h" #'dired-up-directory))

;; <https://github.com/meain/evil-textobj-tree-sitter>
(use-package evil-textobj-tree-sitter :ensure t

  :config

  (evil-define-key 'nil evil-outer-text-objects-map
    "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
    "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (evil-define-key 'nil evil-inner-text-objects-map
    "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
    "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))

  ;; Goto start of next function
  (define-key evil-normal-state-map
              (kbd "]f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  ;; Goto start of previous function
  (define-key evil-normal-state-map
              (kbd "[f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  ;; Goto end of next function
  (define-key evil-normal-state-map
              (kbd "]F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  ;; Goto end of previous function
  (define-key evil-normal-state-map
              (kbd "[F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))  )

;;  ____________________________________________________________________________
;; STRUCTURAL EDITING
;; <https://github.com/noctuid/lispyville>

(use-package lispyville :ensure t
  :diminish
  :init
  (setq lispyville-key-theme
        '(operators
          c-w
          (prettify insert)
          (atom-movement t)
          slurp/barf-lispy
          additional
          additional-insert))
  :config
  (lispyville-set-key-theme)
  :hook
  ((prog-mode conf-mode) . lispyville-mode)
  (smartparens-mode . (lambda ()
                        (when (fboundp #'lispyville-mode)
                          (lispyville-mode -1))
                        (when (fboundp #'lispy)
                          (lispy -1)))))

;;  ____________________________________________________________________________
(provide 'eon-evil)
;;; eon-evil.el ends here
