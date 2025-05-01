;;; eon-core.el --- Shared settings and definitions  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; Emacs ONTOP is an extension on top of the Emacs ONBOARD starter-kit

;;; Code:

;;  ____________________________________________________________________________
;;; GARBAGE COLLECTION
;; <https://gitlab.com/koral/gcmh>

(use-package gcmh
  :diminish
  :init
  ;; Turn off the garbage collection tuning from Emacs ONTOP ...
  (when (fboundp #'eon-cancel-gc-timer)
    (eon-cancel-gc-timer))
  ;; ... and use the more sophisticated GCMH package instead
  (gcmh-mode))

;;  ____________________________________________________________________________
;;; EXEC PATH FROM SHELL
;; <https://github.com/purcell/exec-path-from-shell>

;; Make Emacs use the $PATH set up by the user's shell, especially helpful
;; when on MacOS or starting Emacs via Systemd or similar which
;; doesn't adopt all shell environment variables
(use-package exec-path-from-shell
  :if (or (eon-macp) (daemonp))
  :init
  (exec-path-from-shell-initialize))

;;  ____________________________________________________________________________
;;; AUTO-UPDATE PACKAGES
;; <https://github.com/rranelli/auto-package-update.el>

(use-package auto-package-update
  :init
  (auto-package-update-maybe)
  :custom
  (auto-package-package-update-show-preview t)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))

;;  ____________________________________________________________________________
;;; OS INTEGRATION

;; Use the MacOS trash instead of freedesktop.org ~/.local/share/Trash
;; <https://github.com/emacsorphanage/osx-trash>
(use-package osx-trash
  :when (eon-macp)
  :config
  (osx-trash-setup))

;;  ____________________________________________________________________________
;;; DIMINISH
;; <https://github.com/myrjola/diminish.el>

;; Hide or alter the mode-line strings of certain minor modes
(use-package diminish)

;;;  ____________________________________________________________________________
;;; MARGINALIA
;; <https://github.com/minad/marginalia>

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init
  ;; The :init configuration is always executed (Not lazy!)
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
  :bind
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

;;  ____________________________________________________________________________
;;; ORDERLESS
;; <https://github.com/oantolin/orderless>
;; <https://github.com/minad/corfu?tab=readme-ov-file#orderless-completion>

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic
					                                       partial-completion
					                                       emacs22)))))

;;  ____________________________________________________________________________
;;; CONSULT
;; <https://github.com/minad/consult>
;; <https://github.com/minad/consult/wiki>

(use-package consult
  :after orderless
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :custom
  ;; Don't clutter buffer list with rarely used buffers;
  ;; use `M-x ibuffer' or `C-x C-b' to access these "boring" buffers
  (consult-buffer-filter eon-boring-buffers)
  ;; :hook
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;; (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c M-x"           . consult-mode-command)
   ("C-c h"             . consult-history)
   ("C-c k"             . consult-kmacro)
   ("C-c m"             . consult-man)
   ("C-c i"             . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ; orig. repeat-complex-command
   ("C-x b"   . consult-buffer)              ; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#"   . consult-register-load)
   ("M-'"   . consult-register-store)        ; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; M-g bindings (goto-map)
   ("M-g e"   . consult-compile-error)
   ("M-g f"   . consult-flymake)             ; Alternative: consult-flycheck
   ("M-g g"   . consult-goto-line)           ; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ; orig. goto-line
   ("M-g o"   . consult-outline)             ; Alternative: consult-org-heading
   ("M-g m"   . consult-mark)
   ("M-g k"   . consult-global-mark)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   ;; Other custom bindings
   ("M-y"     . consult-yank-pop)              ; orig. yank-pop
   ("M-SPC"   . consult-project-buffer)        ; orig. project-switch-to-buffer
   ("M-s M-s" . consult-line)
   ("M-s s"   . consult-line-multi)
   ("M-s M-r" . consult-ripgrep)
   :map isearch-mode-map
   ("M-e"   . consult-isearch-history)       ; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ; orig. isearch-edit-string
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ; orig. next-matching-history-element
   ("M-r" . consult-history))                ; orig. previous-matching-history-element
  )

;; Use `consult-completion-in-region' if a vertical completion is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if (or vertico-mode fido-vertical-mode)
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

;;  ____________________________________________________________________________
;;; VERTICO
;; <https://github.com/minad/vertico>
;; <https://github.com/minad/vertico#key-bindings>

(use-package vertico
  :after orderless
  :init
  ;; Disable ONBOARD completion
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)
  ;; Enable Vertico
  (vertico-mode)
  ;; How to display Vertico per default?
  (vertico-multiform-mode)
  ;; (vertico-buffer-mode)
  :custom
  ;; Display certain listings in another form?
  (vertico-multiform-commands '((consult-imenu buffer)
                                (consult-outline buffer)
                                (consult-grep buffer)
                                (consult-ripgrep buffer)))
  ;; In the minibuffer
  (vertico-count 12)
  (vertico-resize 'grow-only)
  ;; Enable cycling for `vertico-next' and `vertico-previous'?
  (vertico-cycle nil))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; EMACS (built-in)

;; Persist history over Emacs restarts. Vertico sorts by history position
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; A few more useful configurations ...
(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '( read-only t
           cursor-intangible t
           face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

;;  ____________________________________________________________________________
;;; CORFU
;; <https://github.com/minad/corfu>

(use-package corfu
  :after orderless
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (global-corfu-minibuffer nil)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.1))
  (corfu-popupinfo-max-height 10)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-preselect 'valid)
  (corfu-on-exact-match 'nil)
  :hook
  ((eat-eshell-mode eshell-mode shell-mode) . corfu-mode)
  :bind
  (:map corfu-map
        ;; ("TAB" . corfu-next)
        ;; ([tab] . corfu-next)
        ;; ("S-TAB" . corfu-previous)
        ;; ([backtab] . corfu-previous)
        ;; ("S-RET" . corfu-insert)
        ;; ("S-<return>" . corfu-insert)
        ("RET" . nil)))

;;  ____________________________________________________________________________
;;; CAPE
;; <https://github.com/minad/cape>

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Continuously update the candidates - deactivate if lagging
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; ;; Compose own completion at point for Eglot
  ;; (defun eon-eglot-capf-super ()
  ;;   (setq-local completion-at-point-functions
  ;;               (list (cape-capf-super
  ;;                      #'eglot-completion-at-point
  ;;                      #'cape-file))))
  ;; :hook
  ;; (eglot-managed-mode . #'eon-eglot-capf-super)
  :bind
  ("C-c p f" . cape-file))

;;  ____________________________________________________________________________
;;; YASNIPPET
;; <https://github.com/joaotavora/yasnippet>

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :config
  (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand)
  :hook
  ((prog-mode text-mode) . yas-minor-mode))

;; <https://github.com/AndreaCrotti/yasnippet-snippets>
(use-package yasnippet-snippets
  :defer t)

;; <https://github.com/elken/yasnippet-capf>
;; (use-package yasnippet-capf
;;   :after cape
;;   :config
;;   (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; EMACS (built-in)

;; A few more useful configurations
(use-package emacs
  :ensure nil
  :init
  ;; Enable indentation + completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Use Dabbrev with Corfu
(use-package dabbrev
  :ensure nil
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;;  ____________________________________________________________________________
;;; WINDOW MANAGEMENT
;; <https://github.com/dimitri/switch-window>

(use-package switch-window
  :custom
  (switch-window-background t)
  (switch-window-multiple-frames nil)
  (switch-window-threshold 1)
  (switch-window-mvborder-increment 1)
  (switch-window-minibuffer-shortcut 109) ; "m"
  :config
  ;; Set Vim/Xmonad-like keybindings for window resizing
  (setq switch-window-extra-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "k") 'switch-window-mvborder-up)
          (define-key map (kbd "j") 'switch-window-mvborder-down)
          (define-key map (kbd "h") 'switch-window-mvborder-left)
          (define-key map (kbd "l") 'switch-window-mvborder-right)
          (define-key map (kbd "b") 'balance-windows)
          (define-key map (kbd "SPC") 'switch-window-resume-auto-resize-window)
          map))
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "g"
          "q" "w" "e" "r" "t" "y"
          "u" "i" "o" "p"
          "z" "x" "c" "v"
          "b" "n"))
  (set-face-attribute 'switch-window-background nil
                      :foreground 'unspecified
                      :inherit 'shadow)
  (set-face-attribute 'switch-window-label nil
                      :inherit 'show-paren-match-expression
                      :height 1.0)
  :bind
  ;; Bind `switch-window' commands to regular Emacs keybindings
  ("C-x o"   . switch-window)
  ("C-x 1"   . switch-window-then-maximize)
  ("C-x 2"   . switch-window-then-split-below)
  ("C-x 3"   . switch-window-then-split-right)
  ("C-x 0"   . switch-window-then-delete)
  ("C-x 4 0" . switch-window-then-kill-buffer)
  ("C-x 4 d" . switch-window-then-dired)
  ("C-x 4 f" . switch-window-then-find-file)
  ("C-x 4 b" . switch-window-then-display-buffer)
  ("C-x 4 s" . switch-window-then-swap-buffer))

;;  ____________________________________________________________________________
;;; COPY / PASTE

;; Allow Emacs to copy/paste from/to the GUI clipboard when running
;; in a terminal emulator
;; <https://elpa.gnu.org/packages/xclip.html>
(use-package xclip)

;; No empty lines etc. in the kill ring
;; <https://github.com/NicholasBHubbard/clean-kill-ring.el>
(use-package clean-kill-ring
  :config
  (clean-kill-ring-mode 1))

;;  ____________________________________________________________________________
;;; UNDO / REDO

(use-package undo-fu
  :custom
  (undo-limit 67108864) ; 64mb
  (undo-strong-limit 100663296) ; 96mb
  (setq undo-outer-limit 1006632960) ; 960mb
  )

(use-package undo-fu-session
  :custom
  (undo-fu-session-incompatible-files
   '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode))

;;  ____________________________________________________________________________
;;; DIRED
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

;; Show/hide dotfiles
;; <https://github.com/mattiasb/dired-hide-dotfiles>
(use-package dired-hide-dotfiles
  :config
  (setq dired-hide-dotfiles-verbose nil)
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode)))

;; Filter Dired listings
(use-package dired-narrow
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow-regexp)))

;; Ranger-like features
(use-package dired-ranger
  :bind
  (:map dired-mode-map
        ("w" . dired-ranger-copy)  ; was dired-copy-filename-as-kill
        ("y" . dired-ranger-paste)  ; was dired-show-file-type
        ("Y" . dired-ranger-move)))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("i" . dired-subtree-insert)
        ("I" . dired-subtree-remove)
        ("<tab>" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-cycle)))

;;  ____________________________________________________________________________
;;; SHELLS

;; <https://codeberg.org/akib/emacs-eat>
;; <https://elpa.nongnu.org/nongnu-devel/doc/eat.html>
(use-package eat
  :defer t
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  :config
  ;; Run Eshell always in Eat?
  (eat-eshell-mode))

;; <https://github.com/LemonBreezes/emacs-fish-completion>
(use-package fish-completion
  :if (executable-find "fish")
  :config
  (global-fish-completion-mode))

;; <https://github.com/wwwjfy/emacs-fish>
(use-package fish-mode
  :if (executable-find "fish"))

;;  ____________________________________________________________________________
;;; RIPGREP
;; <https://github.com/BurntSushi/ripgrep>
;; Ripgrep must be installed on your computer for this to work

;; <https://github.com/dajva/rg.el>
(use-package rg
  :ensure-system-package
  (rg . ripgrep)
  :defer t
  :bind
  ("M-s r" . rg))

;; <https://github.com/mhayashi1120/Emacs-wgrep/>
(use-package wgrep
  :defer t)

;;  ____________________________________________________________________________
;;; PROJECT
;; Setup for Emacs' built-in project management

(use-package project
  :ensure nil
  :defer t
  :config
  (setq project-switch-commands
        '((project-find-file "File" 102)
          (project-dired "Dired" 100)
          ;; Ripgrep is much faster; must be installed on your computer
          (rg-project "Ripgrep" 114)
          (magit-project-status "Magit" 103)
          (project-eshell "Eshell" 101)
          (project-shell "Shell" 115)))
  (setq project-switch-use-entire-map t)
  ;; `project-find-dir' produces huge lists of all subdirectories, which can
  ;; cause a huge lag and freeze Emacs for a while if the directory is large.
  ;; Use `project-dired' instead; switch keybindings accordingly.
  (defun eon--swap-project-key-bindings ()
    "Swap key bindings for `project-dired` and `project-find-dir`."
    ;; Unbind and rebind keys in project-prefix-map
    (define-key project-prefix-map (kbd "D") #'project-find-dir)
    (define-key project-prefix-map (kbd "d") #'project-dired)
    ;; Handling global map bindings requires different strategy.
    ;; Since these bindings are set up with a prefix, we'll unbind
    ;; and rebind them manually
    (global-unset-key (kbd "C-x p D"))
    (global-set-key (kbd "C-x p D") #'project-find-dir)
    (global-unset-key (kbd "C-x p d"))
    (global-set-key (kbd "C-x p d") #'project-dired)
    ;; Swap keys for term-raw-map if you're using term or ansi-term
    (with-eval-after-load 'term
      (define-key term-raw-map (kbd "C-c p D") #'project-find-dir)
      (define-key term-raw-map (kbd "C-c p d") #'project-dired)))
  ;; Call the function to perform the swap
  (eon--swap-project-key-bindings)
  :bind
  ;; Some convenient keybindings
  ("C-x f" . project-find-file)
  ("C-x d" . project-dired)
  ("M-SPC" . project-switch-to-buffer))

;;  ____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md/>

(use-package eglot
  :ensure nil
  :defer t
  :custom
  ;; Shutdown language server after closing last file
  (eglot-autoshutdown t)
  ;; Allow edits without confirmation?
  (eglot-confirm-server-initiated-edits nil)
  :bind
  (:map eglot-mode-map
        ("C-c c r" . eglot-rename)
        ("C-c c f" . eglot-format)
        ("C-c c F" . eglot-format-buffer)
        ("C-c c a" . eglot-code-actions)
        ("C-c c h" . eldoc)))

;; Eglot comes with a fairly complete set of associations of major-modes
;; to popular language servers predefined. If you need to add server
;; associations to the default list, use add-to-list. For example, you can
;; add it to the alist like this:
;;
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(lua-mode . ("lua-language-server" "--stdio"))))
;;
;; This will invoke the program tools with the command-line argument --stdio
;; in support of editing source files for which Emacs turns on foo-mode, and
;; will communicate with the program via the standard streams. As usual with
;; invoking programs, the executable file fools should be in one of the
;; directories mentioned by the exec-path variable (see Subprocess Creation
;; in GNU Emacs Lisp Reference Manual), for Eglot to be able to find it.
;; Sometimes, multiple servers are acceptable alternatives for handling a
;; given major-mode. In those cases, you may combine the helper function
;; eglot-alternatives with the functional form of eglot-server-programs.
;;
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `(lua-mode . ,(eglot-alternatives
;;                               '(("lua-language-server" "--stdio")
;;                                 ("lua-lsp" "--stdio"))))))

;;  ____________________________________________________________________________
;;; MAGIT
;; <https://magit.vc/>

(use-package magit
  :defer t
  :custom
  ;; How many directoriess deep Magit looks for Git repos
  (magit-repository-directories '(("~/" . 1)))
  :config
  (defun magit-kill-magit-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers))))

;;  ____________________________________________________________________________
;;; GIT-GUTTER

;; <https://github.com/emacsorphanage/git-gutter>
(use-package git-gutter
  :defer t
  :diminish
  :hook
  ((text-mode prog-mode) . git-gutter-mode))

;; <https://github.com/emacsorphanage/git-gutter-fringe>
(use-package git-gutter-fringe
  :after git-gutter
  :custom
  (git-gutter-fr:side 'left-fringe))

;;  ____________________________________________________________________________
;; PARENTHESIS DISPLAY

;; Color-code nested parens …
;; <https://github.com/Fanael/rainbow-delimiters>
(use-package rainbow-delimiters
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; … and/or make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :defer t
;;   :hook
;;   (prog-mode . paren-face-mode))

;;  ____________________________________________________________________________
;;; COLOR NAMES
;; <https://elpa.gnu.org/packages/rainbow-mode.html>

;; Colorize color names in arbitrary buffers
(use-package rainbow-mode
  :defer t)

;;  ____________________________________________________________________________
;; INDENTATION
;; <https://github.com/Malabarba/aggressive-indent-mode>

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :hook
  (prog-mode . aggressive-indent-mode))

;;  ____________________________________________________________________________
;;; GOTO LAST CHANGE
;; <https://github.com/emacs-evil/goto-chg>

(use-package goto-chg
  :defer t
  :bind
  (:map prog-mode-map
        ("M-p" . goto-last-change)
        ("M-n" . goto-last-change-reverse))
  (:map text-mode-map
        ("M-p" . goto-last-change)
        ("M-n" . goto-last-change-reverse)))

;;  ____________________________________________________________________________
;;; ORG MODE EXTENSIONS

;; <https://github.com/alphapapa/org-sticky-header>
(use-package org-sticky-header
  :after org
  :hook
  (org-mode . org-sticky-header-mode))

;;  ____________________________________________________________________________
;;; MARKUP- / SERIALIZATION FORMATS

;; <https://github.com/emacsorphanage/adoc-mode>
(use-package adoc-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode)))

;; <https://elpa.gnu.org/packages/csv-mode.html>
(use-package csv-mode
  :defer t)

;; <https://github.com/dhall-lang/dhall-lang>
(use-package dhall-mode
  :defer t)

;; <https://jblevins.org/projects/markdown-mode/>
(use-package markdown-mode
  :defer t
  ;; Turn on visual word wrapping
  ;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Visual-Line-Mode>
  :hook
  (markdown-mode . visual-line-mode))

;; <https://github.com/yoshiki/yaml-mode>
(use-package yaml-mode
  :defer t)

;;  ____________________________________________________________________________
(provide 'eon-core)
;;; eon-core.el ends here
