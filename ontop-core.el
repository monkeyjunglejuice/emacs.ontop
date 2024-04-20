;;; ontop-core.el --- Shared settings and definitions  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; Emacs ONTOP is an extension on top of the Emacs ONBOARD starter-kit

;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;; <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package nil))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure nil)
  (setq use-package-expand-minimally nil))

;;  ____________________________________________________________________________
;;; GARBAGE COLLECTION

(use-package gcmh
  :ensure t
  :diminish
  :init
  (when (fboundp #'eon-cancel-gc-timer)
    (eon-cancel-gc-timer))
  (gcmh-mode))

;;  ____________________________________________________________________________
;;; EXEC PATH FROM SHELL
;; <https://github.com/purcell/exec-path-from-shell>

;; Make Emacs use the $PATH set up by the user's shell, especially helpful
;; when starting Emacs via Systemd or similar which doesn't adopt all
;; shell environment variables
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;;  ____________________________________________________________________________
;;; AUTO-UPDATE PACKAGES
;; <https://github.com/rranelli/auto-package-update.el>

(use-package auto-package-update
  :ensure t
  :init
  (auto-package-update-maybe)
  :custom
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))

;;  ____________________________________________________________________________
;;; DIMINISH
;; <https://github.com/myrjola/diminish.el>

;; Hide or alter the mode-line strings of certain minor modes
(use-package diminish
  :ensure t)

;;  ____________________________________________________________________________
;;; WINDOW MANAGEMENT
;; <https://github.com/dimitri/switch-window>

(use-package switch-window
  :ensure t
  :custom
  (switch-window-background t)
  (switch-window-minibuffer-shortcut 109)
  (switch-window-multiple-frames nil)
  (switch-window-threshold 1)
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
  (set-face-attribute 'switch-window-background nil
                      :foreground nil
                      :inherit 'shadow)
  :bind
  ;; Navigate windows by numbers
  ("C-x o"   . switch-window)
  ("C-x 1"   . switch-window-then-maximize)
  ("C-x 2"   . switch-window-then-split-below)
  ("C-x 3"   . switch-window-then-split-right)
  ("C-x 0"   . switch-window-then-delete)
  ;; ("C-x 4 0" . switch-window-then-kill-buffer)
  ("C-x 4 d" . switch-window-then-dired)
  ("C-x 4 f" . switch-window-then-find-file)
  ("C-x 4 b" . switch-window-then-display-buffer)
  ("C-x 4 s" . switch-window-then-swap-buffer))

;;  ____________________________________________________________________________
;;; COPY/PASTE
;; <https://elpa.gnu.org/packages/xclip.html>

;; Allow Emacs to copy/paste from/to the GUI clipboard when running
;; in a terminal emulator
(use-package xclip
  :ensure t)

;;  ____________________________________________________________________________
;;; DIRED
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

;; Show/hide dotfiles
;; <https://github.com/mattiasb/dired-hide-dotfiles>
(use-package dired-hide-dotfiles
  :ensure t
  :config
  (setq dired-hide-dotfiles-verbose nil)
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode)))

;; Filter Dired listings
(use-package dired-narrow
  :ensure t
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow-regexp)))

;; Ranger-like features
(use-package dired-ranger
  :ensure t
  :bind
  (:map dired-mode-map
        ("w" . dired-ranger-copy)  ; was dired-copy-filename-as-kill
        ("y" . dired-ranger-paste)  ; was dired-show-file-type
        ("Y" . dired-ranger-move)))

(use-package dired-subtree
  :ensure t
  :bind
  (:map dired-mode-map
        ("i" . dired-subtree-insert)
        ("I" . dired-subtree-remove)
        ("<tab>" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-cycle)))

;;  ____________________________________________________________________________
;;; EAT
;; <https://codeberg.org/akib/emacs-eat>
;; <https://elpa.nongnu.org/nongnu-devel/doc/eat.html>

(use-package eat
  :ensure t
  :init
  ;; Run Eshell always in Eat
  (eat-eshell-mode)
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t))

;;  ____________________________________________________________________________
;;; RIPGREP
;; <https://github.com/BurntSushi/ripgrep>
;; Ripgrep must be installed on your computer for this to work

;; <https://github.com/dajva/rg.el>
(use-package rg
  :ensure t
  :bind
  ("M-s r" . rg))

;; <https://github.com/mhayashi1120/Emacs-wgrep/>
(use-package wgrep
  :ensure t)

;;  ____________________________________________________________________________
;;; PROJECT
;; Setup for Emacs' built-in project management

(use-package project
  :ensure nil
  :config
  (setq project-switch-commands
        '((project-find-file "File" 102)
          (project-dired "Dired" 100)
          ;; Ripgrep is much faster; must be installed on your computer
          (rg-project "Ripgrep" 114)
          (magit-project-status "Magit" 109)
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
  ;; Some convenient keybindings.
  ("C-x f" . project-find-file)
  ("C-x d" . project-dired)
  ("M-SPC" . project-switch-to-buffer))

;;  ____________________________________________________________________________
;;; PROJECTILE
;; <https://docs.projectile.mx/projectile/index.html>
;; If you prefer to use Projectile instead,
;; copy/paste this config template to your `ontop-setup-personal.el':

;; (use-package projectile
;;   :ensure t
;;   :init
;;   (projectile-mode)
;;   :custom
;;   ;; Shorter mode-line
;;   (projectile-mode-line-prefix " P")
;;   ;; Don't hide current project
;;   (projectile-current-project-on-switch 'move-to-end)
;;   ;; Hide buffers
;;   (projectile-globally-ignored-buffers eon-boring-buffers)
;;   :config
;;   ;; Enable sorting
;;   (when (not (eon-winp))
;;     (setq projectile-indexing-method 'hybrid  ; no Windows support
;;           projectile-sort-order 'recently-active))
;;   :bind-keymap
;;   ("C-x p" . projectile-command-map))

;;  ____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md/>

(use-package eglot
  :ensure t
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
  :ensure t
  :init
  (define-prefix-command 'ctl-z-g-map)  ; Magit
  (define-key ctl-z-map (kbd "g") 'ctl-z-g-map)
  :custom
  ;; How many directoriess deep Magit looks for Git repos
  (magit-repository-directories '(("~/" . 1)))
  :config
  (defun magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  :bind
  (:map ctl-z-g-map
        ;; The Magit main keybinding
        ("g" . magit-status)
        ;; Kill useless Magit buffers that have been left open
        ("k" . magit-kill-buffers))
  (:map  magit-status-mode-map
         ("q" . magit-kill-buffers)))

;;  ____________________________________________________________________________
;;; GIT-GUTTER

;; <https://github.com/emacsorphanage/git-gutter>
(use-package git-gutter
  :ensure t
  :diminish
  :hook
  ((text-mode prog-mode) . git-gutter-mode))

;; <https://github.com/emacsorphanage/git-gutter-fringe>
(use-package git-gutter-fringe
  :ensure t
  :custom
  (git-gutter-fr:side 'left-fringe))

;;  ____________________________________________________________________________
;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

(use-package smartparens
  :ensure t
  :init
  ;; Turn off other modes that clash with Smartparens
  (electric-pair-mode -1)
  (show-paren-mode -1)
  ;; Globally enable non-strict delimiter handling?
  ;; Specific configurations can be found within the resp. language module files.
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  ;; :custom
  ;; Smartparens comes without keybindings defined, it's totally up to you if you
  ;; go with a pre-defined keybinding set or your set (see `:bind' down below).
  ;; Load one of the default keybinding sets:
  ;; (sp-base-key-bindings 'sp)
  ;; (sp-base-key-bindings 'paredit)
  :config
  (sp-with-modes sp-lisp-modes
    ;; disable ' (apostrophe) pairing, because it's the quote character
    (sp-local-pair "'" nil :actions nil)
    ;; also use the pseudo-quote inside strings where it serves as hyperlink
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))
  :hook
  ((emacs-lisp-mode lisp-interaction-mode) . smartparens-strict-mode)
  ((eshell-mode eval-expression-minibuffer-setup) . smartparens-mode)
  :bind
  ;; Custom keybinding set, resembling standard Emacs sexp keybindings
  (:map smartparens-mode-map
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-n" . sp-next-sexp)
        ("C-M-p" . sp-previous-sexp)
        ("C-M-k" . sp-kill-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-t" . sp-transpose-sexp)
        ("C-M-<space>" . sp-mark-sexp)
        ("C-M-<backspace>" . sp-backward-unwrap-sexp)
        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-<left>" . sp-forward-barf-sexp)
        ("C-M-<left>" . sp-backward-slurp-sexp)
        ("C-M-<right>" . sp-backward-barf-sexp)))

;;  ____________________________________________________________________________
;; PARENTHESIS DISPLAY

;; Color-code nested parens …
;; <https://github.com/Fanael/rainbow-delimiters>
(use-package rainbow-delimiters
  :ensure t
  :hook
  ((prog-mode comint-mode eval-expression-minibuffer-setup)
   . rainbow-delimiters-mode))

;; … and/or make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :ensure t
;;   :hook
;;   ((prog-mode comint-mode eval-expression-minibuffer-setup)
;;    . paren-face-mode))

;;  ____________________________________________________________________________
;; INDENTATION
;; <https://github.com/Malabarba/aggressive-indent-mode>

(use-package aggressive-indent
  :ensure t
  :hook
  (prog-mode . aggressive-indent-mode))

;;  ____________________________________________________________________________
;;; EXPAND REGION
;; <https://github.com/magnars/expand-region.el>

;; Expand region increases the selected region by semantic units.
;; Just keep pressing the key until it selects what you want.

(use-package expand-region
  :ensure t
  :bind
  ("M-=" . er/expand-region)
  ("M--" . er/contract-region))

;;  ____________________________________________________________________________
;;; GOTO LAST CHANGE
;; <https://github.com/emacs-evil/goto-chg>

(use-package goto-chg
  :ensure t
  :bind
  (:map prog-mode-map
        ("M-p" . goto-last-change)
        ("M-n" . goto-last-change-reverse))
  (:map text-mode-map
        ("M-p" . goto-last-change)
        ("M-n" . goto-last-change-reverse)))

;;  ____________________________________________________________________________
;;; EMBARK
;; <https://github.com/oantolin/embark>

(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :hook
  ;; Show the Embark target at point via Eldoc.
  (eldoc-documentation-functions . embark-eldoc-first-target)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  ;; ("<menu>" . embark-act)
  ;; ("M-<menu>" . embark-dwim)
  ("C-." . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings))

;;  ____________________________________________________________________________
;;; ORDERLESS
;; <https://github.com/oantolin/orderless>
;; <https://github.com/minad/corfu?tab=readme-ov-file#orderless-completion>

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic
                                                 partial-completion
                                                 emacs22)))))

;;  ____________________________________________________________________________
;;; COLOR NAMES
;; <https://elpa.gnu.org/packages/rainbow-mode.html>

;; Colorize color names in arbitrary buffers
(use-package rainbow-mode
  :ensure t
  :init
  (rainbow-mode))

;;  ____________________________________________________________________________
;;; ORG MODE
;; <https://orgmode.org/>

;; Loading the latest Org version rather than the one built into Emacs:

;; If Emacs is loaded using literate Org config and more recent Org
;; version is loaded inside the file loaded by ‘org-babel-load-file’.
;; ‘org-babel-load-file’ triggers the built-in Org version clashing
;; the newer Org version attempt to be loaded later.
;; It is recommended to move the Org loading code before the
;; ‘org-babel-load-file’ call.

(use-package org
  :ensure t)

;;  ____________________________________________________________________________
;;; COMMON MARKUP- / SERIALIZATION FORMATS

;; <https://github.com/emacsorphanage/adoc-mode>
(use-package adoc-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode)))

;; <https://elpa.gnu.org/packages/csv-mode.html>
(use-package csv-mode
  :ensure t)

;; <https://github.com/dhall-lang/dhall-lang>
(use-package dhall-mode
  :ensure t)

;; <https://jblevins.org/projects/markdown-mode/>
(use-package markdown-mode
  :ensure t
  ;; Turn on visual word wrapping
  ;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Visual-Line-Mode>
  :hook
  (markdown-mode . visual-line-mode))

;; <https://github.com/yoshiki/yaml-mode>
(use-package yaml-mode
  :ensure t)

;;  ____________________________________________________________________________
(provide 'ontop-core)
;;; ontop-core.el ends here
