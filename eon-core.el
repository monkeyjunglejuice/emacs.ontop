;;; eon-core.el --- Shared settings and definitions  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; Emacs ONTOP is an extension on top of the Emacs ONBOARD starter-kit

;;; Code:

;;  ____________________________________________________________________________
;;; GARBAGE COLLECTION
;; <https://gitlab.com/koral/gcmh>

(use-package gcmh :ensure t
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

;; Make Emacs use the $PATH and other environment variables set up by your
;; shell; especially helpful when on MacOS, or starting Emacs via Systemd or
;; similar, who don't adopt certain environment variables.

(use-package exec-path-from-shell :ensure t
  :when
  (or (daemonp)
      (and (eon-macp) (display-graphic-p)))
  :preface
  (defvar eon-exec-path-from-shell-blocklist
    '(;; Unix/shell state
      "^HOME$" "^\\(OLD\\)?PWD$" "^SHLVL$" "^PS1$" "^R?PROMPT$"
      "^TERM\\(CAP\\)?$" "^USER$" "^GIT_CONFIG" "^INSIDE_EMACS$" "^_$"
      "^COLUMNS$" "^LINES$"
      ;; Display/session
      "^\\(WAYLAND_\\)?DISPLAY$" "^DBUS_SESSION_BUS_ADDRESS$" "^XAUTHORITY$"
      ;; WSL
      "^WSL_INTEROP$"
      ;; XDG runtime/session
      "^XDG_CURRENT_DESKTOP$" "^XDG_RUNTIME_DIR$"
      "^XDG_\\(VTNR$\\|SEAT$\\|BACKEND$\\|SESSION_\\)"
      ;; Socket-like vars
      "SOCK$"
      ;; SSH/GPG can get stale
      "^SSH_\\(AUTH_SOCK\\|AGENT_PID\\)$" "^\\(SSH\\|GPG\\)_TTY$"
      "^GPG_AGENT_INFO$")
    "Regexps for env var names to omit when exporting from the shell.")

  (defun eon-exec-path-from-shell--blocklisted-p (var)
    (seq-some (lambda (re) (string-match-p re var))
              eon-exec-path-from-shell-blocklist))

  (defun eon-exec-path-from-shell--env-lines ()
    "Return \"NAME=VALUE\" lines from the user's shell.
Respects user options: shell name & arguments."
    (let* ((sh (or exec-path-from-shell-shell-name
                   shell-file-name
                   (getenv "SHELL")))
           (ok (and sh (executable-find sh)))
           (args (append exec-path-from-shell-arguments '("-c" "env"))))
      (cond
       (ok (or (ignore-errors (apply #'process-lines sh args))
               (and (executable-find "printenv")
                    (process-lines "printenv"))
               (split-string (shell-command-to-string "env") "\n" t)))
       ((executable-find "printenv") (process-lines "printenv"))
       (t (split-string (shell-command-to-string "env") "\n" t)))))

  (defun eon-exec-path-from-shell--env-names ()
    (seq-keep (lambda (s)
                (when (string-match-p "=" s)
                  (car (split-string s "="))))
              (eon-exec-path-from-shell--env-lines)))

  (defun eon-exec-path-from-shell-refresh ()
    "Refresh names from shell if needed, then import values.
Respects all exec-path-from-shell user options. Interactive calls echo
the list."
    (interactive)
    (require 'exec-path-from-shell)
    (let* ((default
            (eval (car (get 'exec-path-from-shell-variables 'standard-value))))
           (current exec-path-from-shell-variables)
           (names (if (equal current default)
                      (seq-remove
                       #'eon-exec-path-from-shell--blocklisted-p
                       (eon-exec-path-from-shell--env-names))
                    current)))
      (setopt exec-path-from-shell-variables names)
      (exec-path-from-shell-initialize)
      (when (called-interactively-p 'interactive)
        (message "exec-path-from-shell (%d): %s"
                 (length names) (mapconcat #'identity names " ")))
      names))

  :custom
  ;; Example: '("-l") or nil for non-interactive shells (faster).
  ;; Your env vars should be defined for your login shell init, e.g.
  ;; ~/.profile, ~/.zprofile, ~/.bash_profile etc. - not in ~/.bashrc, ~/.zshrc
  (exec-path-from-shell-arguments '("-l"))
  ;; If you set this variable, your env variables will not be auto-selected:
  ;; (exec-path-from-shell-variables '("PATH" "MANPATH"))
  ;; Use a specific shell if you like:
  ;; (exec-path-from-shell-shell-name "/bin/bash")

  :config
  (eon-exec-path-from-shell-refresh))

;;  ____________________________________________________________________________
;;; OS INTEGRATION

;; Use the MacOS trash instead of freedesktop.org ~/.local/share/Trash
;; <https://github.com/emacsorphanage/osx-trash>
(use-package osx-trash :ensure t
  :when (eon-macp)
  :config
  (osx-trash-setup))

;;  ____________________________________________________________________________
;;; DIMINISH
;; <https://github.com/myrjola/diminish.el>

;; Hide or alter the mode-line strings of certain minor modes
(use-package diminish :ensure t)

;;  ____________________________________________________________________________
;;; ORDERLESS
;; <https://github.com/oantolin/orderless>
;; <https://github.com/minad/corfu?tab=readme-ov-file#orderless-completion>

(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic
                                                 partial-completion
                                                 emacs22)))))

;;  ____________________________________________________________________________
;;; CORFU
;; <https://github.com/minad/corfu>

(use-package corfu :ensure t
  :after orderless
  :init
  (global-completion-preview-mode -1)  ; Disable Emacs ONboard standard
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

(use-package cape :ensure t
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
;;; COPY / PASTE

;; Allow Emacs to copy/paste from/to the GUI clipboard when running
;; in a terminal emulator
;; <https://elpa.gnu.org/packages/xclip.html>
(use-package xclip :ensure t)

;; No empty lines etc. in the kill ring
;; <https://github.com/NicholasBHubbard/clean-kill-ring.el>
(use-package clean-kill-ring :ensure t
  :config
  (clean-kill-ring-mode 1))

;;  ____________________________________________________________________________
;;; UNDO / REDO

(use-package undo-fu :ensure t)

(use-package undo-fu-session :ensure t
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
(use-package dired-hide-dotfiles :ensure t
  :config
  (setq dired-hide-dotfiles-verbose nil)
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode)))

;; Filter Dired listings
(use-package dired-narrow :ensure t
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow-regexp)))

;; Ranger-like features
(use-package dired-ranger :ensure t
  :bind
  (:map dired-mode-map
        ("w" . dired-ranger-copy)  ; was dired-copy-filename-as-kill
        ("y" . dired-ranger-paste)  ; was dired-show-file-type
        ("Y" . dired-ranger-move)))

(use-package dired-subtree :ensure t
  :init
  (require 'dired-subtree)
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
(use-package eat :ensure t
  :defer t
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  :config
  ;; Run Eshell always in Eat?
  (eat-eshell-mode))

;; <https://github.com/LemonBreezes/emacs-fish-completion>
(use-package fish-completion :ensure t
  :if (executable-find "fish")
  :config
  (global-fish-completion-mode))

;; <https://github.com/wwwjfy/emacs-fish>
(use-package fish-mode :ensure t
  :if (executable-find "fish"))

;;  ____________________________________________________________________________
;;; RIPGREP
;; <https://github.com/BurntSushi/ripgrep>
;; Ripgrep must be installed on your computer for this to work

;; <https://github.com/dajva/rg.el>
(use-package rg :ensure t
  :ensure-system-package
  (rg . ripgrep)
  :defer t
  :bind
  ("M-s r" . rg))

;; <https://github.com/mhayashi1120/Emacs-wgrep/>
(use-package wgrep :ensure t
  :defer t)

;;  ____________________________________________________________________________
;;; PROJECT
;; Setup for Emacs' built-in project management

(use-package project :ensure nil
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
  ;; Show all project keybindings in the selection?
  (setq project-switch-use-entire-map nil)
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
;;; MAGIT
;; <https://magit.vc/>

(use-package magit :ensure t
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
(use-package git-gutter :ensure t
  :defer t
  :diminish
  :hook
  ((text-mode prog-mode) . git-gutter-mode))

;; <https://github.com/emacsorphanage/git-gutter-fringe>
(use-package git-gutter-fringe :ensure t
  :after git-gutter
  :custom
  (git-gutter-fr:side 'left-fringe))

;;  ____________________________________________________________________________
;; PARENTHESIS DISPLAY

;; Color-code nested parens …
;; <https://github.com/Fanael/rainbow-delimiters>
(use-package rainbow-delimiters :ensure t
  :defer t
  :hook
  ((prog-mode conf-mode) . rainbow-delimiters-mode))

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
(use-package rainbow-mode :ensure t
  :defer t)

;;  ____________________________________________________________________________
;; INDENTATION
;; <https://github.com/Malabarba/aggressive-indent-mode>

(use-package aggressive-indent :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :hook
  ((prog-mode conf-mode) . aggressive-indent-mode))

;;  ____________________________________________________________________________
;;; GOTO LAST CHANGE
;; <https://github.com/emacs-evil/goto-chg>

(use-package goto-chg :ensure t
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
(use-package org-sticky-header :ensure t
  :after org
  :hook
  ;; Don't load for Org mode *scratch* buffer
  (org-mode . (lambda ()
                (when buffer-file-name
                  (org-sticky-header-mode 1)))))

;;  ____________________________________________________________________________
;;; MARKUP- / SERIALIZATION FORMATS

;; <https://github.com/emacsorphanage/adoc-mode>
(use-package adoc-mode :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode)))

;; <https://elpa.gnu.org/packages/csv-mode.html>
(use-package csv-mode :ensure t
  :defer t)

;; <https://github.com/dhall-lang/dhall-lang>
(use-package dhall-mode :ensure t
  :defer t)

;; <https://jblevins.org/projects/markdown-mode/>
(use-package markdown-mode :ensure t
  :defer t
  ;; Turn on visual word wrapping
  ;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Visual-Line-Mode>
  :hook
  (markdown-mode . visual-line-mode))

;; <https://github.com/yoshiki/yaml-mode>
(use-package yaml-mode :ensure t
  :defer t)

;;  ____________________________________________________________________________
;;; LISP

;; Listing of Lisp-related modes, can be used to enable/disable hooks all at
;; once

(defun my-lisp-src-modes ()
  "Generate a non-exhaustive list of loaded Lisp-related modes.
Entries are derived from the smartparens package."
  (seq-filter #'fboundp '(clojure-mode
                          clojurec-mode
                          clojurescript-mode
                          clojurex-mode
                          clojure-ts-mode
                          clojurescript-ts-mode
                          clojurec-ts-mode
                          common-lisp-mode
                          emacs-lisp-mode
                          fennel-mode
                          gerbil-mode
                          lfe-mode ; addition
                          lisp-mode
                          lisp-data-mode ; addition
                          racket-mode
                          scheme-mode
                          stumpwm-mode
                          )))

(defun my-lisp-repl-modes ()
  "Generate a non-exhaustive list of loaded Lisp-related REPLs.
Entries are derived from the smartparens package."
  (seq-filter #'fboundp '(cider-repl-mode
                          eshell-mode
                          fennel-repl-mode
                          geiser-repl-mode
                          inf-clojure-mode
                          inferior-emacs-lisp-mode
                          inferior-lfe-mode ; addition
                          inferior-lisp-mode
                          inferior-scheme-mode
                          lisp-interaction-mode
                          monroe-mode
                          racket-repl-mode
                          scheme-interaction-mode
                          slime-repl-mode
                          sly-mrepl-mode
                          )))

;;  ____________________________________________________________________________
(provide 'eon-core)
;;; eon-core.el ends here
