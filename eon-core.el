;;; eon-core.el --- Common configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; Emacs ONTOP is an extension on top of the Emacs ONBOARD starter-kit
;;
;;; Code:

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
    "Regexps for env var names to omit when exporting from the shell.
Adapted from Doom Emacs")
  
  (defun eon-exec-path-from-shell--blocklisted-p (var)
    (seq-some (lambda (re) (string-match-p re var))
              eon-exec-path-from-shell-blocklist))
  
  (defun eon-exec-path-from-shell--env-lines ()
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
  (exec-path-from-shell-arguments '("-l"))
  ;; You can set the variables manually, no autoselection:
  ;; (exec-path-from-shell-variables '("PATH" "MANPATH"))
  ;; Use a specific shell if you like:
  ;; (exec-path-from-shell-shell-name "/bin/bash")

  :config
  (eon-exec-path-from-shell-refresh))

;;  ____________________________________________________________________________
;;; OS INTEGRATION

;; Enable the built-in `use-package' extension ":ensure-system-package"
(use-package use-package-ensure-system-package)

;; <https://gitlab.com/jabranham/system-packages>
;; The package attempts to guess which system package manager you use,
;; and lets you manage your system packages directly from Emacs via
;; "M-x system-packages"
(use-package system-packages :ensure t)

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

;; (use-package corfu :ensure t
;;   :after orderless
;;   :init
;;   (global-completion-preview-mode -1)  ; Disable Emacs ONboard standard
;;   (global-corfu-mode)
;;   (corfu-popupinfo-mode)
;;   :custom
;;   (global-corfu-minibuffer t)
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.2)
;;   (corfu-popupinfo-delay '(0.3 . 0.1))
;;   (corfu-popupinfo-max-height 10)
;;   (corfu-quit-at-boundary 'separator)
;;   (corfu-quit-no-match 'separator)
;;   (corfu-preview-current t)
;;   (corfu-preselect 'valid)
;;   (corfu-on-exact-match 'nil)
;;   :hook
;;   ((eat-eshell-mode eshell-mode shell-mode)
;;    . corfu-mode)
;;   :bind
;;   (:map corfu-map
;;         ;; ("TAB" . corfu-next)
;;         ;; ([tab] . corfu-next)
;;         ;; ("S-TAB" . corfu-previous)
;;         ;; ([backtab] . corfu-previous)
;;         ;; ("S-RET" . corfu-insert)
;;         ;; ("S-<return>" . corfu-insert)
;;         ("RET" . nil)))

(use-package corfu
  :ensure t
  :after orderless
  :init
  (global-completion-preview-mode -1)
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  ;; Shells: cycle with TAB/S-TAB.
  (defun eon-corfu-shell-setup ()
    (setq-local corfu-auto t))
  ;; REPLs (comint & friends): keep/force auto popup.
  (defun eon-corfu-repl-setup ()
    (setq-local corfu-auto t))
  (defun eon-corfu-insert ()
    "Insert first candidate, even if nothing selected (works with 'prompt)."
    (interactive)
    (completion-at-point)
    (corfu-first)
    (corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.1))
  (corfu-popupinfo-max-height 10)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :config
  ;; "Magic backspace": cancel selection, keep popup open
  ;; (ported from evil-collection)
  (let ((magic-backspace
         (list 'menu-item "" nil
               :filter (lambda (&optional _)
                         (when (and (boundp 'corfu--index)
                                    (>= corfu--index 0))
                           'corfu-reset)))))
    (define-key corfu-map (kbd "DEL")         magic-backspace)
    (define-key corfu-map [backspace]         magic-backspace)
    (define-key corfu-map (kbd "<backspace>") magic-backspace))
  :hook
  ;; Shells: manual completion
  ((eshell-mode eat-eshell-mode shell-mode) . eon-corfu-shell-setup)
  ;; REPLs: most are comint-derived; ensure auto on there
  (comint-mode . eon-corfu-repl-setup)
  :bind
  (:map corfu-map
        ;; Cycle
        ("TAB" . corfu-next) ([tab] . corfu-next)
        ("S-TAB" . corfu-previous) ([backtab] . corfu-previous)
        ;; Commit explicitly
        ("C-j" . eon-corfu-insert)
        ;; Never steal newline/eval
        ("RET" . nil)))

;;  ____________________________________________________________________________
;;; CAPE: compose/augment CAPFs (files in shells & repls)
;; <https://github.com/minad/cape>

;; (use-package cape :ensure t
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   ;; Continuously update the candidates - deactivate if lagging
;;   ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
;;   ;; ;; Compose own completion at point for Eglot
;;   ;; (defun eon-eglot-capf-super ()
;;   ;;   (setq-local completion-at-point-functions
;;   ;;               (list (cape-capf-super
;;   ;;                      #'eglot-completion-at-point
;;   ;;                      #'cape-file))))
;;   ;; :hook
;;   ;; (eglot-managed-mode . #'eon-eglot-capf-super)
;;   :bind
;;   ("C-c p f" . cape-file))

(use-package cape
  :ensure t
  :init
  (defun eon-capf/eshell ()
    "Eshell: pcomplete + files, combined."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'pcomplete-completions-at-point
                       #'cape-file))))

  (defun eon-capf/shell ()
    "M-x shell: comint completion + files, combined."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'comint-completion-at-point
                       #'cape-file))))

  (defun eon-capf/repl ()
    "Other comint REPLs: keep capfs, but ensure files are available."
    (unless (derived-mode-p 'shell-mode)
      (setq-local completion-at-point-functions
                  (if (memq #'cape-file completion-at-point-functions)
                      completion-at-point-functions
                    (cons #'cape-file
                          completion-at-point-functions)))))

  ;; :hook
  ;; (eshell-mode . eon-capf/eshell)
  ;; (shell-mode  . eon-capf/shell)
  ;; ;; Cover comint REPLs (e.g. inferior-*, sql, etc.), but not shell-mode
  ;; (comint-mode . eon-capf/repl)
  )

;;  ____________________________________________________________________________
;;; COPY / PASTE

;; Copy/paste between TUI Emacs and graphical apps.
;; Installs and loads only if Emacs really runs in the terminal emulator.
;; <https://elpa.gnu.org/packages/xclip.html>
(use-package emacs
  :functions (xclip-mode)
  :preface
  (defun eon--xclip-ensure-on-tty (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      (unless (display-graphic-p)
        (use-package xclip :ensure t
          :demand t
          :config (xclip-mode 1)))))
  :hook
  (window-setup . eon--xclip-ensure-on-tty)
  (after-make-frame-functions . eon--xclip-ensure-on-tty))

;; No empty lines etc. in the kill ring
;; <https://github.com/NicholasBHubbard/clean-kill-ring.el>
(use-package clean-kill-ring :ensure t
  :config
  (clean-kill-ring-mode 1))

;;  ____________________________________________________________________________
;;; UNDO / REDO

(use-package undo-fu :ensure t)

;; Keep your undo/redo history even if you close the file and shutdown Emacs
(use-package undo-fu-session :ensure t
  :custom
  (undo-fu-session-incompatible-files
   '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode 1))

;;  ____________________________________________________________________________
;;; DIRED
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

;; Show/hide dotfiles
;; <https://github.com/mattiasb/dired-hide-dotfiles>
(use-package dired-hide-dotfiles :ensure t
  :diminish
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
;;; SHELL / TERMINAL

;; <https://codeberg.org/akib/emacs-eat>
;; <https://elpa.nongnu.org/nongnu-devel/doc/eat.html>
(use-package eat :ensure t
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  :config
  ;; Run Eshell always in Eat?
  (eat-eshell-mode)
  :bind
  (:map ctl-z-e-map
        ;; Terminal emulator
        ("t" . eat)))

(when (executable-find "fish")
  ;; <https://github.com/LemonBreezes/emacs-fish-completion>
  (use-package fish-completion :ensure t
    :config
    (global-fish-completion-mode))
  ;; <https://github.com/wwwjfy/emacs-fish>
  (use-package fish-mode :ensure t))

;;  ____________________________________________________________________________
;;; GREP / RIPGREP
;; <https://github.com/BurntSushi/ripgrep>
;; Ripgrep must be installed on your computer for this to work

;; <https://github.com/dajva/rg.el>
(when (executable-find "rg")
  (use-package rg :ensure t
    :defer t
    :bind
    (:map ctl-z-s-map
          ("r" . rg))))

;; <https://github.com/mhayashi1120/Emacs-wgrep/>
(use-package wgrep :ensure t
  :defer t)

;;  ____________________________________________________________________________
;;; MAGIT
;; <https://magit.vc/>

(use-package magit :ensure t
  :defer t
  :custom
  ;; How many directoriess deep Magit looks for Git repos
  (magit-repository-directories '(("~/" . 1)))
  :config
  (defun eon-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  :bind
  (:map ctl-z-v-map
        ("v" . magit-project-status)
        ("f" . magit-file-dispatch)
        ("g" . magit-dispatch)
        ("k" . eon-magit-kill-buffers)))

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
  :diminish
  :defer t
  :bind
  (:map ctl-z-x-map
        ("c" . rainbow-mode)))

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
  :init
  (defun eon-org--sticky-header-maybe ()
    "Enable org-sticky-header unless in *scratch* buffer."
    (unless (string= (buffer-name) "*scratch*")
      (org-sticky-header-mode 1)))
  :hook (org-mode . eon-org--sticky-header-maybe))

;;  ____________________________________________________________________________
;;; MARKUP- / SERIALIZATION FORMATS

;; <https://github.com/emacsorphanage/adoc-mode>
(use-package adoc-mode
  :ensure t
  :mode (("\\.asciidoc\\'" . adoc-mode)
         ("\\.adoc\\'" . adoc-mode)))

;; <https://elpa.gnu.org/packages/csv-mode.html>
(use-package csv-mode :ensure t)

;; <https://github.com/dhall-lang/dhall-lang>
(use-package dhall-mode :ensure t)

;; <https://jblevins.org/projects/markdown-mode/>
(use-package markdown-mode :ensure t
  :hook
  ;; Turn on visual word wrapping
  (markdown-mode . visual-line-mode))

;; <https://github.com/yoshiki/yaml-mode>
(use-package yaml-mode :ensure t)

;;  ____________________________________________________________________________
;;; LISP

;; Sophisticated macro expander for Emacs Lisp
(use-package macrostep :ensure t
  :bind
  (:map eon-localleader-elisp-macro-map
        ("m" . #'macrostep-expand)
        ("c" . #'macrostep-collapse)
        ("C" . #'macrostep-collapse-all)
        ("[" . #'macrostep-prev-macro)
        ("]" . #'macrostep-next-macro)))

;; Listing of Lisp-related modes, can be used to enable/disable hooks
;; all at once
(eon-add-to-list 'eon-lisp-src-modes-registry
                 '(clojure-mode
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
                   lfe-mode
                   lisp-mode
                   lisp-data-mode
                   racket-mode
                   scheme-mode
                   stumpwm-mode))

(eon-add-to-list 'eon-lisp-repl-modes-registry
                 '(cider-repl-mode
                   eshell-mode
                   fennel-repl-mode
                   geiser-repl-mode
                   inf-clojure-mode
                   inferior-emacs-lisp-mode
                   inferior-lfe-mode
                   inferior-lisp-mode
                   inferior-scheme-mode
                   lisp-interaction-mode
                   monroe-mode
                   racket-repl-mode
                   scheme-interaction-mode
                   slime-repl-mode
                   sly-mrepl-mode))

;;  ____________________________________________________________________________
(provide 'eon-core)
;;; eon-core.el ends here
