;;; eon-core.el --- Shared packages and definitions -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
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
  ;; TODO Refine the loading conditions, so it doesn't load when
  ;; graphical Emacs has been started *from* the terminal
  (or (daemonp) (and (eon-macp) (display-graphic-p)))

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
Adapted from Doom Emacs.")
  
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
                       ;; TODO Currently inefficient, causes 1st shell spawn
                       (eon-exec-path-from-shell--env-names))
                    current)))
      (setopt exec-path-from-shell-variables names)
      ;; TODO Causes 2nd shell spawn to do the work; should do all at once
      (exec-path-from-shell-initialize)
      (when (called-interactively-p 'interactive)
        ;; TODO Also show the issued shell command in the message
        (message "exec-path-from-shell (%d): %s"
                 (length names) (mapconcat #'identity names " ")))
      names))

  :custom

  ;; Example: '("-l") or nil for non-interactive shells; supposedly faster
  (exec-path-from-shell-arguments '("-l"))

  ;; You can set the variables manually, then no autoselection will happen:
  ;; (exec-path-from-shell-variables '("PATH" "MANPATH"))

  ;; Use a specific shell if you like:
  ;; (exec-path-from-shell-shell-name "/bin/bash")

  :config

  (eon-exec-path-from-shell-refresh))

;;  ____________________________________________________________________________
;;; OS INTEGRATION

;; Use the MacOS trash instead of freedesktop.org ~/.local/share/Trash
;; <https://github.com/emacsorphanage/osx-trash>
(when (eon-macp)
  (use-package osx-trash :ensure t
    :config
    (osx-trash-setup)))

;; <https://gitlab.com/jabranham/system-packages>
;; The package attempts to guess which system package manager you use,
;; and lets you manage your system packages directly from Emacs via
;; "M-x system-packages"
;; (use-package system-packages :ensure t)

;;  ____________________________________________________________________________
;;; USER INTERFACE

;; Hide or alter the mode-line strings of certain minor modes
;; <https://github.com/myrjola/diminish.el>
(use-package diminish :ensure t)

;; Hydra
;; <https://github.com/abo-abo/hydra>
(use-package hydra :ensure t)

;;  ____________________________________________________________________________
;;; ORDERLESS
;; <https://github.com/oantolin/orderless>

(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion))))))

;;  ____________________________________________________________________________
;;; COPY / PASTE

;; Copy/paste between TUI Emacs and graphical applications.
;; Installs and loads only if Emacs really runs in the terminal emulator.
;; <https://elpa.gnu.org/packages/xclip.html>
(use-package emacs :ensure nil
  :functions (xclip-mode)
  :preface
  ;; TODO Factor out tty-recognition as a predicate
  (defun eon-xclip--ensure-on-tty (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      (unless (display-graphic-p)
        (use-package xclip :ensure t
          :demand t
          :config (xclip-mode 1)))))
  :hook
  (window-setup . eon-xclip--ensure-on-tty)
  (after-make-frame-functions . eon-xclip--ensure-on-tty))

;; No empty lines etc. in the kill ring
;; <https://github.com/NicholasBHubbard/clean-kill-ring.el>
(use-package clean-kill-ring :ensure t
  :config
  (clean-kill-ring-mode))

;;  ____________________________________________________________________________
;;; UNDO / REDO

(use-package undo-fu :ensure t)

;; Keep the undo/redo history, even if you close the file and shutdown Emacs
(use-package undo-fu-session :ensure t
  :custom
  (undo-fu-session-incompatible-files
   '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode))

;;  ____________________________________________________________________________
;; PARENTHESIS DISPLAY

;; Color-code nested parens …
;; <https://github.com/Fanael/rainbow-delimiters>
(use-package rainbow-delimiters :ensure t
  :hook
  ((prog-mode conf-mode comint-mode eshell-mode shell-mode)
   . rainbow-delimiters-mode))

;; … and/or make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   (prog-mode . paren-face-mode))

;;  ____________________________________________________________________________
;;; NAVIGATION

;; Goto visible character
(use-package avy :ensure t
  :bind
  ("M-g c" . avy-goto-char)
  ("M-g l" . avy-goto-line)
  (:map ctl-z-g-map
        ("g" . avy-goto-char)
        ("l" . avy-goto-line)))

;; Goto last change
;; <https://github.com/emacs-evil/goto-chg>
(use-package goto-chg :ensure t
  :bind
  (:map prog-mode-map
        ("M-p" . goto-last-change)
        ("M-n" . goto-last-change-reverse))
  (:map text-mode-map
        ("M-p" . goto-last-change)
        ("M-n" . goto-last-change-reverse)))

;; Expand region
;; <https://github.com/magnars/expand-region.el>
(use-package expand-region :ensure t
  :bind
  ("C-=" . er/expand-region))

;;  ____________________________________________________________________________
;;; COLOR NAMES
;; <https://elpa.gnu.org/packages/rainbow-mode.html>

;; Colorize color names and hex codes in arbitrary buffers
(use-package rainbow-mode :ensure t
  :diminish
  :defer t
  :bind
  (:map ctl-z-x-map
        ("c" . rainbow-mode)))

;;  ____________________________________________________________________________
;;; GIT-GUTTER

;; <https://github.com/emacsorphanage/git-gutter>
(use-package git-gutter :ensure t
  :diminish
  :config
  (global-git-gutter-mode))

;; <https://github.com/emacsorphanage/git-gutter-fringe>
(use-package git-gutter-fringe :ensure t
  :after git-gutter
  :custom
  (git-gutter-fr:side 'left-fringe))

;;  ____________________________________________________________________________
;;; DIRED
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

;; Show/hide dotfiles
;; <https://github.com/mattiasb/dired-hide-dotfiles>
(use-package dired-hide-dotfiles :ensure t
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
  :bind
  (:map eon-localleader-dired-map
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
;;; GREP / RIPGREP
;; <https://github.com/BurntSushi/ripgrep>
;; Ripgrep must be installed on your computer for this to work

;; <https://github.com/dajva/rg.el>
(when (executable-find "rg")
  (use-package rg :ensure t
    :custom
    ;; Inject Ripgrep into the `project-switch-commands' dispatch menu
    (project-switch-commands
     (cl-substitute '(rg-project "Ripgrep" ?g) 'project-find-regexp
                    project-switch-commands
                    :key #'car :test #'eq))
    :bind
    (:map project-prefix-map
          ("g" . rg-project))
    (:map ctl-z-s-map
          ("G" . rg))))

;; <https://github.com/mhayashi1120/Emacs-wgrep/>
(use-package wgrep :ensure t)

;;  ____________________________________________________________________________
;;; SHELL / TERMINAL
;; <https://codeberg.org/akib/emacs-eat>
;; <https://elpa.nongnu.org/nongnu-devel/doc/eat.html>

;; To setup shell integration for GNU Bash, insert at the end of your .bashrc:
;; [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
;; source "$EAT_SHELL_INTEGRATION_DIR/bash"
;;
;; For Zsh, put the following in your .zshrc:
;; [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
;; source "$EAT_SHELL_INTEGRATION_DIR/zsh"

(use-package eat :ensure t
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  :config
  ;; make C-u work in Eat terminals like in normal terminals
  (delete [?\C-u] eat-semi-char-non-bound-keys)
  ;; ditto for C-g
  (delete [?\C-g] eat-semi-char-non-bound-keys)
  (eat-update-semi-char-mode-map)
  ;; Workaround for the need to call eat-reload after changing
  ;; Eat's keymaps, but reloading from :config section causes infinite recursion
  ;; because :config wraps with-eval-after-load.
  (defvar eat--prevent-use-package-config-recursion nil)
  (unless eat--prevent-use-package-config-recursion
    (setq eat--prevent-use-package-config-recursion t)
    (eat-reload))
  (makunbound 'eat--prevent-use-package-config-recursion)
  :hook
  ;; Run Eshell always in eat: `eat-eshell-mode'.
  (eshell-load . eat-eshell-mode)
  ;; Run TUI programs in Eshell: `eat-eshell-visual-command-mode'.
  (eshell-load . eat-eshell-visual-command-mode)
  :bind
  (:map ctl-z-e-map
        ;; Set Eat as terminal emulator
        ("t" . eat)))

(when (executable-find "fish")
  ;; <https://github.com/LemonBreezes/emacs-fish-completion>
  (use-package fish-completion :ensure t
    :config
    (global-fish-completion-mode))
  ;; <https://github.com/wwwjfy/emacs-fish>
  (use-package fish-mode :ensure t))

;;  ____________________________________________________________________________
;;; ORG MODE EXTENSIONS

;; <https://github.com/alphapapa/org-sticky-header>
(use-package org-sticky-header :ensure t
  :init
  (defun eon-org--sticky-header-maybe ()
    "Enable org-sticky-header unless in *scratch* buffer."
    (unless (string= (buffer-name) "*scratch*")
      (org-sticky-header-mode 1)))
  :hook
  (org-mode . eon-org--sticky-header-maybe))

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

;;  ____________________________________________________________________________
;;; LISP

;; Sophisticated macro expander for Emacs Lisp
(use-package macrostep :ensure t
  :bind
  (:map eon-localleader-elisp-map
        ("m" . #'macrostep-expand)
        ("c" . #'macrostep-collapse)
        ("C" . #'macrostep-collapse-all)
        ("[" . #'macrostep-prev-macro)
        ("]" . #'macrostep-next-macro)))

;;  ____________________________________________________________________________
(provide 'eon-core)
;;; eon-core.el ends here
