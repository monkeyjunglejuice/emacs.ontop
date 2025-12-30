;;; eon-base.el --- Shared packages and definitions -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; EXEC PATH FROM SHELL
;; <https://github.com/purcell/exec-path-from-shell>

;; Make Emacs use the $PATH and other environment variables set up by your
;; shell; especially helpful when on MacOS, or starting Emacs via Systemd or
;; similar, who don't adopt certain environment variables.

(use-package exec-path-from-shell :ensure t
  :when
  (or (daemonp)
      ;; Enable on macOS if not started from the terminal
      (and (eon-macp)
           (not (eon-terminalp))
           (not (getenv "TERM_PROGRAM"))))

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

  ;; Use a specific shell if you need:
  ;; (exec-path-from-shell-shell-name "/bin/bash")

  :config

  (eon-exec-path-from-shell-refresh))

;; _____________________________________________________________________________
;;; SHELL

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Fish

(when (executable-find "fish")

  ;; <https://github.com/LemonBreezes/emacs-fish-completion>
  (use-package fish-completion :ensure t
    :config
    (global-fish-completion-mode))

  ;; <https://github.com/wwwjfy/emacs-fish>
  (use-package fish-mode :ensure t))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Bash

;; <https://github.com/szermatt/emacs-bash-completion>
(when (executable-find "bash")

  (use-package bash-completion :ensure t
    :config
    (bash-completion-setup)))

;; _____________________________________________________________________________
;;; OS INTEGRATION

;; Use the MacOS trash instead of freedesktop.org ~/.local/share/Trash
;; <https://github.com/emacsorphanage/osx-trash>
(when (eon-macp)
  (use-package osx-trash :ensure t
    :config
    (osx-trash-setup)))

;; Adapt title bar to macOS theme
;; <https://github.com/purcell/ns-auto-titlebar>
(when (eon-macp)
  (use-package ns-auto-titlebar :ensure t
    :config
    (ns-auto-titlebar-mode)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Manage OS packages from Emacs
;; <https://gitlab.com/jabranham/system-packages>

;; System-packages is an Emacs frontend to various system package managers.
;; It attempts to guess which system package manager to use,
;; and lets you manage your system packages directly from Emacs via
;; "M-x system-packages".
;;
;; If the detected package manager is wrong or you prefer a different one,
;; then set `system-packages-package-manager' directly, e.g.
;; (setopt system-packages-package-manager 'pacman) in your 'init.el',
;; or use the Customization UI "M-x customize-group RET system-packages RET".
;;
;; Default mapping from Emacs commands to package manager commands defined by
;; `system-packages-supported-package-managers'.

(use-package system-packages :ensure t

  :config

  ;; Rerun detection logic, because 'brew' seems not recognized with Emacs
  ;; started from macOS app (Emacs started from the terminal works).
  ;;
  ;; In the current upstream system-packages.el the detection is done inside
  ;; the defcustom’s initform.
  ;;
  ;; If that initform was evaluated when the file was compiled/loaded in
  ;; an Emacs that didn’t have brew (or any of the others) on $PATH,
  ;; then the result of the initform is nil, and that nil is what Emacs keeps
  ;; as the value.
  ;;
  ;; So later, even though now (executable-find "brew") is non-nil,
  ;; the variable stays nil, because defcustom doesn’t re-run its initform
  ;; on every load.

  (when (null system-packages-package-manager)
    ;; Re-evaluate the original initform the package used
    (setopt system-packages-package-manager
            (eval (car (get 'system-packages-package-manager
                            'standard-value)))))

  (when (null system-packages-use-sudo)
    (let* ((entry (assoc system-packages-package-manager
                         system-packages-supported-package-managers))
           (sudo (cdr (assoc 'default-sudo (cdr entry)))))
      (when sudo
        (setopt system-packages-use-sudo sudo))))

  ;; Use an `eshell' buffer instead of `async-shell-command'

  (defcustom eon-system-packages-backend 'eshell
    "Run system-packages commands via this backend.
- 'eshell' provides a nicer output and interactivity afterwards.
- 'async-shell-command' dumps the output verbatim into a simple buffer."
    :type '(choice (const eshell) (const async-shell-command))
    :group 'eon-misc)

  (defun eon-system-packages--eshell (cmd dir)
    (let ((buf (get-buffer "*system-packages*")))
      (unless (and buf (buffer-live-p buf)
                   (with-current-buffer buf
                     (derived-mode-p 'eshell-mode)))
        (setq buf (get-buffer-create "*system-packages*"))
        (with-current-buffer buf
          (setq default-directory dir)
          (eshell-mode)))
      (with-current-buffer buf
        (setq default-directory dir)
        (goto-char (point-max))
        (insert cmd)
        (eshell-send-input))
      (pop-to-buffer buf)
      buf))

  (defun eon-system-packages--run (orig action &optional pack args)
    (let* ((cmd (system-packages-get-command action pack args))
           (dir (if system-packages-use-sudo "/sudo::" default-directory)))
      (if (eq eon-system-packages-backend 'eshell)
          (eon-system-packages--eshell cmd dir)
        (let ((default-directory dir))
          (funcall orig action pack args)))))

  (advice-add 'system-packages--run-command
              :around #'eon-system-packages--run))

;; _____________________________________________________________________________
;;; USER INTERFACE

;; Hide or alter the mode-line strings of certain minor modes
;; <https://github.com/myrjola/diminish.el>
(use-package diminish :ensure t)

;; Hydra
;; <https://github.com/abo-abo/hydra>
(use-package hydra :ensure t)

;; _____________________________________________________________________________
;;; ORDERLESS
;; <https://github.com/oantolin/orderless>

(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles . (basic partial-completion))))))

;; _____________________________________________________________________________
;;; COPY / PASTE

;; Copy/paste between TUI Emacs and graphical applications
;; Only installed and loaded if Emacs really runs in the terminal UI,
;; e.g. via 'emacs -nw'.
;; <https://elpa.gnu.org/packages/xclip.html>
(when (eon-terminalp)
  (use-package xclip :ensure t
    :config (xclip-mode 1)))

;; No empty lines etc. in the kill ring
;; <https://github.com/NicholasBHubbard/clean-kill-ring.el>
(use-package clean-kill-ring :ensure t
  :config
  (clean-kill-ring-mode))

;; _____________________________________________________________________________
;;; UNDO / REDO

(use-package undo-fu :ensure t)

;; Keep the undo/redo history, even if you close the file and shutdown Emacs
(use-package undo-fu-session :ensure t
  :custom
  (undo-fu-session-incompatible-files
   '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode))

;; _____________________________________________________________________________
;; PARENTHESIS DISPLAY

;; Color-code nested parens
;; <https://github.com/Fanael/rainbow-delimiters>
(use-package rainbow-delimiters :ensure t
  :hook
  ((prog-mode conf-mode comint-mode eshell-mode shell-mode)
   . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face :ensure t
;;   :hook
;;   (prog-mode . paren-face-mode))

;; _____________________________________________________________________________
;;; NAVIGATION

;; Goto visible character
(use-package avy :ensure t
  :bind
  ("M-g c" . avy-goto-char-timer)
  ("M-g l" . avy-goto-line)
  (:map ctl-z-g-map
        ("g" . avy-goto-char-timer)
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

;; _____________________________________________________________________________
;;; COLOR NAMES
;; <https://elpa.gnu.org/packages/rainbow-mode.html>

;; Colorize color names and hex codes in arbitrary buffers
(use-package rainbow-mode :ensure t
  :diminish
  :defer t
  :bind
  (:map ctl-z-x-map
        ("c" . rainbow-mode)))

;; _____________________________________________________________________________
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
  ;; Display the indicators on which side?
  (git-gutter-fr:side 'left-fringe))

;; <https://github.com/dgutov/diff-hl>
;; TODO Consider as an alternative
(use-package diff-hl :ensure t
  :init
  (global-diff-hl-mode)
  :custom
  ;; Consider disabling on slow or high-latency connections
  (diff-hl-disable-on-remote nil)
  ;; Don't block the main Emacs thread
  (diff-hl-update-async t)
  ;; Reduce visual noise
  (diff-hl-draw-borders nil)
  ;; Display the indicators on which side?
  (diff-hl-side 'right)
  :hook

;; _____________________________________________________________________________
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

;; _____________________________________________________________________________
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

;; _____________________________________________________________________________
;;; MARKUP, CONFIG AND SERIALIZATION FORMATS

;; <https://github.com/emacsorphanage/adoc-mode>
(use-package adoc-mode :ensure t
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

;; _____________________________________________________________________________
;;; LISP

;; Sophisticated macro expander for Emacs Lisp
(use-package macrostep :ensure t
  :bind
  (:map eon-localleader-elisp-map
        ("m" . #'macrostep-expand)))

;; _____________________________________________________________________________
(provide 'eon-base)
;;; eon-base.el ends here
