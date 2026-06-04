;;; eon-base.el --- Shared packages and definitions -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.2.0
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; EXEC PATH FROM SHELL
;; <https://github.com/purcell/exec-path-from-shell>

;; Make Emacs use the $PATH and other environment variables set up by your
;; shell; especially helpful when on MacOS, or starting Emacs via Systemd or
;; similar, who don't adopt certain environment variables.

(when (or
       ;; Always enable when Emacs starts as daemon
       (daemonp)
       ;; Enable on macOS if not started from the terminal
       (and (eon-macp)
            (not (eon-terminalp))
            (not (getenv "TERM_PROGRAM"))))

  (use-package exec-path-from-shell :ensure t

    :preface

    (defvar eon-exec-path-from-shell-blocklist
      ;; Shell/process state from temporary shell
      '("^HOME$"
        "^\\(OLD\\)?PWD$"
        "^SHLVL$"
        "^_$"

        ;; Interactive shell state
        "^PS1$"
        "^R?PROMPT$"
        "^COLUMNS$"
        "^LINES$"

        ;; Terminal state
        "^TERM\\(CAP\\)?$"
        "^TERM_PROGRAM\\(_VERSION\\)?$"

        ;; Identity variables that GUI Emacs already has
        "^USER$"
        "^LOGNAME$"

        ;; Emacs recursion / nested shell state
        "^INSIDE_EMACS$"

        ;; Per-process Git overrides
        "^GIT_CONFIG"

        ;; Display/session state
        "^\\(WAYLAND_\\)?DISPLAY$"
        "^DBUS_SESSION_BUS_ADDRESS$"
        "^XAUTHORITY$"

        ;; XDG runtime/session state
        "^XDG_CURRENT_DESKTOP$"
        "^XDG_RUNTIME_DIR$"
        "^XDG_\\(VTNR$\\|SEAT$\\|BACKEND$\\|SESSION_\\)"

        ;; macOS launch/session state
        "^TMPDIR$"
        "^XPC_"
        "^SECURITYSESSIONID$"
        "^LaunchInstanceID$"
        "^COMMAND_MODE$"

        ;; WSL process state
        "^WSL_INTEROP$"

        ;; Socket-like variables; these are commonly stale when copied
        "\\(_SOCK\\|_SOCKET\\)$"

        ;; SSH/GPG agent and terminal state
        "^SSH_\\(AUTH_SOCK\\|AGENT_PID\\)$"
        "^\\(SSH\\|GPG\\)_TTY$"
        "^GPG_AGENT_INFO$")
      "Regexps for environment variable names not imported from the shell.")

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

    (eon-exec-path-from-shell-refresh)))

;; _____________________________________________________________________________
;;; SYSTEM INTEGRATION

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - MacOS Trash

;; Use the MacOS trash instead of freedesktop.org ~/.local/share/Trash
;; <https://github.com/emacsorphanage/osx-trash>
(when (eon-macp)
  (use-package osx-trash :ensure t
    :config
    (osx-trash-setup)))

;; _____________________________________________________________________________
;;; SHELLS

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Bash

;; <https://github.com/szermatt/emacs-bash-completion>
(when (executable-find "bash")
  (use-package bash-completion :ensure t
    :custom
    (bash-completion-nospace t)
    :config
    (bash-completion-setup)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Fish

(when (executable-find "fish")

  ;; <https://github.com/LemonBreezes/emacs-fish-completion>
  (use-package fish-completion :ensure t
    :config
    (global-fish-completion-mode))

  ;; <https://github.com/wwwjfy/emacs-fish>
  (use-package fish-mode :ensure t))

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
;;; COMPLETION

;; <https://github.com/oantolin/orderless>
(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless))
  :config
  (eon-add-to-list* 'completion-category-overrides
                    '((file
                       ;; `basic' must be first to work with Tramp
                       (styles basic partial-completion))
                      ;; Explicitly use Orderless for Eglot
                      (eglot
                       (styles orderless))
                      (eglot-capf
                       (styles orderless)))))

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

;; _____________________________________________________________________________
;;; USER INTERFACE

;; Hide or alter the mode-line strings of certain minor modes
;; <https://github.com/myrjola/diminish.el>
(use-package diminish :ensure t)

;; Hydra
;; <https://github.com/abo-abo/hydra>
(use-package hydra :ensure t)

;; Adapt title bar to macOS theme
;; <https://github.com/purcell/ns-auto-titlebar>
(when (eon-macp)
  (use-package ns-auto-titlebar :ensure t
    :hook
    (after-init . ns-auto-titlebar-mode)))

;; _____________________________________________________________________________
;;; RENDER COLOR CODES/NAMES VISUALLY
;; <https://elpa.gnu.org/packages/rainbow-mode.html>

;; Colorize color names and hex codes in arbitrary buffers: "<leader> x c"
(use-package rainbow-mode :ensure t
  :diminish
  :defer t
  :bind
  (:map ctl-z-x-map
        ("c" . rainbow-mode)))

;; _____________________________________________________________________________
;;; ORG MODE EXTENSIONS

;; <https://github.com/alphapapa/org-sticky-header>
(use-package org-sticky-header :ensure t
  :bind
  (:map eon-localleader-org-mode-map
        ("^" . org-sticky-header-mode)))

;; _____________________________________________________________________________
;;; MARKUP-, CONFIG- AND SERIALIZATION FORMATS

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

(use-package yaml-ts-mode :ensure nil
  :mode (("\\.yaml\\'" . yaml-ts-mode)))

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
