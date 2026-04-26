;;; eon-git.el --- Magit user interface and friends -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; MAGIT
;; <https://magit.vc/>

(use-package magit :ensure t

  :custom

  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; How many directoriess deep magit looks for git repos
  (magit-repository-directories '(("~/" . 1)))
  ;; Inject magit into the `project-switch-commands' dispatch menu
  (project-switch-commands
   (cl-substitute '(magit-project-status "Magit" ?v) 'project-vc-dir
                  project-switch-commands
                  :key #'car :test #'eq))

  :config

  (defun eon-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  :bind

  (:map ctl-z-v-map
        ("v" . magit-status)
        ("V" . magit-project-status)
        ("f" . magit-file-dispatch)
        ("k" . eon-magit-kill-buffers)
        ("r" . magit-file-rename)
        ("," . magit-dispatch)))

;; _____________________________________________________________________________
;;; GIT TIMEMACHINE
;; <https://codeberg.org/pidu/git-timemachine>

(use-package git-timemachine :ensure t
  :bind
  (:map ctl-z-v-map
        ("t" . git-timemachine-toggle)))

;; _____________________________________________________________________________
(provide 'eon-git)
;;; eon-git.el ends here
