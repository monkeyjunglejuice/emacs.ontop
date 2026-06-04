;;; eon-git.el --- Magit user interface and friends -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.1
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
;;; MAGIT
;; <https://magit.vc/>

(use-package magit :ensure t

  :custom

  ;; How many directoriess deep magit looks for git repos
  (magit-repository-directories '(("~/" . 1)))

  ;; Inject Magit into the `project-switch-commands' dispatch menu
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
        ("," . magit-dispatch)
        ("c" . magit-clone-shallow)
        ("C" . magit-clone)
        ("f" . magit-file-dispatch)
        ("k" . eon-magit-kill-buffers)
        ("p" . magit-patch-apply)
        ("P" . magit-patch)
        ("r" . magit-file-rename)
        ("v" . magit-status)
        ("V" . magit-project-status))

  :hook

  ;; Update the Magit status when a file has been saved
  (after-save . magit-after-save-refresh-status))

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
