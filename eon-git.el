;;; eon-git.el --- Git tools -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; MAGIT
;; <https://magit.vc/>

(use-package magit :ensure t
  :custom
  ;; How many directoriess deep Magit looks for Git repos
  (magit-repository-directories '(("~/" . 1)))
  ;; Inject Magit into the `project-switch-commands' dispatch menu
  (project-switch-commands
   (cl-substitute '(magit-status "Magit" ?v) 'project-vc-dir
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
        ("," . magit-dispatch)
        ("k" . eon-magit-kill-buffers)))

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
