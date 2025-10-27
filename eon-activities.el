;;; eon-activities.el --- Suspend/resume windows, buffers, etc. -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Activities for Emacs - suspend and resume activities,
;; i.e. buffers, frames/tabs and their windows
;;
;;; Code:

;; _____________________________________________________________________________
;;; ACTIVITIES
;; <https://github.com/alphapapa/activities.el>

(use-package activities :ensure t

  :init

  (defvar-keymap ctl-z-a-map :doc "Activity")
  (keymap-set ctl-z-map "a" `("Activity" . ,ctl-z-a-map))

  (activities-mode)
  (activities-tabs-mode)

  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :custom

  (activities-kill-buffers t)

  :bind

  (:map ctl-z-a-map
   ("d"   . activities-define)
   ("k"   . activities-kill)
   ("l"   . activities-list)
   ("n"   . activities-new)
   ("r"   . activities-rename)
   ("a"   . activities-resume)
   ("g"   . activities-revert)
   ("c"   . activities-suspend)
   ("RET" . activities-switch)
   ("b"   . activities-switch-buffer)))

;; _____________________________________________________________________________
(provide 'eon-activities)
;;; eon-activities.el ends here
