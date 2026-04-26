;;; eon-activities.el --- Suspend/resume windows, buffers, etc. -*- lexical-binding: t; no-byte-compile: t; -*-

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
        ("D"   . activities-discard)
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
