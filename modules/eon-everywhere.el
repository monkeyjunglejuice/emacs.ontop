;;; eon-everywhere.el --- Use Emacs for text input in other apps -*- lexical-binding: t; no-byte-compile: t; -*-

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
;; Emacs-everywhere needs a way to launch when a not-emacs application has focus.
;; The easiest way to do this is to have a global keybinding/keyboard shortcut
;; dedicated to launching:
;;
;; emacsclient --eval "(emacs-everywhere)"
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; EMACS EVERYWHERE
;; <https://github.com/tecosaur/emacs-everywhere>

(use-package emacs-everywhere :ensure t
  :custom
  (emacs-everywhere-major-mode-function #'text-mode))

;; _____________________________________________________________________________
(provide 'eon-everywhere)
;;; eon-everywhere.el ends here
