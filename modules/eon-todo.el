;;; eon-todo.el --- Highlight todo keywords in comments -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience tools
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; HIGHLIGHT KEYWORDS IN COMMENTS
;; <https://github.com/tarsius/hl-todo>

(use-package hl-todo :ensure t
  :config
  (global-hl-todo-mode)
  :bind
  (:map ctl-z-i-map
        ("t" . #'hl-todo-insert)))

(when (eon-modulep 'eon-consult)
  (use-package consult-todo :ensure t
    :after (hl-todo consult)
    :bind
    (:map ctl-z-g-map
          ("t" . consult-todo)
          ("T" . consult-todo-all))))

;; _____________________________________________________________________________
(provide 'eon-todo)
;;; eon-todo.el ends here
