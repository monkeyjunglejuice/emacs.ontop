;;; eon-agent-shell.el --- ACP-powered frontend for coding agents -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
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
;; URL: <https://github.com/xenodium/agent-shell>
;;      <https://xenodium.com/introducing-agent-shell>
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon eon-ai))

;; _____________________________________________________________________________
;;; AGENT SHELL

(use-package agent-shell :ensure t

  :custom

  (agent-shell-display-action '(display-buffer-pop-up-window))
  (agent-shell-header-style 'text)
  (agent-shell-highlight-blocks nil)
  (agent-shell-show-welcome-message nil)

  :bind

  (:map ctl-z-l-map
        ("s" . agent-shell)))

;; _____________________________________________________________________________
(provide 'eon-agent-shell)
;;; eon-agent-shell.el ends here
