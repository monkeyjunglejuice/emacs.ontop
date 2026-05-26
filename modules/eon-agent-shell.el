;;; eon-agent-shell.el --- ACP-powered frontend for coding agents -*- lexical-binding: t; no-byte-compile: t; -*-

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

  :init

  (defvar-keymap ctl-z-a-map :doc "AI Agents")
  (keymap-set ctl-z-map "a" `("Agent" . ,ctl-z-a-map))

  (eon-localleader-defkeymap
      agent-shell-mode eon-localleader-agent-shell-map
    :doc "Local leader keymap for `agent-shell-mode'."
    "r"   #'agent-shell-reload
    "C-r" #'agent-shell-restart
    "m"   #'agent-shell-help-menu)

  :custom

  (agent-shell-display-action '(display-buffer-pop-up-window))
  (agent-shell-header-style 'text)
  (agent-shell-highlight-blocks nil)
  (agent-shell-show-welcome-message nil)

  :bind

  (:map ctl-z-a-map
        ("a"   . agent-shell)
        ("."   . agent-shell-send-dwim)
        ("f"   . agent-shell-send-file)
        ("F"   . agent-shell-send-file-to)
        ("r"   . agent-shell-send-region)
        ("R"   . agent-shell-send-region-to)))

;; _____________________________________________________________________________
(provide 'eon-agent-shell)
;;; eon-agent-shell.el ends here
