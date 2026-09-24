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
;; <https://github.com/xenodium/agent-shell>

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
;;; TRAMP SUPPORT
;; <https://github.com/junyi-hou/agent-shell-tramp>
;;
;; When the agent runs as a different user, in a container or remote and that
;; separation is represented to Emacs through a Tramp path.
;;
;; Typical cases:
;; - Another local user, e.g. /sudo:agent@localhost:/home/agent/project/
;; - A remote host, e.g. /ssh:user@host:/code/project/
;; - A container, if accessed through a TRAMP container method any other
;;   environment where Emacs sees the project through Tramp.
;;
;; Its job is mainly to bridge the pathname/process
;; boundary between Emacs and the ACP agent.

(use-package agent-shell-tramp
  :vc (:url "https://github.com/junyi-hou/agent-shell-tramp"
       :rev :newest)
  :after agent-shell
  :config
  (agent-shell-tramp-mode 1))

;; _____________________________________________________________________________
(provide 'eon-agent-shell)
;;; eon-agent-shell.el ends here
