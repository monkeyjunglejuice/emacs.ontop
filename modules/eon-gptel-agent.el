;;; eon-gptel-agent.el --- Tools and presets for Gptel -*- lexical-binding: t; no-byte-compile: t; -*-

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
;; URL: https://github.com/karthink/gptel-agent
;; 
;; What is Gptel-agent?
;; <https://github.com/karthink/gptel-agent/issues/64>
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon eon-ai eon-gptel))

;; _____________________________________________________________________________
;;; GPTEL
;; <https://github.com/karthink/gptel>

(use-package gptel-agent :ensure t

  :config
  
  ;; Read files from agents directories
  (gptel-agent-update)

  :bind

  (:map ctl-z-l-map
        ("a" . gptel-agent)))

;; _____________________________________________________________________________
(provide 'eon-gptel-agent)
;;; eon-gptel-agent.el ends here
