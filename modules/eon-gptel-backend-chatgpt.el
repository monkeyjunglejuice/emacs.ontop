;;; eon-gptel-backend-chatgpt.el --- Gptel preset for ChatGPT backend -*- lexical-binding: t; no-byte-compile: t; -*-

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
;; ChatGPT Plus or Pro subscription needed (this is different from OpenAI API).
;; Authenticate via "M-x gptel-openai-oauth-login".
;; 
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon-gptel))

;; _____________________________________________________________________________
;;; GPTEL CHATGPT BACKEND
;; <https://github.com/karthink/gptel>

(use-package gptel-openai-oauth :ensure nil
  :after gptel

  :config

  (gptel-make-openai-oauth "ChatGPT")

  (defun eon-gptel-backend-chatgpt-set-default ()
    "Set the registered ChatGPT backend as Gptel's default."
    (interactive)
    (setopt gptel-backend
            (gptel-get-backend "ChatGPT"))))

;; _____________________________________________________________________________
(provide 'eon-gptel-backend-chatgpt)
;;; eon-gptel-backend-chatgpt.el ends here
