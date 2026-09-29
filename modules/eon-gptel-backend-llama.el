;;; eon-gptel-backend-llama.el --- Gptel preset for Llama.cpp backend -*- lexical-binding: t; no-byte-compile: t; -*-

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
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon eon-ai eon-gptel))

;; _____________________________________________________________________________
;;; GPTEL LLAMA.CPP BACKEND
;; <https://github.com/karthink/gptel>

(require 'url-parse)

(defcustom eon-gptel-backend-llama-api-base-url
  "http://localhost:9931"
  "Base URL of the Llama.cpp OpenAI-compatible API."
  :type 'string
  :group 'eon-ai)

(defun eon-gptel-backend-llama-url-parts ()
  "Return protocol, host and endpoint for the configured Llama.cpp API."
  (let* ((url
          (url-generic-parse-url
           eon-gptel-backend-llama-api-base-url))
         (path
          (string-remove-suffix "/" (or (url-filename url) "")))
         (port
          (url-portspec url)))
    (list
     (url-type url)
     (if port
         (format "%s:%d" (url-host url) port)
       (url-host url))
     (concat path "/v1/chat/completions"))))

(use-package gptel-openai :ensure nil
  :after gptel

  :config

  (pcase-let
      ((`(,protocol ,host ,endpoint)
        (eon-gptel-backend-llama-url-parts)))
    (gptel-make-openai "Llama.cpp"
      :protocol protocol
      :host host
      :endpoint endpoint
      :stream t
      :models
      (eon-openai-list-models
       eon-gptel-backend-llama-api-base-url)))

  (defun eon-gptel-backend-llama-set-default ()
    "Set the registered Llama.cpp backend as Gptel's default."
    (interactive)
    (setopt gptel-backend
            (gptel-get-backend "Llama.cpp"))))

;; _____________________________________________________________________________
(provide 'eon-gptel-backend-llama)
;;; eon-gptel-backend-llama.el ends here
