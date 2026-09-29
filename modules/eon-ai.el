;;; eon-ai.el --- Generic AI integration helpers -*- lexical-binding: t; no-byte-compile: t; -*-

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
 :requires  '(eon))

;; _____________________________________________________________________________
;;; OPENAI-COMPATIBLE APIS

(require 'subr-x)
(require 'url)

(defun eon-openai-list-models (api-base-url)
  "Return model IDs exposed by OpenAI-compatible API-BASE-URL.

API-BASE-URL is the URL prefix preceding the `/v1' API path."
  (let ((url
         (concat
          (string-trim-right api-base-url "/+")
          "/v1/models")))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let* ((response
              (json-parse-buffer
               :object-type 'plist
               :array-type 'list))
             (models (plist-get response :data)))
        (unless (plist-member response :data)
          (error "Response from %s has no `data' member" url))
        (mapcar
         (lambda (model)
           (let ((id (plist-get model :id)))
             (unless (stringp id)
               (error "Invalid model entry from %s: %S"
                      url model))
             id))
         models)))))

;; _____________________________________________________________________________
(provide 'eon-ai)
;;; eon-ai.el ends here
