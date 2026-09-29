;;; eon-ai.el --- Shared functionality for AI integration -*- lexical-binding: t; no-byte-compile: t; -*-

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
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; GLOBAL DEFINITIONS

(defgroup eon-ai nil
  "AI integration."
  :group 'eon)

(require 'json)
(require 'url)

(defun eon-openai-list-models (api-base-url)
  "Return model IDs exposed by OpenAI-compatible API-BASE-URL."
  (let ((url
         (concat
          (string-remove-suffix "/" api-base-url)
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
               (error "Invalid model entry from %s: %S" url model))
             id))
         models)))))

;; _____________________________________________________________________________
(provide 'eon-ai)
;;; eon-ai.el ends here
