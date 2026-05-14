;;; eon-ollama.el --- Local and cloud LLMs -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.1.0
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience tools
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
 :requires  '(eon eon-ai))

;; _____________________________________________________________________________
;;; OLLAMA

(defun eon-ollama-getenv-context-window (var)
  "Return environment VAR as an integer, or nil.

Only non-negative decimal integer values are accepted."
  (let ((val (getenv var)))
    (when (and val
               (string-match-p "\\`[0-9]+\\'" val))
      (string-to-number val))))

(defcustom eon-ollama-default-context-window
  (eon-ollama-getenv-context-window "OLLAMA_CONTEXT_LENGTH")
  "Default context window size for Ollama models.

When nil, no explicit context window size is configured."
  :type '(choice
          (const :tag "Unspecified" nil)
          integer)
  :group 'eon-ai)

(defcustom eon-ollama-default-capabilities
  '(media tool-use json url responses-api)
  "Default capabilities for Ollama models."
  :type '(repeat symbol)
  :group 'eon-ai)

(defcustom eon-ollama-default-mime-types
  '("image/jpeg" "image/png" "image/gif" "image/webp")
  "Default MIME types supported by Ollama models."
  :type '(repeat string)
  :group 'eon-ai)

(defun eon-ollama-models (type &optional prefix)
  "Return a list of installed Ollama models.

TYPE must be either `string' or `symbol'. PREFIX, when non-nil, must be a
string with no whitespace and is prepended to each model name."
  (unless (memq type '(string symbol))
    (user-error "TYPE must be `string' or `symbol', got: %S" type))
  (when (and prefix
             (or (not (stringp prefix))
                 (string-match-p "[ \t\n\r\f\v]" prefix)))
    (user-error "PREFIX must be a string without whitespace: %S" prefix))
  (let* ((output (shell-command-to-string "ollama list"))
         (lines (split-string output "\n" t))
         (rows (cdr lines))
         (names (mapcar (lambda (line)
                          (car (split-string line)))
                        rows))
         (models (if prefix
                     (mapcar (lambda (model)
                               (concat prefix model))
                             names)
                   names)))
    (if (eq type 'string)
        models
      (mapcar #'intern models))))

(defcustom eon-ollama-default-model
  (car (eon-ollama-models 'symbol))
  "Default Ollama model.

If not explicitly specified, set the first model from the list."
  :type
  (let ((models (eon-ollama-models 'symbol)))
    `(choice
      ,@(if models
            (mapcar (lambda (model)
                      `(const :tag ,(symbol-name model) ,model))
                    models)
          '((const :tag "No models found" nil)))))
  :set
  (lambda (symbol value)
    (let ((models (eon-ollama-models 'symbol)))
      (unless (or (memq value models)
                  (and (null models) (null value)))
        (user-error "Model %S not installed. Choices: %S"
                    value models))
      (set-default symbol value)))
  :safe
  (lambda (value)
    (let ((models (eon-ollama-models 'symbol)))
      (or (memq value models)
          (and (null models) (null value)))))
  :group 'eon-ai)

;; _____________________________________________________________________________
(provide 'eon-ollama)
;;; eon-ollama.el ends here
