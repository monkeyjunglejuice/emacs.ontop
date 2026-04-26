;;; eon-ollama.el --- Local and cloud LLMs -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
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

(defun eon-ollama-models (type &optional prefix)
  "Return a list of installed Ollama models.
TYPE must be either 'string or 'symbol. PREFIX, when non-nil, must be a
string with no whitespace (space, tab, newline, CR, FF, VT) and is
prepended to each model name."
  (unless (memq type '(string symbol))
    (user-error "TYPE must be 'string or 'symbol, got: %S" type))
  (when (and prefix
             (or (not (stringp prefix))
                 (string-match-p "[ \t\n\r\f\v]" prefix)))
    (user-error "PREFIX must be a string without whitespace: %S" prefix))
  (let* ((out (shell-command-to-string "ollama list"))
         (lines (split-string out "\n" t))
         (rows (cdr lines))  ; drop header
         (names (mapcar (lambda (line) (car (split-string line)))
                        rows))
         (strings (if prefix
                      (mapcar (lambda (s) (concat prefix s)) names)
                    names)))
    (if (eq type 'string)
        strings
      (mapcar #'intern strings))))

(defcustom eon-ollama-default-model
  (car (eon-ollama-models 'symbol))
  "Default Ollama model."
  :type
  (let ((syms (eon-ollama-models 'symbol)))
    `(choice
      ,@(if syms
            (mapcar (lambda (s)
                      `(const :tag ,(symbol-name s) ,s))
                    syms)
          '((const :tag "No models found" nil)))))
  :set (lambda (sym val)
         (let ((choices (eon-ollama-models 'symbol)))
           (unless (or (memq val choices)
                       (and (null choices) (null val)))
             (user-error "Model %S not installed. Choices: %S"
                         val choices))
           (set-default sym val)))
  :safe (lambda (v)
          (let ((choices (eon-ollama-models 'symbol)))
            (or (memq v choices)
                (and (null choices) (null v)))))
  :group 'eon-ai)

;; _____________________________________________________________________________
(provide 'eon-ollama)
;;; eon-ollama.el ends here
