;;; eon-gptel.el --- Comprehensive LLM integration -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.1.1
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
 :requires  '(eon eon-ai))

;; _____________________________________________________________________________
;;; GPTEL
;; <https://github.com/karthink/gptel>

(use-package gptel :ensure t

  :init

  (defvar-keymap ctl-z-l-map :doc "Large Language Models / AI")
  (keymap-set ctl-z-map "l" `("LLM/AI" . ,ctl-z-l-map))

  ;; Gptel requires package `transient' version 0.7.4 or later.
  (unless (package-installed-p 'transient '(0 7 4))
    (package-upgrade 'transient))

  :custom

  (gptel-default-mode 'org-mode)

  :bind

  (:map ctl-z-l-map
        ("l"   . gptel)
        ("RET" . gptel-send)
        ("k"   . gptel-abort)
        ("m"   . gptel-menu)
        ("o"   . gptel-org-set-topic)
        ("O"   . gptel-org-set-properties)
        ("p"   . gptel-system-prompt)
        ("P"   . gptel-preset)
        ("r"   . gptel-rewrite)
        ("t"   . gptel-tools)))


(use-package gptel-context :ensure nil

  :bind

  (:map ctl-z-l-map
        ("c"   . gptel-context-add)
        ("f"   . gptel-context-add-file)
        ("y"   . gptel-context-add-current-kill)
        ("C-d" . gptel-context-remove-all))

  (:map gptel-context-buffer-mode-map
        ("["   . gptel-context-previous)
        ("]"   . gptel-context-next)
        ("."   . gptel-context-visit)
        ("d"   . gptel-context-flag-deletion)
        ("C-d" . gptel-context-remove-all)
        ("x"   . gptel-context-confirm)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Ollama
;; Module `eon-ollama' must be enabled/loaded to make Gptel aware of Ollama.

(when (and (eon-modulep 'eon-ollama)
           (executable-find "ollama"))

  (use-package gptel-ollama :ensure nil
    :after gptel

    :init

    ;; Module with common definitions and functionality is required.
    (eon-load-module 'eon-ollama)

    :config

    (defun eon-gptel-ollama-model-spec (model)
      "Return a Gptel model specification for Ollama MODEL."
      (append
       (list model
             :capabilities (copy-sequence eon-ollama-default-capabilities)
             :mime-types (copy-sequence eon-ollama-default-mime-types))
       (when eon-ollama-default-context-window
         ;; Gptel expects not raw token count like Ollama, but divided by 1024;
         ;; e.g. 65536 becomes 64
         (list :context-window (/ eon-ollama-default-context-window 1024)))))

    (defun eon-gptel-ollama-model-specs ()
      "Return installed Ollama models as Gptel model specifications."
      (mapcar #'eon-gptel-ollama-model-spec
              (eon-ollama-models 'symbol)))

    (defun eon-gptel-register-ollama-backend ()
      "Register local/cloud Ollama models with Gptel."
      (setopt gptel-backend
              (gptel-make-ollama "Ollama"
                :host "localhost:11434"
                :endpoint "/api/chat"
                :stream t
                :models (eon-gptel-ollama-model-specs)))
      (when eon-ollama-default-model
        (setopt gptel-model eon-ollama-default-model)))

    ;; Register once during init
    (eon-gptel-register-ollama-backend)))

;; _____________________________________________________________________________
;;; GPTEL PROMPTS
;; <https://github.com/jwiegley/gptel-prompts>

;; Put your prompt files in `gptel-prompts-directory' (defaults to
;; "~/.emacs.d/prompts"). Whenever files change, gptel-prompts-update refreshes
;; gptel-directives automatically.
;;
;; Prompt formats
;; Each file in the prompts directory becomes a named directive. The filename
;; (sans extension) is the key. There are several supported formats:
;;
;; - Plain text (.txt, .md, .org)
;; The file content is used directly as a system prompt string. This is the
;; simplest option -- just write your prompt and save it.
;;
;; - Emacs Lisp (.el, .eld)
;; For .eld files, the content is read as a Lisp data structure -- a list of
;; strings and/or symbols, matching what gptel-directives expects. For .el
;; files, the content is evaluated as Emacs Lisp and should return the same kind
;; of list. See the gptel-directives documentation for the expected format.
;;
;; - JSON (.json)
;; JSON files can contain either a simple string (used as a system prompt) or an
;; array of objects with role and content fields for multi-turn conversations.
;;
;; - Prompt Poet (.poet, .jinja, .j2)
;; Based on Prompt Poet, these files are YAML with Jinja templating. The
;; templating is applied dynamically when the prompt is used, so you can see the
;; expansion results via GPTel's Inspect capabilities when gptel-expert-commands
;; is non-nil.

(use-package gptel-prompts
  :vc (:url "https://github.com/jwiegley/gptel-prompts.git"
            :rev :newest)
  :after gptel
  :demand t

  :config

  (when (file-directory-p gptel-prompts-directory)
    (gptel-prompts-update)
    ;; Ensure prompts are updated if prompt files change
    (gptel-prompts-add-update-watchers)))

;; _____________________________________________________________________________
;;; ORG BLOCKS
;; <https://github.com/jwiegley/ob-gptel>

(use-package ob-gptel
  :vc (:url "https://github.com/jwiegley/ob-gptel.git"
            :rev :newest)
  :after org

  :config

  (add-to-list 'org-babel-load-languages '(gptel . t))

  (defun eon-ob-gptel-setup-completions ()
    (add-hook 'completion-at-point-functions 'ob-gptel-capf nil t))

  :hook

  (org-mode . eon-ob-gptel-setup-completions))

;; _____________________________________________________________________________
(provide 'eon-gptel)
;;; eon-gptel.el ends here
