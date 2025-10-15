;;; eon-gptel.el --- LLM chat client -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GPTEL
;; <https://github.com/karthink/gptel>

(use-package gptel :ensure t
  :init
  ;; Gptel requires built-in package `transient' version 0.7.4 or later
  (unless (package-installed-p 'transient '(0 7 4))
    (package-upgrade 'transient))
  :custom
  (gptel-default-mode 'org-mode))

;; Setup for local LLMs via Ollama - see also `eon-ollama'
(use-package gptel-ollama :ensure nil
  :when (eon-modulep 'eon-ollama)
  :init
  (require 'eon-ollama)
  :custom
  ;; Register local Ollama models
  (gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :endpoint "/api/chat"
                   :stream t
                   :models (eon-ollama-models 'symbol)))
  :config
  (defun eon-gptel--set-ollama-default-model ()
    "Set the default LLM"
    (interactive)
    (setq-local gptel-model eon-ollama-default-model))
  :hook
  (gptel-mode . eon-gptel--set-ollama-default-model))

;; _____________________________________________________________________________
(provide 'eon-gptel)
;;; eon-gptel.el ends here
