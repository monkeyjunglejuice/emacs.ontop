;;; eon-gptel.el --- Comprehensive AI chat client -*- lexical-binding: t; no-byte-compile: t; -*-
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

  (gptel-default-mode 'org-mode)

  :bind

  (:map ctl-z-l-map
        ("l" . gptel)
        ("m" . gptel-menu)
        ("a" . gptel-add)
        ("k" . gptel-abort)
        ("f" . gptel-add-file)
        ("h" . gptel-highlight-mode)
        ("o" . gptel-org-set-topic)
        ("O" . gptel-org-set-properties)
        ("p" . gptel-system-prompt)
        ("r" . gptel-rewrite)
        ("s" . gptel-send)
        ("t" . gptel-tools)))

;; Setup for local and cloud LLMs via Ollama - see also `./eon-ai'
(when (executable-find "ollama")
  (use-package gptel-ollama :ensure nil
    :after gptel

    :init

    (require 'eon-ai)
    :custom
    ;; Register local Ollama models
    (gptel-backend (gptel-make-ollama "Ollama"
                     :host "localhost:11434"
                     :endpoint "/api/chat"
                     :stream t
                     :models (eon-ollama-models 'symbol)))

    :config

    (defun eon-gptel--maybe-set-ollama-model ()
      "Set the default LLM."
      (interactive)
      (cond
       (eon-ollama-default-model
        (setopt gptel-model eon-ollama-default-model))
       (t gptel-model)))
    (eon-gptel--maybe-set-ollama-model)))

;; _____________________________________________________________________________
(provide 'eon-gptel)
;;; eon-gptel.el ends here
