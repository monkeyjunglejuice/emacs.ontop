;;; eon-ai.el --- Shared functionality for AI integration -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GLOBAL DEFINITIONS

(defgroup eon-ai nil
  "AI integration."
  :group 'eon)

(defvar-keymap ctl-z-l-map :doc "Large Language Models / AI")
(keymap-set ctl-z-map "l" `("LLM/AI" . ,ctl-z-l-map))

;; _____________________________________________________________________________
(provide 'eon-ai)
;;; eon-ai.el ends here
