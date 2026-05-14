;;; eon-rainbow-delimiters.el --- Color-code nested parenthesis -*- lexical-binding: t; no-byte-compile: t; -*-

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
;; <https://github.com/Fanael/rainbow-delimiters>
;;
;;; Code:

(eon-module-metadata
 :conflicts '(eon-paren-face)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; PAREN FACE MODE

(use-package rainbow-delimiters :ensure t

  :config

  (defun eon-rainbow-delimiters-enable-lisp-src-modes ()
    "Enable `rainbow-delimiters-mode' in all known Lisp source code buffers."
    (mapc (lambda (mode)
            (add-hook mode #'rainbow-delimiters-mode))
          ;; Return all modes, as some may not be available at eval time
          (eon-lisp-src-modes 'hook)))

  (defun eon-rainbow-delimiters-enable-lisp-repl-modes ()
    "Enable `rainbow-delimiters-mode' in all known Lisp REPL buffers."
    (mapc (lambda (mode)
            (add-hook mode #'rainbow-delimiters-mode))
          ;; Return all modes, as some may not be available at eval time
          (eon-lisp-repl-modes 'hook)))

  :hook

  ;; Useful for non-Lisp languages, so enable for parent modes too
  ((prog-mode conf-mode comint-mode) . rainbow-delimiters-mode)

  ;; Add to all known lisp modes after the entire config has been loaded
  ;; to improve the change mode-specific ...-mode-hook variables actually exist.
  (emacs-startup . eon-rainbow-delimiters-enable-lisp-src-modes)
  (emacs-startup . eon-rainbow-delimiters-enable-lisp-repl-modes))

;; _____________________________________________________________________________
(provide 'eon-rainbow-delimiters)
;;; eon-rainbow-delimiters.el ends here
