;;; eon-helpful.el --- Extended help viewer -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; HELPFUL
;;  <https://github.com/Wilfred/helpful>

;; 'Helpful' is an alternative to the built-in Emacs help that provides
;; much more contextual information

(use-package helpful :ensure t
  :defer t

  :bind

  ;; Lookup the current symbol at point
  ("C-h o" . #'helpful-at-point)

  ;; Look up functions and macros
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  ("C-h f" . #'helpful-callable)

  ;; Look up functions, exclude macros
  ;; By default, C-h F is bound to `info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  ("C-h F" . #'helpful-function)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)

  (:map ctl-z-h-map
        ("c" . #'helpful-command)
        ("f" . #'helpful-callable)
        ("F" . #'helpful-function)
        ("o" . #'helpful-at-point)
        ("k" . #'helpful-key)
        ("v" . #'helpful-variable))

  (:map eon-localleader-elisp-map
        ("h" . #'helpful-symbol)))

;; _____________________________________________________________________________
(provide 'eon-helpful)
;;; eon-helpful.el ends here
