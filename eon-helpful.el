;;; eon-helpful.el --- Helpful -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
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
        ("o" . #'helpful-at-point)
        ("f" . #'helpful-callable)
        ("F" . #'helpful-function)
        ("v" . #'helpful-variable)
        ("K" . #'helpful-key)
        ("c" . #'helpful-command))
  (:map eon-localleader-elisp-map
        ("h" . #'helpful-at-point)
        ("H" . #'helpful-symbol)))

;;  ____________________________________________________________________________
(provide 'eon-helpful)
;;; eon-helpful.el ends here
