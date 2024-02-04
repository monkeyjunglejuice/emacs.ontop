;;; ontop-helpful.el --- Helpful configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-helpful.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;;  <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package nil))

(eval-when-compile
  (require 'use-package))

;;  ____________________________________________________________________________
;;; HELPFUL
;;  <https://github.com/Wilfred/helpful>

;; 'Helpful' is an alternative to the built-in Emacs help that provides
;; much more contextual information

(use-package helpful
  :ensure t
  :bind
  ;; Lookup the current symbol at point
  ("C-h o" . #'helpful-at-point)
  ;; Look up functions and macros
  ;; Note that the built-in 'describe-function' includes both functions
  ;; and macros. 'helpful-function' is functions only, so we provide
  ;; 'helpful-callable' as a drop-in replacement.
  ("C-h f" . #'helpful-callable)
  ;; Look up functions, exclude macros
  ;; By default, C-h F is bound to 'Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  ("C-h F" . #'helpful-function)
  ("C-h v" . #'helpful-variable)
  ("C-h k" .  #'helpful-key)
  ;; Look up *C*ommands
  ;; By default, C-h C is bound to describe 'describe-coding-system'.
  ;; I don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  ("C-h C" . #'helpful-command))

;;  ____________________________________________________________________________
(provide 'ontop-helpful)
;;; ontop-helpful.el ends here
