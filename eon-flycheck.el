;;; eon-flycheck.el --- Flycheck configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-flycheck.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; SYNTAX CHECKER / LINTER
;; <https://www.flycheck.org/en/latest/>

;; Alternative for the built-in Flymake
(use-package flycheck
  :init
  ;; Remove Flymake from that hook, as we're going to use Flycheck instead
  (remove-hook 'emacs-lisp-mode-hook #'flymake-mode)
  :hook
  (emacs-lisp-mode . flycheck-mode)
  (lisp-interaction-mode . (lambda () (flycheck-mode -1))))

;;  ____________________________________________________________________________
(provide 'eon-flycheck)
;;; eon-flycheck.el ends here
