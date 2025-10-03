;;; eon-flycheck.el --- Flycheck -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; Alternative for the built-in Flymake
;;
;;; Code:

;;  ____________________________________________________________________________
;;; SYNTAX CHECKER / LINTER
;; <https://www.flycheck.org/en/latest/>

(use-package flycheck :ensure t
  :preface
  (defun eon-flycheck--enabling-p (cur arg)
    "Return non-nil if the mode should enable given CUR and ARG.
CUR is the current mode state (non-nil means enabled).
ARG is the raw prefix arg: nil toggles, >0 enables, <=0 disables.")
  (defun eon-flycheck--prefer (orig &optional arg)
    "Prefer Flycheck on auto-enable, then call ORIG with ARG.
ORIG is the advised `flycheck-mode'. ARG is the prefix arg.
If enabling non-interactively while Flymake is on, disable Flymake."
    (when (and (not (called-interactively-p 'interactive))
               (eon-flycheck--enabling-p
                (bound-and-true-p flycheck-mode) arg)
               (bound-and-true-p flymake-mode))
      (flymake-mode -1))
    (funcall orig arg))
  (defun eon-flymake--yield (orig &optional arg)
    "Make Flymake yield to Flycheck on auto-enable.
ORIG is the advised `flymake-mode'. ARG is the prefix arg.
If enabling non-interactively while Flycheck is on, skip enabling."
    (if (and (not (called-interactively-p 'interactive))
             (eon-flycheck--enabling-p
              (bound-and-true-p flymake-mode) arg)
             (bound-and-true-p flycheck-mode))
        nil
      (funcall orig arg)))
  :init
  (advice-add 'flycheck-mode :around #'eon-flycheck--prefer)
  (with-eval-after-load 'flymake
    (advice-add 'flymake-mode :around #'eon-flymake--yield))
  :hook
  ((emacs-lisp-mode . flycheck-mode)
   (lisp-interaction-mode . (lambda () (flycheck-mode -1)))))

;; Use Eglot with Flycheck instead of Flymake
;; <https://github.com/flycheck/flycheck-eglot>
(use-package flycheck-eglot :ensure t
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

;;  ____________________________________________________________________________
(provide 'eon-flycheck)
;;; eon-flycheck.el ends here
