;;; eon-vertico.el --- Vertico -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; VERTICO
;; <https://github.com/minad/vertico>
;; <https://github.com/minad/vertico#key-bindings>

(use-package vertico :ensure t
  :after orderless
  :init
  ;; Disable ONBOARD default completion
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)
  :custom
  ;; Display certain listings in another form?
  (vertico-multiform-commands '((consult-imenu buffer)
                                (consult-outline buffer)
                                (consult-grep buffer)
                                (consult-ripgrep buffer)))
  ;; In the minibuffer
  (vertico-count 12)
  (vertico-resize 'grow-only)
  ;; Enable cycling for `vertico-next' and `vertico-previous'?
  (vertico-cycle nil)
  :config
  ;; Enable Vertico
  (vertico-mode)
  ;; How to display Vertico per default?
  (vertico-multiform-mode))

;; Use `consult-completion-in-region' if a vertical completion is enabled.
;; Otherwise use the default `completion--in-region' function.
(when (eon-modulep 'consult)
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (or vertico-mode fido-vertical-mode)
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; EMACS (built-in)

;; A few more useful configurations ...
(use-package emacs :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;;  ____________________________________________________________________________
(provide 'eon-vertico)
;;; eon-vertico.el ends here
