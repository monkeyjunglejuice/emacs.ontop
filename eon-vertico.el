;;; eon-vertico.el --- Vertical completion  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-vertico.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; VERTICO
;; <https://github.com/minad/vertico>
;; <https://github.com/minad/vertico#key-bindings>

(use-package vertico :ensure t
  :after orderless
  :init
  ;; Disable ONBOARD completion
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)
  ;; Enable Vertico
  (vertico-mode)
  ;; How to display Vertico per default?
  (vertico-multiform-mode)
  ;; (vertico-buffer-mode)
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
  (vertico-cycle nil))

;; Use `consult-completion-in-region' if a vertical completion is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if (or vertico-mode fido-vertical-mode)
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

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
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t
                    cursor-intangible t
                    face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

;;;  ____________________________________________________________________________
;;; MARGINALIA
;; <https://github.com/minad/marginalia>

;; Enable rich annotations using the Marginalia package
(use-package marginalia :ensure t
  :init
  ;; The :init configuration is always executed (Not lazy!)
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
  :bind
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

;;  ____________________________________________________________________________
(provide 'eon-vertico)
;;; eon-vertico.el ends here
