;;; eon-vertico.el --- Vertical minibuffer completion UI -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; VERTICO
;; <https://github.com/minad/vertico>
;; <https://github.com/minad/vertico#key-bindings>

(use-package vertico :ensure t
  :init

  ;; Disable Emacs ONBOARD default completion UIs
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)

  ;; Enable Vertico
  (vertico-mode)
  ;; How to display Vertico per default?
  (vertico-multiform-mode)

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
  (vertico-multiform-mode)

  ;; :hook

  ;; Enable repeatable Vertico sessions
  ;; (minibuffer-setup . vertico-repeat-save)

  ;; :bind

  ;; (:map vertico-map
  ;;       ("M-P" . vertico-repeat-previous)
  ;;       ("M-N" . vertico-repeat-next))
  )

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; EMACS (built-in)

;; A few more useful configurations
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

;; _____________________________________________________________________________
(provide 'eon-vertico)
;;; eon-vertico.el ends here
