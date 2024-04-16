;;; ontop-vertico.el --- Vertico settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;; <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package nil))

(eval-when-compile
  (require 'use-package))

;;  ____________________________________________________________________________
;;; VERTICO
;; <https://github.com/minad/vertico>
;; <https://github.com/minad/vertico#key-bindings>

(use-package vertico
  :ensure t
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
  (vertico-cycle t)
  :config
  (defun +embark-live-vertico ()
    "Shrink Vertico minibuffer when `embark-live' is active."
    (when-let (win (and (string-prefix-p "*Embark Live" (buffer-name))
                        (active-minibuffer-window)))
      (with-selected-window win
        (when (and (bound-and-true-p vertico--input)
                   (fboundp 'vertico-multiform-unobtrusive))
          (vertico-multiform-unobtrusive)))))
  (add-hook 'embark-collect-mode-hook #'+embark-live-vertico))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; EMACS (built-in)

;; Persist history over Emacs restarts. Vertico sorts by history position
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; A few more useful configurations ...
(use-package emacs
  :ensure nil
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
        '( read-only t
           cursor-intangible t
           face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;  ____________________________________________________________________________
;;; MARGINALIA
;; <https://github.com/minad/marginalia>

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :init
  ;; The :init configuration is always executed (Not lazy!)
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode 1)
  :bind
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

;;; ___________________________________________________________________________
(provide 'ontop-vertico)
;;; ontop-vertico.el ends here
