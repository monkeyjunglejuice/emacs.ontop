;;; eon-smartparens.el --- Structural editing -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; Emacs ONTOP is an extension on top of the Emacs ONBOARD starter-kit

;;; Code:

;;  ____________________________________________________________________________
;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

(use-package smartparens
  :init
  ;; Turn off other modes that clash with Smartparens
  (electric-pair-mode -1)
  (show-paren-mode -1)
  ;; Globally enable non-strict delimiter handling?
  ;; Specific configurations can be found within the resp. language module files.
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  ;; :custom
  ;; Smartparens comes without keybindings defined, it's totally up to you if you
  ;; go with a pre-defined keybinding set or your set (see `:bind' down below).
  ;; Load one of the default keybinding sets:
  ;; (sp-base-key-bindings 'sp)
  ;; (sp-base-key-bindings 'paredit)
  :config
  ;; Enable language-specific configurations
  (require 'smartparens-config)
  :hook
  ((emacs-lisp-mode lisp-interaction-mode) . smartparens-strict-mode)
  ((eshell-mode eval-expression-minibuffer-setup) . smartparens-mode)
  :bind
  ;; Custom keybinding set, a blend of standard Emacs sexp keybindings
  ;; and Paredit keybindings
  (:map smartparens-mode-map
        ;; Navigation
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-p" . sp-backward-down-sexp)
        ("C-M-n" . sp-up-sexp)
        ("C-M-a" . sp-beginning-of-sexp)
        ("C-M-e" . sp-end-of-sexp)
        ;; Depth-changing commands
        ("C-M-g" . sp-unwrap-sexp)
        ("C-M-s" . sp-splice-sexp)
        ;; Forward slurp/barf
        ("C-)" . sp-forward-slurp-sexp)
        ("C-}" . sp-forward-barf-sexp)
        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-<left>" . sp-forward-barf-sexp)
        ;; Backward slurp/barf
        ("C-(" . sp-backward-slurp-sexp)
        ("C-{" . sp-backward-barf-sexp)
        ("C-M-<left>" . sp-backward-slurp-sexp)
        ("C-M-<right>" . sp-backward-barf-sexp)
        ;; Misc
        ("C-M-k" . sp-kill-sexp)
        ("C-M-<backspace>" . sp-backward-kill-sexp)
        ("C-M-SPC" . sp-mark-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-t" . sp-transpose-sexp)
        ("M-(" . sp-wrap-round)))

;;  ____________________________________________________________________________
(provide 'eon-smartparens)
;;; eon-smartparens.el ends here
