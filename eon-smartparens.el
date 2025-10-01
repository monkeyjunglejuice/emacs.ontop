;;; eon-smartparens.el --- Smartparens -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; Emacs ONTOP is an extension on top of the Emacs ONBOARD starter-kit

;;; Code:

;;  ____________________________________________________________________________
;;; STRUCTURAL EDITING

;; SMARTPARENS
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>

(use-package smartparens :ensure t
  :init
  ;; Turn off other modes that clash with Smartparens
  (electric-pair-mode -1)
  (show-paren-mode -1)
  ;; :custom
  ;; Smartparens comes without keybindings defined, it's totally up to you
  ;; if you go with a pre-defined keybinding set or your personal set.
  ;; Before you load one of the default sets, comment out the :bind form below.
  ;; (sp-base-key-bindings 'sp)  ; default smartparens bindings
  ;; (sp-base-key-bindings 'paredit)  ; default paredit bindings
  :config
  ;; Enable language-specific configurations
  (require 'smartparens-config)
  ;; Only use the pseudo-quote inside strings where it serves as hyperlink
  (sp-with-modes 'emacs-lisp-mode
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))
  ;; Minibuffer
  (defun eon-smartparens-enable-minibuffer ()
    "Enable `smartparens-mode' in the minibuffer during `eval-expression'."
    (sp-local-pair 'minibuffer-pairs "'" nil :actions nil)
    (sp-local-pair 'minibuffer-pairs "`" nil :actions nil)
    (sp-update-local-pairs 'minibuffer-pairs)
    (smartparens-mode 1))
  ;; Globally enable non-strict delimiter handling?
  ;; Specific configurations can be found within the resp. language module files.
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :hook
  ((emacs-lisp-mode lisp-interaction-mode) . smartparens-strict-mode)
  (eshell-mode . smartparens-mode)
  (eval-expression-minibuffer-setup . eon-smartparens-enable-minibuffer)
  :bind
  ;; Custom keybinding set, a blend of standard Emacs sexp keybindings
  ;; and Paredit keybindings
  (:map smartparens-mode-map
        ;; Navigation
        ("C-M-f"           . sp-forward-sexp)
        ("C-M-b"           . sp-backward-sexp)
        ("C-M-u"           . sp-backward-up-sexp)
        ("C-M-d"           . sp-down-sexp)
        ("C-M-p"           . sp-backward-down-sexp)
        ("C-M-n"           . sp-up-sexp)
        ("C-M-a"           . sp-beginning-of-sexp)
        ("C-M-e"           . sp-end-of-sexp)
        ;; Depth-changing commands
        ("C-M-g"           . sp-unwrap-sexp)
        ("C-M-s"           . sp-splice-sexp)
        ;; Forward slurp/barf
        ("C-)"             . sp-forward-slurp-sexp)
        ("C-}"             . sp-forward-barf-sexp)
        ("C-<right>"       . sp-forward-slurp-sexp)
        ("C-<left>"        . sp-forward-barf-sexp)
        ;; Backward slurp/barf
        ("C-("             . sp-backward-slurp-sexp)
        ("C-{"             . sp-backward-barf-sexp)
        ("C-M-<left>"      . sp-backward-slurp-sexp)
        ("C-M-<right>"     . sp-backward-barf-sexp)
        ;; Misc
        ("C-M-k"           . sp-kill-sexp)
        ("C-M-<backspace>" . sp-backward-kill-sexp)
        ("C-M-SPC"         . sp-mark-sexp)
        ("C-M-w"           . sp-copy-sexp)
        ("C-M-t"           . sp-transpose-sexp)
        ("M-("             . sp-wrap-round)))

;;  ____________________________________________________________________________
(provide 'eon-smartparens)
;;; eon-smartparens.el ends here
