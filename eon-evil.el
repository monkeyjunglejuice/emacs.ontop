;;; eon-evil.el --- Vim keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; EVIL
;; <https://evil.readthedocs.io/en/latest/settings.html>
;; <https://github.com/noctuid/evil-guide>

(use-package evil :ensure t

  :init

  (setopt evil-want-integration t
          evil-want-keybinding nil)

  ;; We're not using Evil's leader/localleader implementation. Instead we're
  ;; wiring in the agnostic implementation from Emacs ONBOARD that works
  ;; independently from Evil.

  (defun eon-evil--bind-leader-in-states (old new)
    "Explicitly bind the leader key to prevent hijacking."
    (when (and (stringp new) (> (length new) 0))
      (dolist (m (list evil-normal-state-map
                       evil-visual-state-map
                       evil-motion-state-map))
        (when (and (stringp old) (> (length old) 0))
          (define-key m (kbd old) nil))
        (define-key m (kbd new) ctl-z-map))))

  (defun eon-evil--set-leaders (sym val)
    "Setter for leader and local leader keys.
Used by custom variables `eon-evil-leader-key' and `eon-evil-localleader-key'."
    (let ((old (and (boundp sym) (default-value sym))))
      (set-default sym val)
      (with-eval-after-load 'evil
        (pcase sym
          ('eon-evil-leader-key
           (eon-evil--bind-leader-in-states old val))
          ('eon-evil-localleader-key
           (when old (define-key ctl-z-map (kbd old) nil))
           (define-key ctl-z-map (kbd val)
                       (cons "Local" ctl-z-localleader-map)))))))

  (defcustom eon-evil-leader-key ","
    "Leader key for Evil."
    :group 'eon :type 'string
    :set #'eon-evil--set-leaders
    :initialize 'custom-initialize-set)

  (defcustom eon-evil-localleader-key ","
    "Local leader key for Evil."
    :group 'eon :type 'string
    :set #'eon-evil--set-leaders
    :initialize 'custom-initialize-set)

  :config

  ;; Activate Evil first
  (evil-mode 1)

  ;; Explicitly bind the leader key afterwards so it won't get hijacked
  (eon-evil--bind-leader-in-states nil eon-evil-leader-key)

  ;; Escape from Evil Emacs state
  (evil-define-key 'emacs 'global [escape] #'evil-normal-state)

  ;; Fast window switching
  (evil-define-key 'normal 'global (kbd "RET") #'evil-window-mru)

  ;; Comment/uncomment by pressing "gcc" in normal mode and "gc" in visual mode
  (evil-define-operator eon-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global
    (kbd "gc") #'eon-evil-comment-or-uncomment))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(use-package evil-vars :ensure nil
  :after evil
  :custom
  (evil-echo-state nil)
  (evil-move-cursor-back nil)
  (evil-undo-system 'undo-fu)
  ;; TODO `evil-lookup-func' should be a context-dependent documentation lookup;
  ;; - helpful-at-point for Elisp (implemented for now)
  ;; - sly-hyperspec-lookup for Common Lisp / Sly, opens page in EWW
  ;; - eglot doc buffer (eldoc) for LSP-enabled modes
  ;; - etc. -- there's probably a package for that already existing.
  (evil-lookup-func #'helpful-at-point))

;; _____________________________________________________________________________
;; <https://github.com/emacs-evil/evil-collection>

(use-package evil-collection :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :init
  (setopt evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init)
  ;; Adopt ranger-like movements in Dired
  (evil-collection-define-key 'normal 'dired-mode-map
    "l" #'dired-find-file
    "h" #'dired-up-directory
    "y" #'dired-ranger-copy
    "p" #'dired-ranger-paste
    "P" #'dired-ranger-move))

;; _____________________________________________________________________________

(use-package which-key :ensure nil
  :after evil
  :custom
  (which-key-allow-evil-operators t))

;; _____________________________________________________________________________
(provide 'eon-evil)
;;; eon-evil.el ends here
