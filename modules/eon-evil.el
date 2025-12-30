;;; eon-evil.el --- Modal editing: Vim keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; EVIL
;; <https://evil.readthedocs.io/en/latest/settings.html>
;; <https://github.com/noctuid/evil-guide>

;; Evil states cheatsheet:
;; normal
;; visual
;; insert
;; emacs
;; operator
;; motion
;; replace
;; global (binds the key without evil current-global-map)

(use-package evil :ensure t

  :init

  ;; Let Evil handle the cursor styles
  (eon-cursor-mode -1)

  ;; Required settings for Evil Collection to handle keybindings
  (setopt evil-want-integration t
          evil-want-keybinding nil)

  ;; We're not using Evil's leader/localleader implementation. Instead we
  ;; wire the agnostic implementation from Emacs ONBOARD './eon.el' that works
  ;; independently from Evil.

  ;; We define a separate leader key for Evil here, so that the default leader
  ;; key fulfills the role of the alternative leader key. The alternative leader
  ;; leader will be accessible from Evil states other than <normal> or <visual>,
  ;; e.g. <insert> or <emacs>.
  ;;
  ;; You may want to customize the variables:
  ;; `eon-evil-leader-key' - default "SPC"
  ;; `eon-evil-localleader-key' - default ","
  ;; `eon-leader-key' - default for Evil "C-SPC"
  ;; `eon-localleader-key' - default "C-,"

  (defun eon-evil--bind-leader-in-states (old new)
    "Explicitly bind the leader key to prevent hijacking."
    (when (and (stringp new) (> (length new) 0))
      (dolist (m (list evil-normal-state-map
                       evil-visual-state-map
                       evil-motion-state-map))
        (when (and (stringp old) (> (length old) 0))
          (define-key m (kbd old) nil))
        (define-key m (kbd new) eon-leader-map))))

  (defun eon-evil--set-leaders (sym val)
    "Setter for leader and local leader keys.
Used by the custom variables `eon-evil-leader-key'
and `eon-evil-localleader-key'."
    (let ((old (and (boundp sym) (default-value sym))))
      (set-default sym val)
      (with-eval-after-load 'evil
        (pcase sym
          ('eon-evil-leader-key
           (eon-evil--bind-leader-in-states old val))
          ('eon-evil-localleader-key
           (when old (define-key eon-leader-map (kbd old) nil))
           (define-key eon-leader-map (kbd val)
                       (cons "Local" eon-localleader-map)))))))

  (defcustom eon-evil-leader-key "SPC"
    "Leader key for Evil."
    :group 'eon-leader
    :type 'string
    :set #'eon-evil--set-leaders
    :initialize 'custom-initialize-set)

  (defcustom eon-evil-localleader-key ","
    "Local leader key for Evil."
    :group 'eon-leader
    :type 'string
    :set #'eon-evil--set-leaders
    :initialize 'custom-initialize-set)

  :config

  ;; Enable Evil first
  (evil-mode 1)

  (setq evil-normal-state-cursor eon-cursor-type-extra
        evil-visual-state-cursor eon-cursor-type-extra-select
        evil-insert-state-cursor eon-cursor-type-write
        evil-emacs-state-cursor  eon-cursor-type-write)

  ;; Explicitly bind the Evil leader key, defaults to "SPC". Customize
  ;; `eon-evil-leader-key' and/or `eon-evil-localleader-key' to change.
  (eon-evil--bind-leader-in-states nil eon-evil-leader-key)
  ;; Set "M-SPC" as the default alternative leader key for Evil;
  ;; customize `eon-leader-key' to change the binding.
  (setopt eon-leader-key "M-SPC")

  ;; Escape from Evil Emacs state
  (evil-define-key 'emacs 'global [escape] #'evil-normal-state)

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
  (evil-default-cursor 'box)
  (evil-echo-state nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-ex-search-vim-style-regexp t)
  (evil-ex-visual-char-range t)
  (evil-kbd-macro-suppress-motion-error t)
  (evil-move-cursor-back nil)
  (evil-symbol-word-search t)
  (evil-undo-system (if (featurep 'undo-fu) 'undo-fu 'undo-redo))
  ;; TODO `evil-lookup-func' should be a context-dependent documentation lookup;
  ;; - helpful-at-point for Elisp (implemented for now)
  ;; - sly-hyperspec-lookup for Common Lisp / Sly that opens page in EWW
  ;; - eglot doc buffer (eldoc) for LSP-enabled modes
  ;; - etc. -- there's probably a package for that already existing
  (evil-lookup-func #'helpful-at-point))

;; _____________________________________________________________________________
;;; EVIL COLLECTION
;; Ongoing mission to evilize everything - a bottomless hole
;; <https://github.com/emacs-evil/evil-collection>

(use-package evil-collection :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode

  :init

  ;; Use Evil editing-behavior in the minibuffer too
  (setopt evil-collection-setup-minibuffer t)

  :custom

  ;; Prevent the leader key from getting shadowed by evil-collection
  (evil-collection-key-blacklist `(,eon-evil-leader-key
                                   ,eon-leader-key
                                   ,eon-localleader-key))

  :config

  (evil-collection-init)

  ;; Add VIM-like movements to Dired
  (evil-collection-define-key 'normal 'dired-mode-map
    "l" #'dired-find-file
    "h" #'dired-up-directory)

  ;; Add VIM-like copy/paste to Dired
  (when (eon-modulep 'eon-dired)
    (evil-collection-define-key 'normal 'dired-mode-map
      "y" #'dired-ranger-copy
      "p" #'dired-ranger-paste
      "P" #'dired-ranger-move)))

;; _____________________________________________________________________________
;;; WHICH-KEY

(use-package which-key :ensure nil
  :after evil
  :custom
  (which-key-allow-evil-operators t))

;; _____________________________________________________________________________
(provide 'eon-evil)
;;; eon-evil.el ends here
