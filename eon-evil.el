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

  (setopt evil-want-integration t
          evil-want-keybinding nil)

  ;; We're not using Evil's leader/localleader implementation. Instead we're
  ;; wiring the agnostic implementation from Emacs ONBOARD `eon.el' that works
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

  ;; Explicitly bind the leader key
  (eon-evil--bind-leader-in-states nil eon-evil-leader-key)

  ;; Escape from Evil Emacs state
  (evil-define-key 'emacs 'global [escape] #'evil-normal-state)

  ;; Fast window switching
  (evil-define-key 'normal 'global (kbd "SPC") #'evil-window-mru)

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
  ;; - sly-hyperspec-lookup for Common Lisp / Sly that opens page in EWW
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

  ;; Prevent the current leader from getting shadowed by evil-collection
  ;; (e.g. as it does with "SPC" in `customize-mode').

  (defun eon-evil--unshadow-leader-in-maps (maps)
    (let ((lk eon-evil-leader-key))
      (when (and (stringp lk) (> (length lk) 0))
        (let ((key (kbd lk)))
          (dolist (ms maps)
            (let ((km (cond ((keymapp ms) ms)
                            ((and (symbolp ms) (boundp ms))
                             (symbol-value ms))
                            (t nil))))
              (when (keymapp km)
                (dolist (st '(normal visual motion))
                  (let ((aux (evil-get-auxiliary-keymap km st nil)))
                    (when aux
                      (define-key aux key nil)))))))))))

  (defun eon-evil--unshadow-leader-in-ec-feature (feat)
    (let* ((name (symbol-name feat))
           (pref "evil-collection-")
           (mod  (and (string-prefix-p pref name)
                      (substring name (length pref))))
           (maps (and mod
                      (intern (format "evil-collection-%s-maps" mod)))))
      (when (and maps (boundp maps))
        (eon-evil--unshadow-leader-in-maps (symbol-value maps)))))

  (defun eon-evil--unshadow-leader-ec-sweep ()
    (mapc #'eon-evil--unshadow-leader-in-ec-feature features))

  (defun eon-evil--unshadow-leader-ec-after-load (file)
    (let ((base (file-name-base file)))
      (when (string-prefix-p "evil-collection-" base)
        (eon-evil--unshadow-leader-in-ec-feature (intern base)))))

  ;; Run now, for future loads, and on leader changes
  (eon-evil--unshadow-leader-ec-sweep)
  (add-hook 'after-load-functions
            #'eon-evil--unshadow-leader-ec-after-load)

  ;; Re-unshadow when the leader changes via setopt/Customize
  (when (fboundp 'eon-evil--set-leaders)
    (advice-add 'eon-evil--set-leaders :after
                (lambda (sym _val)
                  (when (eq sym 'eon-evil-leader-key)
                    (eon-evil--unshadow-leader-ec-sweep)))))

  ;; Add Ranger-like movements to Dired
  (evil-collection-define-key 'normal 'dired-mode-map
    "l" #'dired-find-file
    "h" #'dired-up-directory
    "y" #'dired-ranger-copy
    "p" #'dired-ranger-paste
    "P" #'dired-ranger-move))

;; _____________________________________________________________________________
;;; WHICH-KEY

(use-package which-key :ensure nil
  :after evil
  :custom
  (which-key-allow-evil-operators t))

;; _____________________________________________________________________________
(provide 'eon-evil)
;;; eon-evil.el ends here
