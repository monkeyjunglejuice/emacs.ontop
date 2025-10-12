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
  (setopt evil-want-integration t)
  (setopt evil-want-keybinding nil)
  ;; Leader and local leader in Evil; doesn't need Evil's built-in leader
  (defun eon-evil--setup-leaders ()
    "Bind \",\" as leader and \", ,\" as localleader in Evil states."
    (let ((states '(normal visual motion)))
      ;; In Evil's modal states, "," invokes your leader map
      (mapc (lambda (evil-state)
              (evil-define-key evil-state 'global (kbd ",") ctl-z-map))
            states))
    ;; Add a labeled comma prefix *at runtime* so which-key shows ', +Local'
    (when (keymapp ctl-z-localleader-map)
      (define-key ctl-z-map (kbd ",")
        (cons "Local" ctl-z-localleader-map))))
  :config
  (evil-mode 1)
  ;; Leader key / local leader key
  (eon-evil--setup-leaders)
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
  ;; TODO Needs a context-dependent documentation lookup;
  ;; - helpful-at-point for Elisp (check; for now)
  ;; - sly-hyperspec-lookup for Common Lisp / Sly, opens page in EWW
  ;; - eglot doc buffer (eldoc) for LSP-enabled modes
  ;; - etc.
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
