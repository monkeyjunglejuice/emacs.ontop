;;; eon-god.el --- Modal editing: Emacs keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; What god-mode does:
;;
;; Before: C-p C-k C-n M-^ ) C-j C-y M-r C-x z z M-2 M-f C-x C-s
;; After:    p   k   n g ^ )   j   y g r     . .   2 g f   x   s
;;
;;; Code:

;; _____________________________________________________________________________
;;; GOD MODE
;; <https://github.com/emacsorphanage/god-mode>

(use-package god-mode :ensure t

  :init

  (defvar eon-god--emulation-map (make-sparse-keymap)
    "Emulation map used to protect the God leader key.")

  (defun eon-god--install-emulation ()
    "Install `eon-god--emulation-map' for `god-local-mode'."
    (add-to-list 'emulation-mode-map-alists
                 `((god-local-mode . ,eon-god--emulation-map))))

  (defun eon-god--bind-leader (old new)
    "Bind NEW as God leader in the emulation map and unset OLD."
    (when (and (stringp old) (> (length old) 0))
      (keymap-unset eon-god--emulation-map old t))
    (when (and (stringp new) (> (length new) 0))
      ;; Bind to the stable frontend, not the resolved map
      (keymap-set eon-god--emulation-map new eon-leader-map)))

  (defun eon-god--set-leaders (sym val)
    (let ((old (and (boundp sym) (default-value sym))))
      (set-default sym val)
      (with-eval-after-load 'god-mode
        (pcase sym
          ('eon-god-leader-key
           (eon-god--bind-leader old val))
          ('eon-god-localleader-key
           (when old (keymap-unset eon-leader--map old t))
           (keymap-set eon-leader--map val
                       (cons "Local" eon-localleader-map)))))))

  (defcustom eon-god-leader-key ","
    "Leader key for God mode."
    :group 'eon-leader
    :type 'string
    :set #'eon-god--set-leaders
    :initialize 'custom-initialize-set)

  (defcustom eon-god-localleader-key ","
    "Local leader key for God mode, pressed after the leader key."
    :group 'eon-leader
    :type 'string
    :set #'eon-god--set-leaders
    :initialize 'custom-initialize-set)

  ;; Enable God mode almost everywhere;
  ;; see `god-exempt-major-modes' and `god-exempt-preticates' where not.
  (god-mode-all 1)

  :custom

  (god-mode-lighter-string "G")
  (god-mode-enable-function-key-translation nil)

  :config

  (defun eon-god-local-mode-activate ()
    "Turn God mode on.
Bound to \"<escape>\" per default."
    (interactive)
    (god-local-mode 1))

  (defun eon-god-local-mode-disable ()
    "Turn God mode off.
Bound to \"i\" per default."
    (interactive)
    (god-local-mode -1))

  ;; Bind the leader key
  (eon-god--install-emulation)
  (eon-god--bind-leader nil eon-god-leader-key)

  ;; Show special cursor while `god-local-mode' is active in a buffer
  (defun eon-god--cursor-compute ()
    "Return a cursor type when God-mode is active, else nil."
    (cond
     ;; Selection while god-local-mode is active
     ((and (bound-and-true-p god-local-mode)
           (region-active-p))
      eon-cursor-type-extra-select)
     ;; Normal god-local-mode state; no selection
     ((bound-and-true-p god-local-mode)
      eon-cursor-type-extra)
     (t nil)))
  (add-hook 'eon-cursor-functions #'eon-god--cursor-compute)

  ;; Refresh cursor when god-mode toggles
  (add-hook 'god-local-mode-hook #'eon-cursor-update)

  ;; Unblock a major mode
  (defun eon-god-unexempt-mode (&rest modes)
    "Remove certain MODES from `god-exempt-major-modes'.
Once removed, they will start with `god-local-mode' enabled."
    (setopt god-exempt-major-modes
            (seq-difference god-exempt-major-modes modes #'eq)))

  ;; Unblock by predicate
  (defun eon-god-unexempt-predicate (&rest predicates)
    "Remove certain PREDICATES from `god-exempt-predicates'.
Once removed, they will start with `god-local-mode' enabled."
    (setopt god-exempt-predicates
            (seq-difference god-exempt-predicates predicates #'eq)))

  ;; Don't start `vterm' with God-mode enabled
  (eon-add-to-list* 'god-exempt-major-modes 'vterm-mode)

  ;; Intercept the ESC key and let Emacs handle it when in `vterm' buffer;
  ;; toggle via "C-c C-q" between interception and passing through to `vterm'.
  (with-eval-after-load 'vterm
    (add-hook 'vterm-mode-hook
              (lambda ()
                (setq eon-vterm-escape-command #'eon-god-local-mode-activate
                      eon-vterm-send-escape-to-vterm nil)
                (eon-vterm-update-escape))))

  :bind

  ("<escape>" . eon-god-local-mode-activate)
  (:map god-local-mode-map
        ("i" . eon-god-local-mode-disable)
        ("." . repeat)
        ("V" . scroll-down-command)
        ("q" . quit-window)
        ))

;; Adjustments for Isearch
(use-package god-mode-isearch :ensure nil
  :bind
  (:map isearch-mode-map
        ("<escape>" . god-mode-isearch-activate))
  (:map god-mode-isearch-map
        ("<escape>" . god-mode-isearch-disable)))

;; _____________________________________________________________________________
;;; WHICH-KEY

(use-package which-key :ensure nil
  :config
  (which-key-enable-god-mode-support))

;; _____________________________________________________________________________
(provide 'eon-god)
;;; eon-god.el ends here
