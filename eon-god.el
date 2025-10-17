;;; eon-god.el --- God mode keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; GOD MODE
;; <https://github.com/emacsorphanage/god-mode>

(use-package god-mode :ensure t

  :init
  (defun eon-god-unexempt (&rest modes)
    (setopt god-exempt-major-modes
            (seq-difference god-exempt-major-modes modes 'eq)))

  (defun eon-god--bind-leader-in-states (old new)
    "Explicitly bind the leader key to prevent hijacking."
    (when (and (stringp new) (> (length new) 0))
      (dolist (m (list god-local-mode-map
                       ;; Add further God mode maps here
                       ))
        (when (and (stringp old) (> (length old) 0))
          (define-key m (kbd old) nil))
        (define-key m (kbd new) ctl-z-map))))

  (defun eon-god--set-leaders (sym val)
    "Setter for leader and local leader keys.
Used by custom variables `eon-god-leader-key' and `eon-god-localleader-key'."
    (let ((old (and (boundp sym) (default-value sym))))
      (set-default sym val)
      (with-eval-after-load 'god-mode
        (pcase sym
          ('eon-god-leader-key
           (eon-god--bind-leader-in-states old val))
          ('eon-god-localleader-key
           (when old (define-key ctl-z-map (kbd old) nil))
           (define-key ctl-z-map (kbd val)
                       (cons "Local" ctl-z-localleader-map)))))))

  (defcustom eon-god-leader-key ","
    "Leader key for God mode."
    :group 'eon :type 'string
    :set #'eon-god--set-leaders
    :initialize 'custom-initialize-set)

  (defcustom eon-god-localleader-key ","
    "Local leader key for God mode."
    :group 'eon :type 'string
    :set #'eon-god--set-leaders
    :initialize 'custom-initialize-set)

  :custom
  (god-mode-enable-function-key-translation nil)
  
  :config
  (god-mode)

  ;; Don't disable God mode in the following modes per default
  ;; (eon-god-unexempt 'dired-mode 'eww-mode 'ibuffer-mode 'magit-popup-mode
  ;;                   'org-agenda-mode 'wdired-mode)

  (defun eon-god-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

  ;; Explicitly bind the leader key
  (eon-god--bind-leader-in-states nil eon-god-leader-key)

  :hook
  (post-command . eon-god-update-cursor-type)

  :bind
  ("<escape>" . god-local-mode)
  (:map god-local-mode-map
        ("i" . god-local-mode)))

;; Adjustments for Isearch
(use-package god-mode-isearch :ensure nil
  :bind
  (:map isearch-mode-map
        ("<escape>" . god-mode-isearch-activate))
  (:map god-mode-isearch-map
        ("<escape>" . god-mode-isearch-disable)))

;; _____________________________________________________________________________
(provide 'eon-god)
;;; eon-god.el ends here
