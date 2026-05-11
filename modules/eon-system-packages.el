;;; eon-system-packages.el --- Manage OS packages within Emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;; System-packages is an Emacs frontend to various system package managers.
;; It attempts to guess which system package manager to use,
;; and lets you manage your system packages directly from Emacs via
;; "M-x system-packages".
;;
;; If the detected package manager is wrong or you prefer a different one,
;; then set `system-packages-package-manager' directly, e.g.
;; (setopt system-packages-package-manager 'pacman) in your 'init.el',
;; or use the Customization UI "M-x customize-group RET system-packages RET".
;;
;; Default mapping from Emacs commands to package manager commands defined by
;; `system-packages-supported-package-managers'.
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; SYSTEM PACKAGES
;; <https://gitlab.com/jabranham/system-packages>

(use-package system-packages :ensure t

  :config

  ;; Rerun detection logic, because Homebrew seems not recognized with Emacs
  ;; started from macOS app (Emacs started from the terminal works).
  ;;
  ;; In the current upstream system-packages.el the detection is done inside
  ;; the defcustom’s initform.
  ;;
  ;; If that initform was evaluated when the file was compiled/loaded in
  ;; an Emacs that didn’t have brew (or any of the others) on $PATH,
  ;; then the result of the initform is nil, and that nil is what Emacs keeps
  ;; as the value.
  ;;
  ;; So later, even though now (executable-find "brew") is non-nil,
  ;; the variable stays nil, because defcustom doesn’t re-run its initform
  ;; on every load.

  (when (null system-packages-package-manager)
    ;; Re-evaluate the original initform the package used
    (setopt system-packages-package-manager
            (eval (car (get 'system-packages-package-manager
                            'standard-value)))))

  (when (null system-packages-use-sudo)
    (let* ((entry (assoc system-packages-package-manager
                         system-packages-supported-package-managers))
           (sudo (cdr (assoc 'default-sudo (cdr entry)))))
      (when sudo
        (setopt system-packages-use-sudo sudo))))

  ;; Use an `eshell' buffer instead of `async-shell-command'
  ;; TODO Don't just set `eshell-mode', but run command in an actual eshell

  (defcustom eon-system-packages-backend 'eshell
    "Run system-packages commands via this backend.
- 'eshell' provides a nicer output and interactivity afterwards.
- 'async-shell-command' dumps the output verbatim into a simple buffer."
    :type '(choice (const eshell) (const async-shell-command))
    :group 'eon-misc)

  (defun eon-system-packages--eshell (cmd dir)
    (let ((buf (get-buffer "*system-packages*")))
      (unless (and buf (buffer-live-p buf)
                   (with-current-buffer buf
                     (derived-mode-p 'eshell-mode)))
        (setq buf (get-buffer-create "*system-packages*"))
        (with-current-buffer buf
          (setq default-directory dir)
          (eshell-mode)))
      (with-current-buffer buf
        (setq default-directory dir)
        (goto-char (point-max))
        (insert cmd)
        (eshell-send-input))
      (pop-to-buffer buf)
      buf))

  (defun eon-system-packages--run (orig action &optional pack args)
    (let* ((cmd (system-packages-get-command action pack args))
           (dir (if system-packages-use-sudo "/sudo::" default-directory)))
      (if (eq eon-system-packages-backend 'eshell)
          (eon-system-packages--eshell cmd dir)
        (let ((default-directory dir))
          (funcall orig action pack args)))))

  (advice-add 'system-packages--run-command
              :around #'eon-system-packages--run))

;; _____________________________________________________________________________
(provide 'eon-system-packages)
;;; eon-system-packages.el ends here
