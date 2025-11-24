;;; eon-autoupdate.el --- Update packages automatically -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; This module works with Emacs' built-in package manager. It must not be used
;; with other package managers such as Straight or Elpaca. Therefore the
;; packages therein will be installed/activated only when
;; `package-enable-at-startup' is non-nil, no matter if the module is enabled or
;; not.
;;
;;; Code:

;; _____________________________________________________________________________
;;; AUTO-UPDATE PACKAGES
;; <https://github.com/rranelli/auto-package-update.el>

(when package-enable-at-startup
  (use-package auto-package-update :ensure t
    :init
    (auto-package-update-maybe)
    :custom
    (auto-package-update-interval 7)
    (auto-package-update-show-preview nil)
    (auto-package-update-hide-results t)
    (auto-package-update-prompt-before-update t)
    ;; Keep old packages around to roll back if necessary
    (auto-package-update-delete-old-versions nil)
    (auto-package-update-excluded-packages '())))

;; _____________________________________________________________________________
(provide 'eon-autoupdate)
;;; eon-autoupdate.el ends here
