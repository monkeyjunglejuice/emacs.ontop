;;; eon-autoupdate.el --- Auto-update -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; AUTO-UPDATE PACKAGES
;; <https://github.com/rranelli/auto-package-update.el>

(use-package auto-package-update :ensure t
  :when package-enable-at-startup
  :init
  (auto-package-update-maybe)
  :custom
  (auto-package-package-update-show-preview t)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))

;;  ____________________________________________________________________________
(provide 'eon-autoupdate)
;;; eon-autoupdate.el ends here
