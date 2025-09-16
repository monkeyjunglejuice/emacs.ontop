;;; eon-autoupdate.el --- Auto package update  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-autoupdate.el")'.

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
