;;; ontop-setup-personal.el --- Personal Elisp  -*- lexical-binding: t; -*-

;;; Commentary:
;; The code within this file will load last, so that you can shadow or override
;; any setting that has been defined by Emacs ONBOARD and Emacs ONTOP.

;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;; <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package nil))

(eval-when-compile
  (require 'use-package))

;;  ____________________________________________________________________________
;;; ELISP

;; (setq user-full-name "My Name"
;;       user-mail-address "my-email@example.com")

;; You can also load other Elisp files from here
;; (load-file (expand-file-name "~/path/to/another-file.el"))














;;  ____________________________________________________________________________
(provide 'ontop-setup-personal)
;;; ontop-setup-personal.el ends here
