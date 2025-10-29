;;; eon-setup-personal.el --- Personal Emacs Lisp -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; The code within this file will load last, so that you can shadow or override
;; any setting that has been defined by Emacs ONBOARD and Emacs ONTOP.
;;
;; You can also load other Elisp files from here via:
;; (load-file (expand-file-name "~/path/to/another-file.el"))
;;
;; Or you can load other Elisp files from your init.el after this file/module
;; and after any other file/module via:
;; (with-eval-after-load 'eon-setup-personal
;;   (load-file (expand-file-name "~/path/to/another-file.el")))
;;
;;; Code:

;;  ____________________________________________________________________________
;;; ELISP

(setopt user-full-name "My Name"
        user-mail-address "my-email@example.com")






;;  ____________________________________________________________________________
(provide 'eon-setup-personal)
;;; eon-setup-personal.el ends here
