;;; eon-user.el --- Personal Emacs Lisp -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; First: You don't have to use this file at all, but it's there as a starting
;; point for your convenience. Alternatively, simply write your Emaacs Lisp
;; code in your init.el or other Lisp files. That being said ...
;;
;; --> If you haven't done yet, create your user directory first
;; via "M-x eon-user-setup". The location of your user directory is specified by
;; `eon-user-dir'. It can be chosen freely. If not specified otherwise, it will
;; be created within your Emacs init directory, e.g. '~/.emacs.d/'.
;;
;; This file - `eon-user.el' - as well as `eon-setup-modules.el' will be
;; copied into your user directory.
;;
;; Once `eon-user.el' or `eon-setup-modules.el' exist in your user directory,
;; they will take precedence. Both files initially present in `eon-modules-dir'
;; and `eon-root-dir' will be ignored from there on.
;;
;;; - How to use this file
;;
;; Per default, the code within this file will run before the other modules, so
;; you are in full control. It also means: if you want to override any setting
;; that will be defined by a certain module, the override should usually
;; happen after that specific module or package has been loaded. You can either
;; use `add-hook', `with-eval-after-load' or `use-package' forms to do that.
;;
;;; - Example using hooks
;;
;; Delay loading of another Elisp file until Emacs has been started:
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (load-file (expand-file-name "~/path/to/another-file.el"))))
;;
;;; - Examples `with-eval-after-load'
;;
;; Running some code after loading a specific module/package:
;; (with-eval-after-load 'eon
;;   (setopt eon-theme-light 'spacemacs-light
;;           eon-theme-dark 'spacemacs-dark
;;           eon-theme-variant-default 'light)
;;   (eon-theme-load-default))
;;
;; Load another Elisp file directly after this file:
;; (with-eval-after-load 'eon-user
;;   (load-file (expand-file-name "~/path/to/another-file.el")))
;;
;;; - Examples using `use-package' TODO
;; `use-package' is a unifying interface that wraps the previous two facilities
;; and several other mechanisms, in order to make configuring Emacs packages
;; easier.
;;
;;
;;
;;; Code:

;; _____________________________________________________________________________
;;; ELISP

;; (setopt user-full-name "My Name"
;;         user-mail-address "my-email@example.com")

;;; - Theme
;; `eon-theme-spacemacs' should be enabled in `eon-setup-modules'. Toggle
;; between light/dark theme via "<leader> x t". Set the themes after the `eon'
;; module has been loaded, otherwise `eon' overrides this by setting the
;; built-in modus themes.
;; TODO The `eon' module itself should check if another theme is already
;; enabled, and skip if that's the case.
;; (with-eval-after-load 'eon
;;   (setopt eon-theme-light 'spacemacs-light
;;           eon-theme-dark 'spacemacs-dark
;;           eon-theme-variant-default 'light)
;;   (eon-theme-load-default))






;; _____________________________________________________________________________
(provide 'eon-user)
;;; eon-user.el ends here
