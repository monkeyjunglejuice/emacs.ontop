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
;;; - Examples using `with-eval-after-load'
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
;;; - Examples using `use-package'
;; `use-package' is a unifying interface that wraps the previous two facilities
;; and several other mechanisms, in order to make configuring Emacs packages
;; easier.
;;
;; TODO Provide more examples
;;
;;; Code:

;; _____________________________________________________________________________
;;; ELISP

;;; - User info
;; (setopt user-full-name "My Name"
;;         user-mail-address "my-email@example.com")

;;; - Theme
;; Set the themes after the `eon' module has been loaded, otherwise the module
;; 'eon.el' overrides this by setting the built-in Modus themes.
;; Toggle between light/dark theme via "<leader> x t".
;; `eon-theme-spacemacs' should be enabled in `eon-setup-modules'.
;; (with-eval-after-load 'eon
;;   (setopt eon-theme-light 'spacemacs-light
;;           eon-theme-dark 'spacemacs-dark
;;           eon-theme-variant-default 'light)
;;   (eon-theme-load-default))

;;; - Fonts
;; Use a font installed on your computer.
;; (setopt eon-font-default "Iosevka"
;;         eon-font-default-size 140
;;         eon-font-fixed "Iosevka"
;;         eon-font-fixed-alt "Iosevka Slab"
;;         eon-font-proportional "Alegreya Sans"
;;         eon-font-proportional-size 160
;;         eon-font-marginal-size 0.85)

;; _____________________________________________________________________________
(provide 'eon-user)
;;; eon-user.el ends here
