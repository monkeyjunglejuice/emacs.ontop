;;; eon-personal.el --- Personal Emacs Lisp -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; --> If you haven't done yet, create your user directory first
;; via "M-x eon-user-setup". The location of your user directory is specified by
;; `eon-user-dir'. It can be chosen freely. If not specified otherwise, it will
;; be created within your Emacs init directory, e.g. '~/.emacs.d/'.
;;
;; This file - `eon-personal.el' - as well as `eon-setup-modules.el' will be
;; copied into your user directory.
;;
;; Once `eon-personal.el' or `eon-setup-modules.el' exist in your user
;; directory, they will take precedence. Both files initially present in
;; `eon-modules-dir' and `eon-root-dir' will be ignored from there on.
;;
;;; - How to use this file
;;
;; Note, you don't have to use this file at all, but it's there as a starting
;; point for your convenience. Alternatively, simply write your Emaacs Lisp
;; code in your `init.el' or other Lisp files. That being said ...
;;
;; Per default, the code within this file will run after all the other modules,
;; so that you can easily override any settings defined in the modules coming
;; with EON. If you want to run code before any of the EON modules, put that
;; code in your `init.el' file; easily reachable via "<leader> f i".
;;
;; For finer-grained control what code to load when, you can either use
;; `add-hook', `with-eval-after-load' or `use-package' forms.
;;
;;; - Example using hooks
;;
;; Delay loading of another Elisp file until Emacs has been started:
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (load-file (expand-file-name "~/path/to/another-file.el"))))
;;
;;; - Example using `with-eval-after-load'
;;
;; Load another Elisp file directly after this file:
;; (with-eval-after-load 'eon-personal
;;   (load-file (expand-file-name "~/path/to/another-file.el")))
;;
;;; - Examples using `use-package'
;;
;; `use-package' is a unifying interface that wraps the previous two facilities
;; and several other mechanisms, in order to make configuring Emacs packages
;; easier.
;;
;; TODO Provide use-package example
;;
;;; Code:

;; _____________________________________________________________________________
;;; ELISP

;;; - Files and directories you trust
(eon-add-to-list 'trusted-content
                 `(,eon-user-dir  ; your personal user directory
                   ;; "~/emacs-lisp/"  ; example
                   ))

;;; - Theme
;; Toggle between light/dark theme via "<leader> x t".
;; (setopt eon-theme-light 'batppuccin-latte
;;         eon-theme-dark 'batppuccin-frappe
;;         eon-theme-variant-default 'light)
;; (eon-theme-load-default)

;;; - Fonts
;; Use a font installed on your computer.
;; (setopt eon-font-default "Iosevka"
;;         eon-font-default-size 140
;;         eon-font-fixed "Iosevka"
;;         eon-font-fixed-alt "Iosevka Slab"
;;         eon-font-proportional "Alegreya Sans"
;;         eon-font-proportional-size 160
;;         eon-font-marginal-size 0.85)

;;; - Your user info
;; (setopt user-full-name "My Name"
;;         user-mail-address "my-email@example.com")

;; _____________________________________________________________________________
(provide 'eon-personal)
;;; eon-personal.el ends here
