;;; eon-init.el --- Emacs ONTOP extension layers -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;    ▒░▒░▒░   ▒░     ▒░ ▒░▒░▒░▒░▒░  ▒░▒░▒░   ▒░▒░▒░▒░
;;   ▒░    ▒░  ▒░▒░   ▒░ ▒░  ▒░  ▒░ ▒░    ▒░  ▒░     ▒░
;;  ▒░      ▒░ ▒░ ▒░  ▒░     ▒░    ▒░      ▒░ ▒░     ▒░
;;  ▒░      ▒░ ▒░  ▒░ ▒░     ▒░    ▒░      ▒░ ▒░▒░▒░▒░
;;  ▒░      ▒░ ▒░   ▒░▒░     ▒░    ▒░      ▒░ ▒░
;;   ▒░    ▒░  ▒░     ▒░     ▒░     ▒░    ▒░  ▒░
;;    ▒░▒░▒░  ▒░      ▒░     ▒░      ▒░▒░▒░   ▒░
;;
;;  Emacs ONTOP is an extension on top of the Emacs ONBOARD starter-kit
;;
;;
;;  --> LOAD THIS FILE from your init file `~/.emacs.d/init.el' or `~/.emacs'
;;      via (load-file (expand-file-name "~/.emacs.ontop/eon-init.el"))
;;
;;
;; Copyright (C) 2022-2025 Dan Dee
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Version: 1.3.2
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; PLANS AND IDEAS

;; MAYBE The loader could be extended to provide additional value, for
;; instance clean unloading and undefining on the fly for vars, functions and
;; keybindings introduced by a module; meaning all of that may work without
;; restarting Emacs.

;; TODO Enabling/disabling modules maybe shouldn't be done by uncommenting and
;; commenting. There might be more elegant and sensible ways; must work from
;; Lisp code, the `customize' UI and maybe `completing-read'.

;; TODO Modules are currently implemented as Emacs "features". Therefore they
;; are basically Emacs packages, if the required metadata has been added to
;; each module. That way they could be available as "meta-packages" on Melpa /
;; GNU Elpa. It might be interesting to treat them as global minor modes also.

;; TODO Add package metadata to all modules; as there are:
;; - Version number
;; - Minimum required Emacs version
;; - Dependencies
;; - License header
;; - what else?

;; MAYBE Define and add a metadata format for modules, if the metadata
;; required for packages turns out as not enough.

;; MAYBE Generate hooks for all modules automatically, since using
;; `with-eval-after-load' alone is quite blunt.
;; As there are:
;; before-load-hook, after-load-hook, before-unload-hook, after-unload-hook.

;; MAYBE Explore how to further leverage `use-package' regarding modules.

;; TODO Change license to GPL v3 and greater.

;; TODO Optimize for startup speed (reasonably), but don't lazy-load everything
;; per default, as run-time snappiness is more important than immediate startup
;; (use emacsclient for fast startup).

;; _____________________________________________________________________________
;;; USE-PACKAGE

;; Let `imenu' recognize `use-package' and `require' forms?
(setopt use-package-enable-imenu-support t)

(require 'use-package)

;; Setup `use-package' options
(setopt use-package-always-ensure nil
        use-package-always-defer nil
        use-package-expand-minimally t)

;; Options when Emacs is started via "emacs --debug-init"
(when init-file-debug
  (setopt use-package-verbose t
          use-package-expand-minimally nil
          ;; "M-x use-package-report" to see the result for statistics
          use-package-compute-statistics t))

;; Enable the built-in `use-package' extension ":ensure-system-package"
(use-package use-package-ensure-system-package)

;; _____________________________________________________________________________
;;; LOADER

;; Detect and define the path of the EON root directory
(defvar eon-root-dir
  (file-name-as-directory
   (file-name-directory (or load-file-name
                            ;; Provide file name for `eval-buffer' and friends
                            buffer-file-name
                            ;; Only works if directory is in `load-path'
                            (locate-library "eon-init"))))
  "Directory containing 'eon-init.el'.
The value always ends with a directory separator.")

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Built-in modules

;; Directory for built-in modules
(defvar eon-modules-dir
  (file-name-as-directory
   (expand-file-name "modules" eon-root-dir))
  "Directory containing the built-in EON modules.
The value always ends with a directory separator.")

;; Add the modules directory to the `load-path'
(add-to-list 'load-path eon-modules-dir)

;; List of modules ready to load
(defcustom eon-modules nil
  "List of EON modules (features) to load.
Each element is a symbol naming an Emacs feature. This variable is
typically set via `setopt' by requiring `eon-setup-modules'."
  :type '(repeat (symbol :tag "Feature"))
  :group 'eon)

(defun eon-modulep (module-name)
  "Return non-nil if MODULE-NAME is in `eon-modules'.
This does not indicate whether MODULE-NAME is already loaded;
for that, use `featurep'."
  (memq module-name eon-modules))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - User-defined modules
;; TODO Implement loading of user-defined modules

;; Modules in '[eon-user-dir]/modules/' should be handled separately,
;; or classified in another way so they can be told apart.
;; This might further enable specific logic, e.g. determining precedence,
;; managing dependencies, etc.

;; Define the path of the Emacs ONTOP user directory
(defcustom eon-user-dir
  (file-name-as-directory
   (expand-file-name "eon" user-emacs-directory))
  "EON user directory.

Defaults to the `eon' subdirectory of `user-emacs-directory', e.g.
`~/.emacs.d/eon/'. The value always ends with a directory
separator.

If you don't like the default path, move your user directory
somewhere else and customize this variable."
  :type '(directory)
  :group 'eon)

;; Define the path of the Emacs ONTOP user modules directory
(defvar eon-user-modules-dir
  (file-name-as-directory
   (expand-file-name "modules" eon-user-dir))
  "Directory containing the EON user modules.
The value always ends with a directory separator.")

;; Add the user modules directory to the `load-path'
(add-to-list 'load-path eon-user-modules-dir)

;; List of user-defined modules ready to load
(defcustom eon-user-modules nil
  "List of user-defined modules (features) to load.
Each element is a symbol naming an Emacs feature residing under
`eon-user-modules-dir', e.g. `~/.emacs.d/eon/modules/'."
  :type '(repeat (symbol :tag "Feature"))
  :group 'eon)

(defun eon-user-module-p (module-name)
  "Return non-nil if MODULE-NAME is in `eon-user-modules'.
This does not indicate whether MODULE-NAME is already loaded;
for that, use `featurep'."
  (memq module-name eon-user-modules))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Contrib modules
;; TODO Implement loading of contrib modules

;; Distinct from built-in modules and user-defined modules.
;; Contrib modules may be provided as:
;; - (Meta-)Packages available on Melpa etc.
;; - (Meta-)Packages as VC/Git repos
;; - Single Elisp files
;; - Directories with a bunch of files
;; Contrib modules may be installed via Package.el, other package managers
;; and manually by putting them into `eon-user-contrib-dir'.

(defcustom eon-user-contrib-dir
  (file-name-as-directory
   (expand-file-name "contrib" eon-user-dir))
  "Directory containing manually installed contrib modules.

Defaults to the directory 'contrib/' within your `eon-user-dir',
e.g. '~/.emacs.d/eon/contrib/'.

If you don't like the default path, move the contrib modules directory
to another location and adapt the the path.

The value always ends with a directory separator."
  :type '(directory)
  :group 'eon)

;; Add the contrib modules directory to the `load-path'
(add-to-list 'load-path eon-user-contrib-dir)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - User setup

;; Create user directories according to the variables
;; `eon-user-dir', `eon-user-modules-dir' and `eon-user-contrib-dir',
;; then copy eon-setup-modules.el into `eon-user-dir'.

(defun eon-user-setup--dirs ()
  (dolist (entry `((,eon-user-dir "User directory")
                   (,eon-user-modules-dir "User modules directory")
                   (,eon-user-contrib-dir "User contrib directory")))
    (pcase-let ((`(,dir ,label) entry))
      (unless (file-directory-p dir)
        (make-directory dir 'parents)
        (message "%s created: %s" label dir)))))

(defun eon-user-setup--files ()
  (let* ((src-setup (expand-file-name "eon-setup-modules.el"
                                      eon-root-dir))
         (dst-setup (expand-file-name "eon-setup-modules.el"
                                      eon-user-dir))
         (src-user  (expand-file-name "eon-user.el"
                                      eon-modules-dir))
         (dst-user  (expand-file-name "eon-user.el"
                                      eon-user-modules-dir)))
    (unless (file-exists-p src-setup)
      (user-error "Missing template: %s" src-setup))
    (unless (file-exists-p src-user)
      (user-error "Missing template: %s" src-user))
    (unless (file-exists-p dst-setup)
      (copy-file src-setup dst-setup)
      (message "Created user setup file: %s" dst-setup))
    (unless (file-exists-p dst-user)
      (copy-file src-user dst-user)
      (message "Created personal module: %s" dst-user))))

(defun eon-user-setup ()
  (interactive)
  (eon-user-setup--dirs)
  (eon-user-setup--files)
  (message "Your user directory is ready: %s" eon-user-dir))

(defun eon-goto-user-dir ()
  "Open your user directory in Dired."
  (interactive)
  (let ((dir eon-user-dir))
    (if (file-exists-p dir)
        (dired dir)
      (user-error "Your user directory doesn't exist at %s, \
you may run `eon-user-setup' first" dir))))

(defun eon-goto-user-module ()
  "Open your personal module file from the user directory."
  (interactive)
  (let ((file (concat eon-user-modules-dir "eon-user.el")))
    (if (file-exists-p file)
        (find-file file)
      (user-error "Personal module doesn't exist at %s, \
you may run `eon-user-setup' first" file))))

;; Add keybindings for both commands to the leader
(with-eval-after-load 'eon
  (keymap-set ctl-z-x-map "U" #'eon-goto-user-dir)
  (keymap-set ctl-z-x-map "u" #'eon-goto-user-module))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Bootstrap

(defun eon-module-load-path ()
  "Return list of existing EON module directories.
Built from `eon-modules-dir', `eon-user-modules-dir'and `eon-user-contrib-dir',
keeping only those that name existing directories."
  (delq nil
        (list (and (bound-and-true-p eon-modules-dir)
                   (file-directory-p eon-modules-dir)
                   eon-modules-dir)
              ;; TODO Implement loading from there
              (and (bound-and-true-p eon-user-modules-dir)
                   (file-directory-p eon-user-modules-dir)
                   eon-user-modules-dir)
              ;; TODO Implement loading from there
              (and (bound-and-true-p eon-user-contrib-dir)
                   (file-directory-p eon-user-contrib-dir)
                   eon-user-contrib-dir))))

(defun eon-load-module (&optional feature)
  "Require FEATURE, otherwise report error.
Interactively, prompt for a module name using completion over all
.el/.elc files in the existing EON module directories."
  ;; Interactive branch concerning manual module loading.
  ;; Presents a selection of all existing EON modules.
  (interactive
   (let* ((paths (eon-module-load-path)))
     (unless paths
       (user-error "No EON module directories exist"))
     (let* ((files (apply #'append
                          (mapcar (lambda (dir)
                                    (directory-files
                                     dir nil "^[^.].*\\.elc?\\'"))
                                  paths)))
            (names (delete-dups
                    (mapcar #'file-name-sans-extension files)))
            (name  (completing-read "EON module: " names nil t)))
       (list (intern name)))))
  ;; Non-interactive branch concerning initialization;
  ;; loads a single module.
  (unless feature
    (error "FEATURE is required when called non-interactively"))
  (condition-case err
      (require feature)
    (error
     (message "Failed to load %S: %s"
              feature (error-message-string err)))))

;; Placeholder
(defalias 'eon-unload-module #'unload-feature)

;; Add module commands to leader keybindings
(with-eval-after-load 'eon
  (keymap-set ctl-z-x-map "m" #'eon-load-module)
  ;; NOTE Unloads only manually loaded modules right now,
  ;; as it doesn't force unloading.
  (keymap-set ctl-z-x-map "M" #'eon-unload-module))

;; Load the module selection `eon-setup-modules.el',
;; which in turn sets the initially empty custom variable `eon-modules'.
(require 'eon-setup-modules
         (if (file-readable-p (concat eon-user-dir "eon-setup-modules.el"))
             ;; Feature from file in user directory takes precedence
             (concat eon-user-dir "eon-setup-modules.el")
           ;; Otherwise require feature from file in root directory
           (concat eon-root-dir "eon-setup-modules.el")))

;; Walk through the list of modules and load each module
;; TODO Implement loading of user-defined modules and contrib modules
;; TODO If user-defined module/feature exists, ignore built-in module/feature
;; TODO If contrib module/feature exists, ignore built-in module/feature
;; TODO Add branch for interactive use
(defun eon-load-modules (modules-list)
  "Require each EON module from MODULES-LIST in order."
  (dolist (module modules-list)
    (eon-load-module module)))

;; Load all modules
(eon-load-modules eon-modules)

;; _____________________________________________________________________________
(provide 'eon-init)
;;; eon-init.el ends here
