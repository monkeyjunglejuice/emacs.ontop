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
;; Version: 1.3.1
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

;; TODO Hooks should be automatically generated for all modules, since
;; using `with-eval-after-load' is ok but quite blunt.
;; As there are:
;; before-load-hook, after-load-hook, before-unload-hook, after-unload-hook.

;; MAYBE Explore how to further leverage `use-package' regarding modules.

;; TODO Deprecate 'eon-setup-personal.el' in favor of a directory for personal
;; modules and config files; e.g. '~/.emacs.d/eon/'.

;; TODO Rename file eon-setup-modules.el to eon-setup.el

;; TODO Add command `eon-user-setup' that creates the directory structure
;; under eon-user-dir and copies eon-setup.el there:
;; eon
;; ├── contrib
;; ├── modules
;; └── eon-setup.el

;; TODO Change license to GPL v3 and greater.

;; _____________________________________________________________________________
;;; USE-PACKAGE

;; Use-package is required for all modules except `eon'.
;; Hence, delay loading until `eon' has been loaded, in order to shave off
;; a few garbage collections.
(with-eval-after-load 'eon
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
  (use-package use-package-ensure-system-package))

;; _____________________________________________________________________________
;;; LOADER

;; Detect and define the path of the EON root directory
(defvar eon-root-dir
  (file-name-directory (or load-file-name buffer-file-name
                           (locate-library "eon-init")))
  "Detected absolute path of the directory containing 'eon-init.el'.
The path must end with a trailing slash.")

;; Add the root directory to the `load-path'
(add-to-list 'load-path eon-root-dir)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Built-in modules

;; Define the directory for built-in modules
(defvar eon-modules-dir
  (concat eon-root-dir "modules/")
  "Path of the directory containing the EON modules.
The path must end with a trailing slash.")

;; Add the modules directory to the `load-path'
(add-to-list 'load-path eon-modules-dir)

;; List of modules ready to load
(defcustom eon-modules nil
  "List of selected modules (Emacs features) to load.
The variable will be set via `setopt' by requiring
`eon-setup-modules.el'."
  :type '(repeat (symbol :tag "Feature"))
  :group 'eon)

(defun eon-modulep (module-name)
  "True if MODULE-NAME is in the list of available modules `eon-modules'.
Doesn't indicate if MODULE-NAME is loaded; to find out if a module is
loaded, use `featurep' instead."
  (memq module-name eon-modules))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - User-defined modules
;; NOTE Not implemented yet

;; Modules in '[eon-user-dir]/modules/' should be collected separately,
;; or classified in another way so they can be told apart.
;; This might further enable specific logic, e.g. determining precedence,
;; managing dependencies, etc.

;; Define the path of the Emacs ONTOP user directory
(defcustom eon-user-dir
  (expand-file-name (concat user-emacs-directory "eon/"))
  "Path of the EON user directory.

Defaults to the directory 'eon/' within your Emacs init directory; e.g.
'~/.emacs.d/eon/'. The default path may vary, depending on your
system/config.

If you don't like the default path, move your user direcory somewhere else
and set the path here. The path must end with a trailing slash."
  :type '(directory)
  :group 'eon)

;; Add the user directory to the `load-path'
(add-to-list 'load-path eon-user-dir)

;; Define the path of the Emacs ONTOP user modules directory
(defvar eon-user-modules-dir
  (concat eon-user-dir "modules/")
  "Path of the directory containing the EON user modules.
The path must end with a trailing slash.")

;; Add the user modules directory to the `load-path'
(add-to-list 'load-path eon-user-modules-dir)

;; List of user-defined modules ready to load
(defcustom eon-user-modules nil
  "List of user-defined modules (Emacs features) to load.
Contains enabled modules residing in the in `eon-user-dir',
e.g. '~/.emacs.d/eon/modules/'."
  :type '(repeat (symbol :tag "Feature"))
  :group 'eon)

(defun eon-user-modulep (module-name)
  "True if MODULE-NAME is in the list `eon-user-modules'.
Doesn't indicate if MODULE-NAME is loaded; to find out if a module
is loaded, use `featurep' instead."
  (memq module-name eon-user-modules))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Contrib modules
;; NOTE Not implemented yet

;; Distinct from built-in modules and user-defined modules.

;; Contrib modules will be:
;; - (Meta-)Packages available on Melpa etc.
;; - (Meta-)Packages as VC/Git repos

;; Contrib modules can be installed via Package.el, other package managers
;; and manually by putting them into `eon-user-contrib-dir'.

(defcustom eon-user-contrib-dir
  (expand-file-name (concat eon-user-dir "contrib/"))
  "Path of the directory containing manually installed EON contrib modules.

Defaults to the directory 'eon/contrib/' within your Emacs init
directory; e.g. '~/.emacs.d/eon/contrib/'.

The default path may vary, depending on your system/config.
If you don't like the default path, move the contrib modules directory
to another location and adapt the the path.

The path must end with a trailing slash."
  :type '(directory)
  :group 'eon)

;; Add the contrib modules directory to the `load-path'
(add-to-list 'load-path eon-user-contrib-dir)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Create user directory structure

;; TODO Create user directories according to the variables
;; `eon-user-dir', `eon-user-modules-dir' and `eon-user-contrib-dir',
;; then copy eon-setup-modules.el into `eon-user-dir'.

(defun eon-user-setup--dirs ()
  (make-directory eon-user-dir)
  (message "User directory created: %s" eon-user-dir)
  (make-directory eon-user-modules-dir)
  (message "User modules directory created: %s" eon-user-modules-dir)
  (make-directory eon-user-contrib-dir)
  (message "User contrib directory created: %s" eon-user-contrib-dir))

(defun eon-user-setup--files ()
  (copy-file (concat eon-root-dir "eon-setup-modules.el")
             (concat eon-user-dir "eon-setup-modules.el")
             nil nil)
  (copy-file (concat eon-root-dir "eon-setup-personal.el")
             (concat eon-user-dir "eon-setup-personal.el")
             nil nil))

(defun eon-user-setup ()
  (interactive)
  (eon-user-setup--dirs)
  (eon-user-setup--files)
  (message "Your user directory is ready: %s" eon-user-dir))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Bootstrap

(defun eon-module-load-path ()
  "Return list of existing EON module directories.
Built from `eon-root-dir', `eon-modules-dir', `eon-user-modules-dir'
and `eon-user-contrib-dir', keeping only those that name existing
directories."
  (delq nil
        ;; NOTE eon-root-dir will be removed from the `load-path'.
        ;; Only explicitly specified files should be loaded from there.
        (list (and (bound-and-true-p eon-root-dir)
                   (file-directory-p eon-root-dir)
                   eon-root-dir)
              (and (bound-and-true-p eon-modules-dir)
                   (file-directory-p eon-modules-dir)
                   eon-modules-dir)
              (and (bound-and-true-p eon-user-modules-dir)
                   (file-directory-p eon-user-modules-dir)
                   eon-user-modules-dir)
              ;; NOTE Not implemented yet; can be packages/repos rather than
              ;; "naked" modules.
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

;; Add module commands as leader keybindings
(with-eval-after-load 'eon
  (keymap-set ctl-z-x-map "m" #'eon-load-module)
  ;; NOTE Unloads only manually loaded modules right now,
  ;; as it doesn't force unloading.
  (keymap-set ctl-z-x-map "M" #'eon-unload-module))

;; Load the module selection `eon-setup-modules.el',
;; which in turn sets the initially empty custom variable `eon-modules'.
(require 'eon-setup-modules
         (if (file-readable-p (concat eon-user-dir "eon-setup-modules.el"))
             ;; Feature from file in the user directory takes precedence
             (concat eon-user-dir "eon-setup-modules.el")
           ;; Otherwise require feature from file in root directory
           (concat eon-root-dir "eon-setup-modules.el")))

;; Walk through a list of modules and load each module
(defun eon-load-modules (modules-list)
  "Require each EON module from MODULES-LIST in order."
  (dolist (module modules-list)
    (eon-load-module module)))

;; Load all default modules
(eon-load-modules eon-modules)

;; _____________________________________________________________________________
(provide 'eon-init)
;;; eon-init.el ends here
