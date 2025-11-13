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

;; TODO The loader is very primitive right now. It just `require's the features
;; listed in `eon-modules' in order.

;; TODO Maybe the loader could be extended to provide additional value, for
;; instance clean unloading and undefining on the fly for vars, functions and
;; keybindings introduced by a module; meaning all of that may work without
;; restarting Emacs.

;; TODO Enabling/disabling modules shouldn't be done by uncommenting/commenting.
;; There are more elegant and sensible ways; must work from Lisp code,
;; the `customize' UI and maybe `completing-read'.

;; TODO Modules are currently implemented as Emacs "features". Therefore they
;; are basically Emacs packages, after the required metadata has been added to
;; each module. That way they could be available as "meta-packages" on Melpa /
;; GNU Elpa. It might be interesting to treat them as global minor modes also.

;; TODO Deprecate 'eon-setup-personal.el' in favor of a directory for personal
;; modules and config files; e.g. '~/.emacs.d/eon/'.

;; TODO Change license to GPL v3 and greater.

;; _____________________________________________________________________________
;;; USE-PACKAGE

;; Let `imenu' recognize `use-package' and `require' forms?
(setopt use-package-enable-imenu-support t)

(require 'use-package)

;; Setup `use-package' options
(setopt use-package-always-ensure nil
        use-package-always-defer nil
        use-package-expand-minimally nil)

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

;; Function to load a single module
;; TODO Will be extended further
(defun eon-require-module (feature)
  "Require FEATURE, report an error if loading fails.
Central function to load an Eon module."
  (condition-case err
      (require feature)
    (error (message "Failed to load %s: %s" feature err))))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Built-in modules

;; Detect and define the path of the EON root directory
(defvar eon-root-dir
  (file-name-directory (or load-file-name buffer-file-name
                           (locate-library "eon-init")))
  "Detected absolute path of the directory containing 'eon-init.el'.
The path ends with a trailing slash; will be added to the `load-path'.")

;; Add the directory to the `load-path'
(add-to-list 'load-path eon-root-dir)

;; Define the directory for built-in modules
(defvar eon-modules-dir
  (concat eon-root-dir "modules/")
  "Path of the directory containing the EON modules.
The path ends with a trailing slash; will be added to the `load-path'.")

;; Add the directory to the `load-path'
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

;; Define the path of the EON user directory
(defcustom eon-user-dir
  (expand-file-name (concat user-emacs-directory "eon-user/"))
  "Path of the EON user directory.
Defaults to the directory 'eon-user/' within the Emacs init directory;
e.g. '~/.emacs.d/eon-user/' or similar, depending on your system/config.
The path must end with a trailing slash; will be added to the `load-path'."
  :type '(directory)
  :group 'eon)

;; Add the directory to the `load-path'
(add-to-list 'load-path eon-user-dir)

;; Define the path of the EON user modules directory
(defvar eon-user-modules-dir
  (concat eon-user-dir "modules/")
  "Path of the directory containing the EON user modules.
The path ends with a trailing slash; will be added to the `load-path'.")

;; Add the directory to the `load-path'
(add-to-list 'load-path eon-user-modules-dir)

;; List of user-defined modules ready to load
(defcustom eon-user-modules nil
  "List of user-defined modules (Emacs features) to load.
Contains enabled modules residing in the in `eon-user-dir', e.g.
'~/.emacs.d/eon-user/modules/'."
  :type '(repeat (symbol :tag "Feature"))
  :group 'eon)

(defun eon-user-modulep (module-name)
  "True if MODULE-NAME is in the list `eon-user-modules'.
Doesn't indicate if MODULE-NAME is loaded; to find out if a module is
loaded, use `featurep' instead."
  (memq module-name eon-user-modules))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Contrib modules
;; NOTE Not implemented yet

;; Contrib modules will be:
;; - Meta-Packages available on Melpa etc.
;; - Meta-Packages as Git/VC repos

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Bootstrap

;; Load the file with the module selection `eon-setup-modules.el',
;; which in turn sets the initially empty custom variable `eon-modules'.
(eon-require-module 'eon-setup-modules)

;; Require each module from the module selection
(defun eon-init-with (modules-list)
  "Require each selected Eon module from MODULES-LIST.
The modules then will install necessary 3rd-party Emacs packages."
  (dolist (module modules-list)
    (eon-require-module module)))

;; Load all default modules
(eon-init-with eon-modules)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Helpers

;; Placeholder, added for UX/consistency.
(defalias 'eon-load-module #'load-library)
(defalias 'eon-unload-module #'unload-feature)

;; _____________________________________________________________________________
(provide 'eon-init)
;;; eon-init.el ends here
