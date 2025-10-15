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
;;  Emacs ONTOP is an extension on top of the Emacs ONboard starter-kit
;;
;;  
;;  --> LOAD THIS FILE from your init file `~/.emacs.d/init.el' or `~/.emacs'
;;      via (load-file (expand-file-name "~/.emacs.ontop/eon-init.el"))
;;
;;
;; Copyright (C) 2022-2025 Dan Dee
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Version: 1.1.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; USE-PACKAGE

;; Let `imenu' recognize `use-package' and `require' forms?
(setopt use-package-enable-imenu-support t)

(require 'use-package)

;; Setup `use-package' options
(setopt use-package-always-ensure nil
        use-package-expand-minimally t)

;; Options when Emacs is started via "emacs --debug-init"
(when init-file-debug
  (setopt use-package-verbose t
          use-package-expand-minimally nil
          ;; "M-x use-package-report" to see the result for statistics
          use-package-compute-statistics t))

;; Enable the built-in `use-package' extension ":ensure-system-package"
(use-package use-package-ensure-system-package)

;;  ____________________________________________________________________________
;;; LOADER

;; TODO The loader is very primitive right now. It just `require's the features
;; listed in `eon-modules' in order.

;; TODO Maybe the whole loader should be extended to provide real value, for
;; instance clean unloading and undefining on the fly for vars, functions and
;; keybindings introduced by a module; meaning all of that should work without
;; restarting Emacs. And also a module-selection menu based on `completing-read'
;; (targeting `fido-vertical-mode' and `vertico'), and/or the `customize' UI.

;; Define the path of the Emacs ONTOP directory
(defvar eon-ontop-directory
  (file-name-directory (or load-file-name buffer-file-name
                           (locate-library "eon-init")))
  "Directory containing eon-init.el and modules.")

;; Add the Emacs ONTOP directory to the `load-path', so that we can `require'
;; modules or do `load-library' and `unload-feature'.
(add-to-list 'load-path eon-ontop-directory)

;; Define the initial module list
(defcustom eon-modules nil
  "List of selected modules (Emacs features) available to load.
The variable will be set by requiring `eon-setup-modules.el'."
  :type '(repeat (symbol :tag "Feature"))
  :group 'eon)

(defun eon-modulep (module-name)
  "Check if MODULE-NAME is in the list of available modules `eon-modules'.
Doesn't indicate if `MODULE-NAME' is loaded; use `featurep' instead to
find out if a module is loaded."
  (memq module-name eon-modules))

;; Don't block if an error occurs
(defun eon-require-with-error-handling (feature)
  "Require FEATURE, reporting an error if loading fails."
  (condition-case err
      (require feature)
    (error (message "Failed to load %s: %s" feature err))))

;; Require and read the module selection from the file `eon-setup-modules.el'.
;; Also setopts the custom variable `eon-modules'.
(eon-require-with-error-handling 'eon-setup-modules)

;; Load each module from the module selection
(defun eon-require-modules ()
  "Require the selected modules.
The modules then will install necessary 3rd-party Emacs packages."
  (interactive)
  (dolist (module eon-modules)
    (eon-require-with-error-handling module)))

;; Trigger
(eon-require-modules)

;; Placeholder, added for UX/consistency.
(defalias 'eon-load-module #'load-library)
(defalias 'eon-unload-module #'unload-feature)

;;  ____________________________________________________________________________
(provide 'eon-init)
;;; eon-init.el ends here
