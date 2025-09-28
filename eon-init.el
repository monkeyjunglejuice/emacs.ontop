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
;; Package-Requires: ((EMACS "30.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;; Setup `use-package' options before loading the modules

(require 'use-package)
(setq use-package-always-ensure nil)

;;  ____________________________________________________________________________
;;; LOADER

;; Define the path of the Emacs ONTOP directory
(defvar eon-ontop-directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The directory that contains the file eon-init.el and the modules.")

;; Add the Emacs ONTOP directory to the `load-path`, so that we can `require`
;; modules or do `load-library` and `unload-feature` on the fly.
(add-to-list 'load-path eon-ontop-directory)

;; Define the initial module list
(defvar eon-modules '()
  "List of selected modules, implemented as Emacs features.
It is empty per default, and will be set in `eon-setup-modules.el'.")

;; Don't block if an error occurs
(defun eon-require-with-error-handling (feature)
  "Require FEATURE, reporting an error if loading fails."
  (condition-case err
      (require feature)
    (error (message "Failed to load %s: %s" feature err))))

;; Require and read your module selection
(eon-require-with-error-handling 'eon-setup-modules)

;; Load each module from the module selection
(defun eon-require-modules ()
  "Require the selected modules and install Emacs packages if necessary."
  (interactive)
  (dolist (module eon-modules)
    (eon-require-with-error-handling module)))

(eon-require-modules)

;; Added for UX/consistency
(defalias 'eon-load-module #'load-library)
(defalias 'eon-unload-module #'unload-feature)

;;  ____________________________________________________________________________
(provide 'eon-init)
;;; eon-init.el ends here
