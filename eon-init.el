;;; eon-init.el --- Emacs ONTOP extension layers -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.
;;
;;
;;    ▒░▒░▒░   ▒░     ▒░ ▒░▒░▒░▒░▒░  ▒░▒░▒░   ▒░▒░▒░▒░
;;   ▒░    ▒░  ▒░▒░   ▒░ ▒░  ▒░  ▒░ ▒░    ▒░  ▒░     ▒░
;;  ▒░      ▒░ ▒░ ▒░  ▒░     ▒░    ▒░      ▒░ ▒░     ▒░
;;  ▒░      ▒░ ▒░  ▒░ ▒░     ▒░    ▒░      ▒░ ▒░▒░▒░▒░
;;  ▒░      ▒░ ▒░   ▒░▒░     ▒░    ▒░      ▒░ ▒░
;;   ▒░    ▒░  ▒░     ▒░     ▒░     ▒░    ▒░  ▒░
;;    ▒░▒░▒░  ▒░      ▒░     ▒░      ▒░▒░▒░   ▒░
;;

;;; Commentary:
;;
;; EON is an Emacs distribution built on top of the Emacs ONBOARD starter-kit
;;
;;  --> LOAD THIS FILE from your init file `~/.emacs.d/init.el' or `~/.emacs'
;;      via (load-file (expand-file-name "~/.emacs.ontop/eon-init.el"))
;;
;;; Code:

;; _____________________________________________________________________________
;;; PLANS AND IDEAS

;; MAYBE The loader could be extended to provide additional value, for
;; instance clean unloading and undefining on the fly for vars, functions and
;; keybindings introduced by a module; meaning all of that may work without
;; restarting Emacs.

;; MAYBE Enabling/disabling modules shouldn't be done by uncommenting and
;; commenting. There might be more elegant and sensible ways; must work from
;; Lisp code and the `customize' UI.

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
;; (use emacsclient for fast startup). However, a test has shown that through
;; deffered loading, startup-time almost could be cut in half.

;; _____________________________________________________________________________
;;; USE-PACKAGE

;; Let `imenu' recognize `use-package' and `require' forms?
(setopt use-package-enable-imenu-support t)

(require 'use-package)

;; Setup `use-package' options
(setopt use-package-always-ensure (if package-enable-at-startup t nil)
        use-package-always-defer nil
        use-package-expand-minimally t)

;; Options when Emacs is started via "emacs --debug-init"
(when init-file-debug
  (setopt use-package-verbose t
          use-package-expand-minimally nil
          ;; "M-x use-package-report" to see the result for statistics
          use-package-compute-statistics t))

;; Enable the built-in `use-package' extension ":ensure-system-package"
(use-package use-package-ensure-system-package :ensure nil)

;; _____________________________________________________________________________
;;; ROOT DIRECTORY

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

;; _____________________________________________________________________________
;;; BUILT-IN MODULES

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

;; _____________________________________________________________________________
;;; USER-DEFINED MODULES
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

;; _____________________________________________________________________________
;;; CONTRIB MODULES
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

;; _____________________________________________________________________________
;;; USER SETUP

;; Create user directories according to the variables
;; `eon-user-dir', `eon-user-modules-dir' and `eon-user-contrib-dir',
;; then copy eon-setup-modules.el into `eon-user-dir'.

(defun eon-user-setup--dirs ()
  "Create user directory and sub-directories if they don't exist."
  (dolist (entry `((,eon-user-dir "User directory")
                   (,eon-user-modules-dir "User modules directory")
                   (,eon-user-contrib-dir "User contrib directory")))
    (pcase-let ((`(,dir ,label) entry))
      (unless (file-directory-p dir)
        (make-directory dir 'parents)
        (message "%s created: %s" label dir)))))

(defun eon-user-setup--files ()
  "Create user files if they don't exist."
  (let* ((src-mod-setup (expand-file-name "eon-setup-modules.el"
                                          eon-root-dir))
         (dest-mod-setup (expand-file-name "eon-setup-modules.el"
                                           eon-user-dir))
         (src-user-mod  (expand-file-name "eon-user.el"
                                          eon-modules-dir))
         (dest-user-mod  (expand-file-name "eon-user.el"
                                           eon-user-modules-dir)))
    (unless (file-exists-p src-mod-setup)
      (user-error "Missing template: %s" src-mod-setup))
    (unless (file-exists-p src-user-mod)
      (user-error "Missing template: %s" src-user-mod))
    (unless (file-exists-p dest-mod-setup)
      (copy-file src-mod-setup dest-mod-setup)
      (message "Created module setup: %s" dest-mod-setup))
    (unless (file-exists-p dest-user-mod)
      (copy-file src-user-mod dest-user-mod)
      (message "Created personal module: %s" dest-user-mod))))

(defun eon-user-setup ()
  "Create user directory and files, if they don't exist yet."
  (interactive)
  (eon-user-setup--dirs)
  (eon-user-setup--files)
  (message "Your user directory is ready: %s" eon-user-dir))

(defun eon-user-find-file ()
  "Run `find-file' in `eon-user-dir'."
  (interactive)
  (unless (file-directory-p eon-user-dir)
    (user-error "User directory doesn't exist at %s; \
run `eon-user-setup' first" eon-user-dir))
  (let ((default-directory eon-user-dir))
    (call-interactively #'find-file)))

;; Add keybindings to the leader
(with-eval-after-load 'eon
  (keymap-set ctl-z-x-map "C-u" #'eon-user-setup)
  (keymap-set ctl-z-x-map "u"   #'eon-user-find-file)
  (keymap-set ctl-z-f-map "u"   #'eon-user-find-file))

;; _____________________________________________________________________________
;;; LOADER

(require 'cl-lib)

(defmacro eon-module-metadata (&rest plist)
  "Declare PLIST as metadata for an EON module.

The EON loader reads this form from module source files before the
module is loaded. At normal evaluation time, it expands to nil.

Supported keywords:

  :requires   List of EON modules that must be loaded before this module.
  :conflicts  List of EON modules that must not be loaded together with this
              module."
  (ignore plist)
  nil)

(defun eon-module-load-path ()
  "Return list of existing EON module directories.
Built from `eon-modules-dir', `eon-user-modules-dir' and
`eon-user-contrib-dir', keeping only those that name existing directories."
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

(defun eon--warning (message &optional level)
  "Report MESSAGE as an EON warning with optional LEVEL.

When Emacs is still starting up, delay warning display until
`emacs-startup-hook', because early startup warnings can be delayed
or otherwise not displayed reliably in `*Warnings*'."
  (message "[EON] %s" message)
  (if after-init-time
      (display-warning 'eon message (or level :error) "*Warnings*")
    (add-hook
     'emacs-startup-hook
     (lambda ()
       (display-warning 'eon message (or level :error) "*Warnings*")))))

(defun eon-module-file (module &optional source-only)
  "Return the EON module file for MODULE.

When SOURCE-ONLY is non-nil, only return a readable .el source file.
Search only EON module directories, not the whole `load-path'."
  (catch 'found
    (dolist (directory (eon-module-load-path))
      (let* ((name (symbol-name module))
             (source-file (expand-file-name (concat name ".el") directory))
             (compiled-file (expand-file-name (concat name ".elc")
                                              directory)))
        (cond
         ((file-readable-p source-file)
          (throw 'found source-file))
         ((and (not source-only)
               (file-readable-p compiled-file))
          (throw 'found compiled-file)))))))

(defun eon-module-known-p (module)
  "Return non-nil if MODULE names an existing EON module."
  (and (symbolp module)
       (eon-module-file module)))

(defvar eon-module-metadata-cache nil
  "Alist mapping EON module symbols to metadata plists.")

(defun eon-module-metadata--read-form (module)
  "Return the raw `eon-module-metadata' form from MODULE, or nil."
  (when-let* ((file (eon-module-file module 'source-only)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (catch 'metadata
        (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                (when (and (consp form)
                           (eq (car form) 'eon-module-metadata))
                  (throw 'metadata form))))
          (end-of-file nil))))))

(defun eon-module-metadata--symbol-list (value key module)
  "Normalize VALUE from metadata KEY in MODULE into a list of symbols."
  (when (and (consp value)
             (eq (car value) 'quote))
    (setq value (cadr value)))
  (unless (or (null value)
              (listp value))
    (error "Invalid %S metadata in %S: expected a list, got %S"
           key module value))
  (dolist (entry value)
    (unless (symbolp entry)
      (error "Invalid %S metadata in %S: %S is not a symbol"
             key module entry)))
  value)

(defun eon-module-metadata-read (module)
  "Read and validate metadata for MODULE."
  (unless (eon-module-known-p module)
    (error "Unknown EON module: %S" module))
  (let* ((form (eon-module-metadata--read-form module))
         (metadata (cdr-safe form))
         (requires (eon-module-metadata--symbol-list
                    (plist-get metadata :requires) :requires module))
         (conflicts (eon-module-metadata--symbol-list
                     (plist-get metadata :conflicts) :conflicts module)))
    (dolist (required requires)
      (unless (eon-module-known-p required)
        (error "Module %S requires unknown EON module %S"
               module required)))
    (dolist (conflict conflicts)
      (unless (eon-module-known-p conflict)
        (error "Module %S conflicts with unknown EON module %S"
               module conflict)))
    (list :module module
          :requires requires
          :conflicts conflicts)))

(defun eon-module-metadata-get (module)
  "Return cached metadata for MODULE."
  (or (alist-get module eon-module-metadata-cache)
      (let ((metadata (eon-module-metadata-read module)))
        (push (cons module metadata) eon-module-metadata-cache)
        metadata)))

(defun eon-module-requires (module)
  "Return modules required by MODULE."
  (plist-get (eon-module-metadata-get module) :requires))

(defun eon-module-conflicts (module)
  "Return modules conflicting with MODULE."
  (plist-get (eon-module-metadata-get module) :conflicts))

(defun eon-module-conflict-p (module other-module)
  "Return non-nil if MODULE conflicts with OTHER-MODULE."
  (or (memq other-module (eon-module-conflicts module))
      (memq module (eon-module-conflicts other-module))))

(defun eon-module-resolve (modules)
  "Resolve MODULES into a dependency-ordered, conflict-free load list.

Return a plist with these keys:

  :load     List of modules to load.
  :skipped  Alist of skipped modules and reasons.

Resolution rules:

  - :requires modules are added to the load graph and loaded before modules
    that require them.
  - If two modules conflict, both are skipped. There is no winner.
  - If a module requires a skipped module, it is skipped as well.
  - Dependencies that are only needed by skipped modules are not loaded."
  (let ((roots (copy-sequence modules))
        ordered
        seen
        visiting
        skipped)

    (cl-labels
        ((skip
          (module reason)
          (unless (assq module skipped)
            (push (cons module reason) skipped)))

         (skipped-p
          (module)
          (assq module skipped))

         (visit
          (module)
          (cond
           ((memq module seen)
            nil)
           ((memq module visiting)
            (skip module "circular dependency"))
           ((not (eon-module-known-p module))
            (skip module "unknown EON module"))
           (t
            (condition-case err
                (let ((requires (eon-module-requires module)))
                  (push module visiting)
                  (dolist (required requires)
                    (visit required))
                  (setq visiting (delq module visiting))
                  (push module seen)
                  (push module ordered))
              (error
               (setq visiting (delq module visiting))
               (skip module (error-message-string err)))))))

         (mark-needed
          (module needed)
          (if (or (assq module skipped)
                  (memq module needed))
              needed
            (let ((next (cons module needed)))
              (dolist (required (eon-module-requires module))
                (setq next (mark-needed required next)))
              next))))

      ;; Build dependency closure
      (dolist (module roots)
        (visit module))

      (setq ordered (nreverse ordered))

      ;; Detect conflicts. Conflict policy: no winner, skip all involved.
      (dolist (module ordered)
        (condition-case err
            (dolist (conflict (eon-module-conflicts module))
              (when (and (memq conflict ordered)
                         (not (eq module conflict)))
                (skip module
                      (format "conflicts with enabled module %S" conflict))
                (skip conflict
                      (format "conflicts with enabled module %S" module))))
          (error
           (skip module (error-message-string err)))))

      ;; Propagate skipped requirements.
      ;; If A requires B and B is skipped, A must be skipped too.
      (let ((changed t))
        (while changed
          (setq changed nil)
          (dolist (module ordered)
            (unless (skipped-p module)
              (condition-case err
                  (dolist (required (eon-module-requires module))
                    (when (skipped-p required)
                      (skip module
                            (format "requires skipped module %S" required))
                      (setq changed t)))
                (error
                 (skip module (error-message-string err))
                 (setq changed t)))))))

      ;; Keep only non-skipped roots and the dependencies still needed by them.
      ;; This prevents loading dependencies that were pulled in only by modules
      ;; later skipped because of conflicts or invalid metadata.
      (let (needed)
        (dolist (root roots)
          (setq needed (mark-needed root needed)))
        (list :load
              (cl-remove-if-not (lambda (module)
                                  (memq module needed))
                                ordered)
              :skipped
              (nreverse skipped))))))

(defun eon-load-module (&optional feature)
  "Require FEATURE.

Interactively, prompt for a module name using completion over all
.el/.elc files in the existing EON module directories."
  (interactive
   (let* ((paths (eon-module-load-path)))
     (unless paths
       (user-error "[EON] No module directories exist"))
     (let* ((files (apply #'append
                          (mapcar (lambda (directory)
                                    (directory-files
                                     directory nil "^[^.].*\\.elc?\\'"))
                                  paths)))
            (names (delete-dups
                    (mapcar #'file-name-sans-extension files)))
            (name (completing-read "EON module: " names nil t)))
       (list (intern name)))))

  (unless feature
    (error "FEATURE is required"))

  (unless (eon-module-known-p feature)
    (error "Unknown EON module: %S" feature))

  ;; Single-module loading must also honor conflicts against enabled modules
  ;; and already loaded EON modules
  (let* ((loaded-eon-modules
          (cl-remove-if-not #'eon-module-known-p features))
         (context
          (delete-dups
           (append (list feature) eon-modules loaded-eon-modules)))
         (context-resolution
          (eon-module-resolve context))
         (context-skipped
          (plist-get context-resolution :skipped))
         (feature-skipped
          (assq feature context-skipped)))
    (when feature-skipped
      (error "Cannot load %S: %s"
             (car feature-skipped)
             (cdr feature-skipped))))

  ;; Load the requested module's requirements before the module itself
  (let* ((resolution (eon-module-resolve (list feature)))
         (modules (plist-get resolution :load))
         (skipped (plist-get resolution :skipped)))
    (when skipped
      (let ((entry (or (assq feature skipped)
                       (car skipped))))
        (error "Cannot load %S: %s"
               (car entry)
               (cdr entry))))
    (dolist (module modules)
      (require module))))

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
;; TODO If contrib module/feature exists, ignore built-in module/feature
;;      of the same name.
;; TODO If user-defined module/feature exists, ignore both built-in
;;      module/feature and contrib module/feature of the same name.
;; TODO Add branch for interactive use, MAYBE ask for selection which
;;      set of modules to load, e.g. only built-in modules vs. all modules.
;; TODO Add option for forced reload.
(defun eon-load-modules (&optional modules-list)
  "Require each EON module from MODULES-LIST.

Dependencies are loaded first, conflicting modules are skipped.
If one module fails, report the error and continue loading the rest.
When called interactively, use `eon-modules'."
  (interactive)
  (let* ((resolution (eon-module-resolve (or modules-list eon-modules)))
         (modules (plist-get resolution :load))
         (skipped (plist-get resolution :skipped)))

    ;; Report modules skipped by dependency/conflict resolution.
    (dolist (entry skipped)
      (eon--warning
       (format "Skipped module %S: %s"
               (car entry)
               (cdr entry))
       :error))

    ;; Load unaffected modules
    (dolist (module modules)
      (condition-case err
          (require module)
        (error
         (eon--warning
          (format "Failed to load %S: %s"
                  module
                  (error-message-string err))
          :error))))))

;; Trigger loading of modules
(eon-load-modules eon-modules)

;; _____________________________________________________________________________
(provide 'eon-init)
;;; eon-init.el ends here
