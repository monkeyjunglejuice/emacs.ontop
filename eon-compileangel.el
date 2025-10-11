;;; eon-compileangel.el --- Native compilation -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; EMACS SETTINGS

;; Prevent stale elisp bytecode from shadowing more up-to-date source files?
(setq load-prefer-newer t)

;; Natively compile packages at first use or immediately after installation?
(setq package-native-compile t)

;; Native-compile .elc files asynchronously?
(setq native-comp-jit-compilation t)

;; Ask whether to terminate asynchronous compilations on exit?
(setq native-comp-async-query-on-exit t)

;;  ____________________________________________________________________________
;;; COMPILE-ANGEL
;; <https://github.com/jamescherti/compile-angel.el>

(use-package compile-angel :ensure t
  :demand t
  :config
  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  ;; Exclude the custom-file, recentf, and savehist files
  ;; Ensure that compile-angel is loaded using `require`, `use-package`, or
  ;; another package manager, as compile-angel-excluded-files is declared after
  ;; the package is loaded.
  ;; Ensure that the value of `savehist-file` is updated before proceeding
  (with-eval-after-load "savehist"
    (push (concat "/" (file-name-nondirectory savehist-file))
          compile-angel-excluded-files))
  ;; Ensure that the value of `recentf-save-file` is updated before proceeding
  (with-eval-after-load "recentf"
    (push (concat "/" (file-name-nondirectory recentf-save-file))
          compile-angel-excluded-files))
  ;; Ensure that the value of `custom-file` is updated before proceeding
  (with-eval-after-load "cus-edit"
    (when (stringp custom-file)
      (push (concat "/" (file-name-nondirectory custom-file))
            compile-angel-excluded-files)))
  ;; Enable the (compile-angel-on-load-mode) mode after the above
  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose t)
  ;; Compile automatically when an Elisp file is saved?
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
  ;; (setq compile-angel-display-buffer t)
  ;; Global mode that compiles .el files before they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode))

;;  ____________________________________________________________________________
(provide 'eon-compileangel)
;;; eon-compileangel.el ends here
