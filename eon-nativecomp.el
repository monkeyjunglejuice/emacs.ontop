;;; eon-nativecomp.el --- Native compilation  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:

;;; Code:

;;  ____________________________________________________________________________
;;; COMPILE-ANGEL
;; <https://github.com/jamescherti/compile-angel.el>

;; Don't kill Emacs as long as compiling processes are running
(setq native-comp-async-query-on-exit t)

;; Turn off Emacs' native copilation on demand and use Compile-angel instead
(setq native-comp-jit-compilation nil)

;; Load the newer file
(setq load-prefer-newer t)

(use-package compile-angel
  :demand t
  :config
  ;; Exclude the custom-file, recentf, and savehist files
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
  (setq compile-angel-verbose nil)

  ;; Compile automatically when an Elisp file is saved?
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
  ;; (setq compile-angel-display-buffer t)

  ;; Global mode that compiles .el files before they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode))

;;  ____________________________________________________________________________
(provide 'eon-nativecomp)
;;; eon-nativecomp.el ends here
