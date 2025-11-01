;;; eon-cape.el --- Completion-at-point extensions -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; CAPE

;; Cape extends the capabilities of in-buffer completion. It integrates with
;; Corfu or the default completion UI by providing additional backends through
;; completion-at-point-functions.

(use-package cape :ensure t

  :preface

  (require 'cl-lib)

  (defvar-local eon-cape--super nil
    "Buffer-local super CAPF built by CAPE, or nil.")

  (defun eon-cape--install-super-capf ()
    "Build & install one merged CAPF for the current buffer (no args).

Collect current CAPFs (minus `eon-cape--super'), append Tempel/CAPE
fallbacks when available, wrap with `cape-super-capf', and set it
buffer-locally in `completion-at-point-functions'."
    (let* ((existing
            (if (local-variable-p 'completion-at-point-functions)
                (delq eon-cape--super
                      (copy-sequence completion-at-point-functions))
              (default-value 'completion-at-point-functions)))
           (capfs
            (cl-remove-duplicates
             (delq nil
                   (append existing
                           (list
                            ;; Eglot CAPF (usually already present)
                            (when (fboundp 'eglot-completion-at-point)
                              #'eglot-completion-at-point)
                            ;; Tempel CAPF (snippets)
                            (when (fboundp 'tempel-complete)
                              #'tempel-complete)
                            ;; Generic fallbacks
                            (when (fboundp 'cape-file)    #'cape-file)
                            (when (fboundp 'cape-symbol)  #'cape-symbol)
                            (when (fboundp 'cape-keyword) #'cape-keyword)
                            (when (fboundp 'cape-dabbrev) #'cape-dabbrev)
                            (when (fboundp 'cape-history) #'cape-history))))
             :test #'eq))
           (super (and (fboundp 'cape-super-capf)
                       (apply #'cape-super-capf capfs))))
      (when super
        (setq-local eon-cape--super super)
        (setq-local completion-at-point-functions (list super)))))

  :init

  ;; Install for common editing modes.
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'eon-cape--install-super-capf))

  ;; Rebuild when Eglot (re)attaches so its CAPF gets merged.
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook #'eon-cape--install-super-capf))

  ;; Sensible dabbrev defaults.
  (setq cape-dabbrev-check-other-buffers t
        cape-dabbrev-min-length 3))

;; _____________________________________________________________________________
(provide 'eon-cape)
;;; eon-cape.el ends here
