;;; eon-embark.el --- Embark -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;  ____________________________________________________________________________
;;; EMBARK
;; <https://github.com/oantolin/embark>

(use-package embark :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :hook
  ;; Show the Embark target at point via Eldoc.
  (eldoc-documentation-functions . embark-eldoc-first-target)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  ("C-h B" . embark-bindings))

(use-package embark :ensure t
  :if (or (eon-linp) (eon-winp))
  :bind
  ("<menu>" . embark-act)
  ("M-<menu>" . embark-dwim))

(use-package embark :ensure t
  :if (eon-macp)
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim))

(use-package embark :ensure t
  :after vertico
  :config
  (defun +embark-live-vertico ()
    "Shrink Vertico minibuffer when `embark-live' is active."
    (when-let (win (and (string-prefix-p "*Embark Live" (buffer-name))
                        (active-minibuffer-window)))
      (with-selected-window win
        (when (and (bound-and-true-p vertico--input)
                   (fboundp 'vertico-multiform-unobtrusive))
          (vertico-multiform-unobtrusive)))))
  (add-hook 'embark-collect-mode-hook #'+embark-live-vertico))

;; Consult users will also want the embark-consult package
(use-package embark-consult :ensure t
  :after consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; ___________________________________________________________________________
(provide 'eon-embark)
;;; eon-embark.el ends here
