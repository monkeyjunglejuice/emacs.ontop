;;; ontop-embark.el --- Embark settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;; <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package nil))

(eval-when-compile
  (require 'use-package))

;;  ____________________________________________________________________________
;;; EMBARK
;; <https://github.com/oantolin/embark>

(use-package embark
  :ensure t
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

(use-package embark
  :if (or (eon-linp) (eon-winp))
  :bind
  ("<menu>" . embark-act)
  ("M-<menu>" . embark-dwim))

(use-package embark
  :if (eon-macp)
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim))

;; Consult users will also want the embark-consult package
(use-package embark-consult
  :ensure t
  :after consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; ___________________________________________________________________________
(provide 'ontop-embark)
;;; ontop-embark.el ends here
