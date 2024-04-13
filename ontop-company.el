;;; ontop-company.el --- Auto-completion  -*- lexical-binding: t; -*-
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
;;; COMPANY
;; <http://company-mode.github.io/>

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :diminish
  :custom
  ;; If you're a speedy typer, you may decrease the delay
  (company-idle-delay 0.3)
  ;; Removed space character (32) from the list
  (company-insertion-triggers '(41 46))
  (company-lighter-base "Comp")
  ;; Lower prefix length makes company slower
  (company-minimum-prefix-length 3)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  :config
  ;; Don't insert a selected completion candidate by hitting the return key
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map [return] nil)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        ("C-c C-d" . company-show-doc-buffer)
        ("C-c C-l" . company-show-location)))

;;; ___________________________________________________________________________
(provide 'ontop-company)
;;; ontop-company.el ends here
