;;; ontop-webdev.el --- HTML and CSS configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-webdev.el")'.

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
;;; WEB-MODE
;; <https://web-mode.org>

(use-package web-mode
  :ensure t
  :defer t
  :custom
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-expanding t)
  (web-mode-enable-block-face t)
  ;; TODO: styling required for web-mode-current-element-highlight-face
  (web-mode-enable-current-element-highlight t)
  ':mode
  ;; Associate Web-mode with the following file types?
  '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . web-mode)
  '("\\.css\\'" . web-mode)
  :bind
  (:map web-mode-map
        ("C-M-f" . web-mode-forward-sexp)
        ("C-M-b" . web-mode-backward-sexp)))

;;  ____________________________________________________________________________
;;; HTML-MODE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#HTML-Mode>

(use-package sgml-mode
  :ensure nil
  :defer t)

;;  ____________________________________________________________________________
;;; CSS-MODE

(use-package css-mode
  :ensure nil
  :defer t
  :custom
  (css-indent-offset 2))

;;  ____________________________________________________________________________
;;; JS-MODE

(use-package js
  :ensure nil
  :defer t)

;;  ____________________________________________________________________________
;;; SYNTAX-CHECKER / LINTER
;; <https://www.gnu.org/software/emacs/manual/html_mono/flymake.html>

(use-package flymake
  :ensure nil
  ;; Syntax checkers don't work with web-mode, but the other modes
  ;; Install the linter via: `npm install -g stylelint stylelint-config-standard'
  :hook
  ((html-mode css-mode js-mode) . flymake-mode))

;; <https://github.com/purcell/flymake-css>
;; Install the linter via `npm install -g csslint'
(use-package flymake-css
  :ensure t
  :defer t
  :custom
  (flymake-css-lint-command "csslint")
  :hook
  (css-mode . flymake-css-load))

;; <https://github.com/orzechowskid/flymake-eslint>
(use-package flymake-eslint
  :ensure t
  :defer t
  :hook
  (js-mode . flymake-eslint-enable))

;;  ____________________________________________________________________________
(provide 'ontop-webdev)
;;; ontop-webdev.el ends here
