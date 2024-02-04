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
  :ensure nil)

;;  ____________________________________________________________________________
;;; CSS-MODE

(use-package css-mode
  :ensure nil
  :custom
  (css-indent-offset 2))

;;  ____________________________________________________________________________
;;; JS-MODE

(use-package js
  :ensure nil)

;;  ____________________________________________________________________________
;;; SEMANTIC
;; <https://www.gnu.org/software/emacs/manual/html_mono/semantic.html>

(use-package semantic
  :ensure nil
  :hook
  ((html-mode js-mode) . semantic-mode))

;;  ____________________________________________________________________________
;;; INDENTATION
;; <https://github.com/Malabarba/aggressive-indent-mode>

(use-package aggressive-indent
  :ensure t
  :hook
  ((css-mode js-mode) . aggressive-indent-mode))

;;  ____________________________________________________________________________
;;; SYNTAX-CHECKER / LINTER
;; <https://www.gnu.org/software/emacs/manual/html_mono/flymake.html>

(use-package flymake
  :ensure nil
  ;; Syntax checkers don't work with web-mode, but the other modes
  :hook
  ((html-mode css-mode js-mode) . flymake-mode))

;; <https://github.com/purcell/flymake-css>
(use-package flymake-css
  :hook
  ((css-mode web-mode) . flymake-css-load))

;; <https://github.com/orzechowskid/flymake-eslint>
(use-package flymake-eslint
  :hook
  ((js-mode web-mode) . flymake-eslint-enable))

;;  ____________________________________________________________________________
(provide 'ontop-webdev)
;;; ontop-webdev.el ends here
