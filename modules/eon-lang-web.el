;;; eon-lang-web.el --- HTML/CSS editing -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience languages html css
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; WEB-MODE
;; <https://web-mode.org>

;; (use-package web-mode :ensure t
;;   :defer t
;;   :custom
;;   (web-mode-attr-indent-offset 2)
;;   (web-mode-attr-value-indent-offset 2)
;;   (web-mode-code-indent-offset 2)
;;   (web-mode-css-indent-offset 2)
;;   (web-mode-markup-indent-offset 2)
;;   (web-mode-enable-auto-expanding t)
;;   (web-mode-enable-block-face t)
;;   ;; TODO styling required for web-mode-current-element-highlight-face
;;   (web-mode-enable-current-element-highlight t)
;;   ':mode
;;   ;; Associate Web-mode with the following file types?
;;   '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . web-mode)
;;   '("\\.css\\'" . web-mode)
;;   :bind
;;   (:map web-mode-map
;;         ("C-M-f" . web-mode-forward-sexp)
;;         ("C-M-b" . web-mode-backward-sexp)))

;; _____________________________________________________________________________
;;; HTML
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#HTML-Mode>

(use-package html-ts-mode :ensure nil
  :defer t
  :init
  (eon-treesitter-ensure-grammar
   '(html "https://github.com/tree-sitter/tree-sitter-html")))

;; _____________________________________________________________________________
;;; CSS

(use-package css-ts-mode :ensure nil
  :defer t
  :init
  (eon-treesitter-ensure-grammar
   '(css "https://github.com/tree-sitter/tree-sitter-css"))
  :custom
  (css-indent-offset 2))

;; _____________________________________________________________________________
;;; JS

(use-package js-ts-mode :ensure nil
  :defer t
  :init
  (eon-treesitter-ensure-grammar
   '(javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                "master" "src")))

;; _____________________________________________________________________________
;;; FLYMAKE SYNTAX-CHECKER / LINTER

(unless (eon-modulep 'eon-flycheck)
  ;; <https://www.gnu.org/software/emacs/manual/html_mono/flymake.html>
  (use-package flymake :ensure nil
    ;; Syntax checkers don't work with web-mode, but the other modes. Install the
    ;; linter via: `npm install -g stylelint stylelint-config-standard'
    :hook
    (( html-mode html-ts-mode
       css-mode css-ts-mode
       js-mode js-ts-mode) . flymake-mode))

  ;; <https://github.com/purcell/flymake-css>
  ;; Install the linter via `npm install -g csslint'
  (use-package flymake-css :ensure t
    :defer t
    :custom
    (flymake-css-lint-command "csslint")
    :hook
    ((css-mode css-ts-mode) . flymake-css-load))

  ;; <https://github.com/orzechowskid/flymake-eslint>
  (use-package flymake-eslint :ensure t
    :defer t
    :hook
    ((js-mode js-ts-mode) . flymake-eslint-enable)))

;; _____________________________________________________________________________
;;; AUTO-INDENTATION

;; Enable `aggressive-indent-mode' per major mode
(when (eon-modulep 'eon-indent)
  (use-package aggressive-indent :ensure t
    :hook
    ((css-ts-mode js-ts-mode) . aggressive-indent-mode)))

;; _____________________________________________________________________________
(provide 'eon-lang-web)
;;; eon-lang-web.el ends here
