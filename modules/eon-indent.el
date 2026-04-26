;;; eon-indent.el --- Indent code immediately -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience
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
;; AGGRESSIVE INDENT
;; <https://github.com/Malabarba/aggressive-indent-mode>

;; TODO Aggressive-indent causes weird issues, either fix them or switch
;; to another package, e.g. <https://github.com/jeffvalk/snap-indent>

;; Aggressive indent shouldn't be enabled globally via
;; `aggressive-indent-global-mode' or via hook for `prog-mode', but individually
;; per major mode.

(use-package aggressive-indent :ensure t
  :diminish aggressive-indent-mode

  :custom

  ;; Actually seems to work better when `electric-indent-mode' is enabled
  (aggressive-indent-dont-electric-modes nil)
  (aggressive-indent-sit-for-time 0.01)

  :hook

  (emacs-lisp-mode . aggressive-indent-mode))

;; _____________________________________________________________________________
(provide 'eon-indent)
;;; eon-indent.el ends here
