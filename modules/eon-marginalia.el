;;; eon-marginalia.el --- Rich annotations -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 1.3.3
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon convenience
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2026 Dan Dee
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; MARGINALIA
;; <https://github.com/minad/marginalia>

;; Enable rich annotations using the Marginalia package
(use-package marginalia :ensure t
  :init
  (marginalia-mode)
  :bind
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

;; _____________________________________________________________________________
(provide 'eon-marginalia)
;;; eon-marginalia.el ends here
