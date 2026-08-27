;;; eon-scratch.el --- Scratch buffer enhancements -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience tools
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
;;; PERSISTENT SCRATCH BUFFER
;; <https://github.com/Fanael/persistent-scratch>

(use-package persistent-scratch :ensure t
  :config
  ;; Both enable autosave and restore the last saved state, if any.
  (persistent-scratch-setup-default))

;; _____________________________________________________________________________
(provide 'eon-scratch)
;;; eon-scratch.el ends here
