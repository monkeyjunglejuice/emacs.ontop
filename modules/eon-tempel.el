;;; eon-tempel.el --- Code snippets -*- lexical-binding: t; no-byte-compile: t; -*-

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
;;; - TEMPEL
;; The templates are defined in a Lisp data file configured by `tempel-path'.
;; Lisp data files are files containing Lisp s-expressions (see
;; `lisp-data-mode'). By default the file templates in the
;; `user-emacs-directory' is used, e.g., ~/.config/emacs/templates. The
;; templates are grouped by major mode with an optional :when condition. Each
;; template is a list in the concise form of the Emacs Tempo syntax. The first
;; element of each list is the name of the template. I recommend to avoid
;; special letters for the template names, since special letters may carry
;; meaning during completion filtering and as such make it harder to select the
;; desired template. Thus the name lett is better than let*. Behind the name,
;; the Tempo syntax elements follow.
;; 
;; In addition, after the template elements, each template may specify several
;; key/value pairs. Specifically, templates may specify :pre and/or :post keys
;; with a FORM that is evaluated before the template is expanded or after it is
;; finished, respectively. The :post form is evaluated in the lexical scope of
;; the template, which means that it can access the template’s named fields.
;; Beyond that, templates may include an :ann and :doc key with strings that are
;; used as annotation and documentation respectively.
;; Website: <https://github.com/minad/tempel>
;;
;;; - TEMPEL COLLECTION
;; Collection of the templates to Emacs Tempel package. Template files are
;; placed in the directory templates as Lisp data <mode>.eld files.
;; Website: <https://github.com/Crandel/tempel-collection>
;;
;;; - EGLOT INTEGRATION
;; Simple shim adapter to use the tempel templating library with eglot, instead
;; of yasnippet.
;; Website: <https://github.com/fejfighter/eglot-tempel>
;; 
;;; Code:

(eon-module-metadata
 :conflicts '(eon-yasnippet)
 :requires  '(eon))

;; _____________________________________________________________________________
;;; TEMPEL
;; <https://github.com/minad/tempel>

(use-package tempel

  :init

  ;; Setup completion at point
  (defun eon-tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand'
    ;; only triggers on exact matches. We add `tempel-expand' *before* the main
    ;; programming mode Capf, such that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions))
    ;; Alternatively use `tempel-complete' if you want to see all matches. Use
    ;; a trigger prefix character in order to prevent Tempel from triggering
    ;; unexpectly.
    ;; (setq-local corfu-auto-trigger "/"
    ;;             completion-at-point-functions
    ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
    ;;                   completion-at-point-functions))
    )

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)

  :hook
  ((prog-mode conf-mode text-mode) . eon-tempel-setup-capf)

  :bind (("M-+" . tempel-complete) ; alternative `tempel-expand'
         ("M-*" . tempel-insert)))

;; _____________________________________________________________________________
;; TEMPEL COLLECTION
;; <https://github.com/Crandel/tempel-collection>

(use-package tempel-collection :ensure t
  :after tempel)

;; _____________________________________________________________________________
;; EGLOT INTEGRATION
;; <https://github.com/fejfighter/eglot-tempel>

(use-package eglot-tempel :ensure t
  :after eglot
  :init
  (eglot-tempel-mode))

;; _____________________________________________________________________________
(provide 'eon-tempel)
;;; eon-tempel.el ends here
