;;; eon-reader.el --- Major mode for reading EPUBs in Emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.1
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience epub ebook
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;; Features
;; - Basic navigation (jump to TOC, previous/next chapter)
;; - Remembering and restoring the last read position
;; - Jump to next chapter when scrolling beyond end
;; - Storing and following Org links to EPUB files
;; - Renders EPUB2 (.ncx) and EPUB3 (<nav>) TOCs
;; - Hyperlinks to internal and external targets
;; - Supports textual and image documents
;; - Info-style history navigation
;; - View source of document files
;; - Info-style incremental search
;; - Metadata display
;; - Image rescaling
;;
;; Invalid EPUB documents are not supported. Please use epubcheck to validate
;; yours when running into an error.
;;
;; To change the default font, use M-x customize-face RET variable-pitch, pick a
;; different family, save and apply. If you dislike globally customizing that
;; face, add the following to your init file:
;; 
;;   (defun my-nov-font-setup ()
;;     (face-remap-add-relative 'shr-text :family "Liberation Serif"
;;                              :height 1.0))
;;   (add-hook 'nov-mode-hook 'my-nov-font-setup)
;;     
;; To completely disable the variable pitch font, customize nov-variable-pitch
;; to nil. Text will be displayed with the default face instead which should be
;; using a monospace font. Text width By default text is filled by the window
;; width. You can customize nov-text-width to a number of columns to change
;; that: (setq nov-text-width 80) It's also possible to set it to t to inhibit
;; text filling, this can be used in combination with visual-line-mode and
;; packages such as visual-fill-column to implement more flexible filling:
;; 
;;   (setq nov-text-width t)
;;   (setq visual-fill-column-center-text t)
;;   (add-hook 'nov-mode-hook 'visual-line-mode)
;;   (add-hook 'nov-mode-hook 'visual-fill-column-mode)
;; 
;; In case you're not happy with the rendering at all, you can either use
;; nov-pre-html-render-hook and nov-post-html-render-hook to adjust the HTML
;; before and after rendering or use your own rendering function by customizing
;; nov-render-html-function to one that replaces HTML in a buffer with something
;; nicer than the default output. Here's an advanced example of text
;; justification with the justify-kp package:
;; 
;;   (require 'justify-kp)
;;   (setopt nov-text-width t)
;;   
;;   (defun my-nov-window-configuration-change-hook ()
;;   (my-nov-post-html-render-hook)
;;   (remove-hook 'window-configuration-change-hook
;;                'my-nov-window-configuration-change-hook
;;                t))
;; 
;;   (defun my-nov-post-html-render-hook ()
;;     (if (get-buffer-window)
;;         (let ((max-width (pj-line-width))
;;               buffer-read-only)
;;           (save-excursion
;;             (goto-char (point-min))
;;             (while (not (eobp))
;;               (when (not (looking-at "^[[:space:]]*$"))
;;                 (goto-char (line-end-position))
;;                 (when (> (shr-pixel-column) max-width)
;;                   (goto-char (line-beginning-position))
;;                   (pj-justify)))
;;               (forward-line 1))))
;;       (add-hook 'window-configuration-change-hook
;;                 'my-nov-window-configuration-change-hook
;;                 nil t)))
;;   
;;   (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)
;;
;; Website: <https://depp.brause.cc/nov.el/>
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; NOV.EL

(use-package nov :ensure t
  :mode ("\\.epub\\'" . nov-mode))

;; _____________________________________________________________________________
(provide 'eon-reader)
;;; eon-reader.el ends here
