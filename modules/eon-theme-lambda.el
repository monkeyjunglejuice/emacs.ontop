;;; eon-theme-lambda.el --- Theme set with limited color palette -*- lexical-binding: t; no-byte-compile: t; -*-

;; Version: 2.0.0
;; URL: https://github.com/monkeyjunglejuice/emacs.ontop
;; Package-Requires: ((emacs "30.1")
;;                    (use-package "2.4.6"))
;; Keywords: eon config convenience themes
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2021-2026 Dan Dee

;;; Commentary:
;;
;; Lambda-themes is a collection of four higher and lower contrast light and
;; dark themes. The color palette is limited, and organized according to
;; function and salience. In general the theme aims to use as few highly
;; distinct colors as possible without crossing over into full "monochrome"
;; territory. It also means that the themes use various devices other than
;; foreground face color to capture meaningful differences in text. Different
;; text weights are used throughout, as are subtle differences in background
;; coloring. Colored headlines are largely avoided.
;;
;; Lambda comes in 8 flavors:
;; 'lambda-light
;; 'lambda-light-faded
;; 'lambda-dark
;; 'lambda-dark-faded
;;
;; Example how to set the theme flavors in your init.el:
;;
;; (setopt eon-theme-light 'lambda-light
;;         eon-theme-dark 'lambda-dark-faded
;;         eon-theme-variant-default 'light)
;; 
;; (eon-theme-load-default)
;;
;; Website: <https://codeberg.org/Lambda-Emacs/lambda-themes>
;;
;;; Code:

(eon-module-metadata
 :conflicts '()
 :requires  '(eon))

;; _____________________________________________________________________________
;;; LAMBDA THEMES
;; <https://codeberg.org/Lambda-Emacs/lambda-themes>

(use-package lambda-themes
  :vc (:url "https://codeberg.org/Lambda-Emacs/lambda-themes.git"
            :rev :newest)
  :custom
  (lambda-themes-set-vibrant nil)
  (lambda-themes-set-variable-pitch nil)
  (lambda-themes-set-italic-comments nil)
  (lambda-themes-set-italic-keywords t))

;; _____________________________________________________________________________
(provide 'eon-theme-lambda)
;;; eon-theme-lambda.el ends here
