;;; eon-fonts.el --- Font configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-fonts.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; FONTS

(defun eon-fonts-fira ()
  "Set the Fira fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Fira Code"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150)
  (set-face-attribute 'fixed-pitch nil
                      :family "Fira Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Fira Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Fira Sans"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Fira Code"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Fira Code"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

(defun eon-fonts-iosevka ()
  "Set the Iosevka fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Iosevka"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 160)
  (set-face-attribute 'fixed-pitch nil
                      :family "Iosevka"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Iosevka Slab"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Iosevka Etoile"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Iosevka"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Iosevka"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

(defun eon-fonts-recursive ()
  "Recursive fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Recursive Mono Linear Static"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150)
  (set-face-attribute 'fixed-pitch nil
                      :family "Recursive Mono Linear Static"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Recursive Mono Casual Static"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Recursive Sans Linear Static Regular"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 10)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Recursive Mono Linear Static"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Recursive Mono Linear Static"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-fantasque ()
  "Fantasque fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Fantasque Sans Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 160)
  (set-face-attribute 'fixed-pitch nil
                      :family "Fantasque Sans Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Fantasque Sans Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "EB Garamond"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 170)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Fantasque Sans Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Fantasque Sans Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-courier-prime ()
  "Courier Prime fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Courier Prime"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 160)
  (set-face-attribute 'fixed-pitch nil
                      :family "Courier Prime"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Courier Prime"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "EB Garamond"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 160)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Courier Prime"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Courier Prime"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-hack ()
  "Hack fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Hack"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150)
  (set-face-attribute 'fixed-pitch nil
                      :family "Hack"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Hack"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "DejaVu Serif"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 140)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Hack"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Hack"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-spline ()
  "Spline Sans Mono fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Spline Sans Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150)
  (set-face-attribute 'fixed-pitch nil
                      :family "Spline Sans Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Spline Sans Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Spline Sans"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 130)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Spline Sans Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Spline Sans Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-mononoki ()
  "Mononoki fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Mononoki"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 160)
  (set-face-attribute 'fixed-pitch nil
                      :family "Mononoki"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Mononoki"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Lora"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 140)

  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Mononoki"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Mononoki"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-source-code-pro ()
  "Source Code Pro fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :slant  'normal
                      :weight 'medium
                      :width  'normal
                      :height 160)
  (set-face-attribute 'fixed-pitch nil
                      :family "Source Code Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Source Code Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Source Serif Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Source Code Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Source Code Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-plex ()
  "IBM Plex fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "IBM Plex Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150)
  (set-face-attribute 'fixed-pitch nil
                      :family "IBM Plex Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "IBM Plex Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "IBM Plex Sans"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 132)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "IBM Plex Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "IBM Plex Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-average ()
  "Average Mono fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Average Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 160)
  (set-face-attribute 'fixed-pitch nil
                      :family "Average Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Average Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Average"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 160)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Average Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Average Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-anonymous-pro ()
  "Anonymous Pro fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Anonymous Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 170)
  (set-face-attribute 'fixed-pitch nil
                      :family "Anonymous Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Anonymous Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Anonymous Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 160)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Anonymous Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Anonymous Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.7))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-ubuntu ()
  "Ubuntu Mono fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Ubuntu Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 160)
  (set-face-attribute 'fixed-pitch nil
                      :family "Ubuntu Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Ubuntu Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Ubuntu Sans"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Ubuntu Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Ubuntu Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-azeret-mono ()
  "Azeret Mono fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Azeret Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150)
  (set-face-attribute 'fixed-pitch nil
                      :family "Azeret Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Azeret Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Azeret Sans"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Azeret Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Azeret Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun eon-fonts-monoid ()
  "Monoid fonts."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Monoid"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 130)
  (set-face-attribute 'fixed-pitch nil
                      :family "Monoid"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Monoid"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Monoid"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 130)
  ;; Modeline
  (set-face-attribute 'mode-line nil
                      :family "Monoid"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8)
  (set-face-attribute 'mode-line-inactive nil
                      :family "Monoid"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;;  ____________________________________________________________________________
(provide 'eon-fonts)
;;; eon-fonts.el ends here
