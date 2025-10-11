;;; eon-marginalia.el --- Marginalia -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;;;  ____________________________________________________________________________
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

;;  ____________________________________________________________________________
(provide 'eon-marginalia)
;;; eon-marginalia.el ends here
