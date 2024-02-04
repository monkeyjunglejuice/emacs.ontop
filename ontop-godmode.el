;;; ontop-godmode.el --- Flycheck configuration  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-godmode.el")'.

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
;;; GOD MODE
;; <https://github.com/emacsorphanage/god-mode>

(use-package god-mode
  :ensure t
  :custom
  (god-mode-enable-function-key-translation nil)
  :config
  ;; Set the default cursor type
  (add-to-list 'default-frame-alist '(cursor-type . (bar . 2)))
  (defun god-mode-update-cursor-type ()
    "Turn on cursor blinking by default, but turn off for god-mode cursor"
    (if (or god-local-mode buffer-read-only)
        (progn (setq cursor-type 'box) (blink-cursor-mode -1))
      (progn (setq cursor-type 'bar) (blink-cursor-mode 1))))
  (add-hook 'post-command-hook #'god-mode-update-cursor-type)
  :bind
  ("<escape>" . #'god-local-mode)
  (:map god-local-mode-map
        ("i" . #'god-local-mode)))

;; God mode behaviour in Isearch
(use-package god-mode-isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ("<escape>" . #'god-mode-isearch-activate))
  (:map god-mode-isearch-map
        ("<escape>" . #'god-mode-isearch-disable)))

(use-package repeat
  :ensure nil
  :config
  ;; Not "too dangerous", just disable toggling god-mode via `repeat' command
  (add-to-list 'repeat-too-dangerous #'god-local-mode)
  (add-to-list 'repeat-too-dangerous #'god-mode-all)
  :bind
  (:map god-local-mode-map
        ("." . #'repeat)))

;;  ____________________________________________________________________________
(provide 'ontop-godmode)
;;; ontop-godmode.el ends here
