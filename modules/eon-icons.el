;;; eon-icons.el --- Icons everywhere -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; Please go to Nerd Fonts website and get a Nerd font. After installing the
;; font on your machine, you need to set nerd-icons-font-family to match its
;; font name so that the font can be properly used under GUI. If you want Nerd
;; Fonts to work under a terminal, please change your terminal’s font to a Nerd
;; font.
;;
;; If the Nerd Font you installed does not display correctly (e.g. appear
;; cut off), it is recommended to use Symbols Nerd Font Mono (Symbols Nerd
;; Font).
;;
;; You can use "M-x nerd-icons-install-fonts" to install Symbols Nerd Font
;; Mono for you. Note that for Windows you’ll need to manually install the font
;; after you used this function.
;;
;;; Code:

;; _____________________________________________________________________________
;;; NERD ICONS

;; <https://github.com/rainstormstudio/nerd-icons.el>
(use-package nerd-icons :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended,
  ;; but you can use any other Nerd Font if you want.
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Dired
;; <https://github.com/rainstormstudio/nerd-icons-dired>
(use-package nerd-icons-dired :ensure t
  :after dired
  :diminish
  :hook
  (dired-mode . nerd-icons-dired-mode)
  ;; Display icons for subtrees
  (dired-subtree-after-insert . nerd-icons-dired--refresh))

;; Ibuffer
;; <https://github.com/seagle0128/nerd-icons-ibuffer>
(use-package nerd-icons-ibuffer :ensure t
  :after ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Completion
;; <https://github.com/rainstormstudio/nerd-icons-completion>
(use-package nerd-icons-completion :ensure t
  :config
  (nerd-icons-completion-mode))

;; Marginalia
;; <https://github.com/rainstormstudio/nerd-icons-completion>
(when (eon-modulep 'eon-marginalia)
  (use-package nerd-icons-completion :ensure t
    :hook
    (marginalia-mode #'nerd-icons-completion-marginalia-setup)))

;; Corfu
;; <https://github.com/LuigiPiucco/nerd-icons-corfu>
(when (eon-modulep 'eon-corfu)
  (use-package nerd-icons-corfu :ensure t
    :config
    (eon-add-to-list* 'corfu-margin-formatters
                      #'nerd-icons-corfu-formatter)))

;; Treemacs
;; <https://github.com/rainstormstudio/treemacs-nerd-icons>
(when (eon-modulep 'eon-treemacs)
  (use-package treemacs-nerd-icons :ensure t
    :config
    (treemacs-nerd-icons-config)))

;; _____________________________________________________________________________
(provide 'eon-icons)
;;; eon-icons.el ends here
