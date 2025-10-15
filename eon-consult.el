;;; eon-consult.el --- Consult -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;;; Code:

;; _____________________________________________________________________________
;;; CONSULT
;; <https://github.com/minad/consult>
;; <https://github.com/minad/consult/wiki>
;; <https://github.com/minad/consult/wiki/Auxiliary-packages>

(use-package consult :ensure t

  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Grep
  (defalias 'eon-consult-grep
    (if (executable-find "rg") #'consult-ripgrep #'consult-grep)
    "Run consult-ripgrep if rg is available, else consult-grep.")
  ;; Find
  (defalias 'eon-consult-find
    (if (executable-find "fd") #'consult-fd #'consult-find)
    "Run consult-fd if fd is available, else consult-find.")

  ;; Localleader label
  (keymap-set eon-localleader-global-map "," #'consult-mode-command)
  (with-eval-after-load 'which-key
    (which-key-add-keymap-based-replacements
      eon-localleader-global-map "," "..."))

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

  :hook
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  (completion-list-mode . consult-preview-at-point-mode)

  :bind
  (;; C-c bindings in `mode-specific-map'
   ;; ("C-c M-x" . consult-mode-command)
   ;; ("C-c h" . consult-history)
   ;; ("C-c k" . consult-kmacro)
   ;; ("C-c m" . consult-man)
   ;; ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)

   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)     ; orig. repeat-complex-command
   ("C-x b"   . consult-buffer)              ; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab)    ; orig. switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark)            ; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ; orig. project-switch-to-buffer

   ;; Custom M-# bindings for fast register access
   ("M-#"   . consult-register-load)
   ("M-'"   . consult-register-store)          ; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)

   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ; orig. yank-pop

   ;; M-g bindings in `goto-map'
   ;; ("M-g e" . consult-compile-error)
   ;; ("M-g r" . consult-grep-match)
   ;; ("M-g f" . consult-flymake)               ; Alternative: consult-flycheck
   ("M-g g"   . consult-goto-line)           ; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ; orig. goto-line
   ;; ("M-g o" . consult-outline)               ; Alternative: consult-org-heading
   ;; ("M-g m" . consult-mark)
   ;; ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ;; ("M-g I" . consult-imenu-multi)

   ;; M-s bindings in `search-map'
   ;; ("M-s d" . consult-find)                  ; Alternative: consult-fd
   ;; ("M-s c" . consult-locate)
   ;; ("M-s G" . consult-git-grep)
   ;; ("M-s l" . consult-line)
   ;; ("M-s L" . consult-line-multi)
   ;; ("M-s k" . consult-keep-lines)
   ;; ("M-s u" . consult-focus-lines)

   ;; Isearch integration
   ;; ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e"   . consult-isearch-history)       ; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ; needed by consult-line to detect isearch

   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ; orig. next-matching-history-element
   ("M-r" . consult-history)                 ; orig. previous-matching-history-element

;;; Leader and localleader keymaps

   ;; Leader
   :map ctl-z-map
   ("SPC" . consult-buffer)
   ("n" . consult-project-buffer)

   ;; Buffer
   :map ctl-z-b-map
   ("b" . consult-buffer)

   ;; Code
   :map ctl-z-c-map
   ("c" . consult-flymake)

   ;; Exec
   :map ctl-z-e-map
   ("k" . consult-kmacro)

   ;; Eshell local leader
   :map eon-localleader-eshell-map
   ("h" . consult-history)

   ;; File
   :map ctl-z-f-map
   ("r" . consult-recent-file)

   ;; Goto
   :map ctl-z-g-map
   ("e" . consult-compile-error)
   ("r" . consult-grep-match)
   ("l" . consult-goto-line)
   ("o" . consult-outline)
   ("m" . consult-mark)
   ("M" . consult-global-mark)
   ("i" . consult-imenu)
   ("I" . consult-imenu-multi)

   ;; Help
   :map ctl-z-h-map
   ("m" . consult-man)
   ("i" . consult-info)

   ;; Org
   :map ctl-z-o-map
   ("a" . consult-org-agenda)

   ;; Org localleader
   :map eon-localleader-org-mode-map
   ("g" . consult-org-heading)

   ;; Search
   :map ctl-z-s-map
   ("f" . eon-consult-find)
   ("g" . eon-consult-grep)
   ("s" . consult-line)
   ("S" . consult-line-multi)

   ;; Tab/Workspace
   :map ctl-z-t-map
   ("b" . consult-buffer-other-tab)

   ;; VC/Git
   :map ctl-z-v-map
   ("g" . consult-git-grep)

   ;; Window
   ;; :map ctl-z-w-map
   ;; ("b" . consult-buffer-other-window)

   ;; Misc
   :map ctl-z-x-map
   ("T" . consult-theme)))

;; _____________________________________________________________________________
;;; CONSULT-DIR

(use-package consult-dir :ensure t
  :bind
  (:map ctl-z-map
        ("d" . consult-dir)))

;; _____________________________________________________________________________
;;; CONSULT-FLYCHECK

(when (eon-modulep 'eon-flycheck)
  (use-package consult-flycheck :ensure t
    :bind
    (:map ctl-z-c-map
          ;; Override `consult-flymake' in the `ctl-z-g-map'
          ("c" . consult-flycheck))))

;; _____________________________________________________________________________
;;; CONSULT-TODO

(use-package consult-todo :ensure t
  :bind
  (:map ctl-z-g-map
        ("t" . consult-todo)
        ("T" . consult-todo-all)))

;;  ____________________________________________________________________________
(provide 'eon-consult)
;;; eon-consult.el ends here
