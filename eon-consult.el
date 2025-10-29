;;; eon-consult.el --- Navigation and search framework -*- lexical-binding: t; no-byte-compile: t; -*-
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

  ;; Provide the "..." label for the global local leader keymap
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

  ;; Let Consult replace the *Completions* buffer and provide code completion
  (setq-default completion-in-region-function #'consult-completion-in-region)

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

   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ; orig. yank-pop

   ;; M-g bindings in `goto-map'
   ;; ("M-g e" . consult-compile-error)
   ;; ("M-g r" . consult-grep-match)
   ;; ("M-g f" . consult-flymake)               ; Alternative: consult-flycheck
   ("M-g g"   . consult-goto-line)           ; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ; orig. goto-line
   ("M-g o" . consult-outline)               ; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g M" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)

   ;; M-s bindings in `search-map'
   ("M-s f" . eon-consult-find)
   ;; ("M-s c" . consult-locate)
   ;; ("M-s G" . consult-git-grep)
   ("M-s s" . consult-line)
   ("M-s S" . consult-line-multi)
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
   ("SPC" . consult-project-buffer)

   ;; Leader -> Buffer
   :map ctl-z-b-map
   ("b" . consult-buffer)

   ;; Leader -> Code
   :map ctl-z-c-map
   ("e" . consult-flymake)

   ;; Leader -> Exec
   :map ctl-z-e-map
   ("k" . consult-kmacro)

   ;; Local leader -> Eshell
   :map eon-localleader-eshell-map
   ("h" . consult-history)

   ;; Leader -> File
   :map ctl-z-f-map
   ("h" . consult-recent-file)

   ;; Leader -> Goto
   :map ctl-z-g-map
   ("e" . consult-compile-error)
   ("r" . consult-grep-match)
   ("L" . consult-goto-line)
   ("o" . consult-outline)
   ("m" . consult-mark)
   ("M" . consult-global-mark)
   ("i" . consult-imenu)
   ("I" . consult-imenu-multi)

   ;; Leader -> Help
   :map ctl-z-h-map
   ("m" . consult-man)
   ("i" . consult-info)

   ;; Leader -> Org
   :map ctl-z-o-map
   ("a" . consult-org-agenda)

   ;; Local leader -> Org
   :map eon-localleader-org-mode-map
   ("g" . consult-org-heading)

   ;; Leader -> Register
   :map ctl-z-r-map
   ;; Unbind redundant built-in commands
   ("i"   . nil)                     ; `insert-register'
   ("R"   . nil)                     ; `copy-rectangle-to-register'
   ("n"   . nil)                     ; `number-to-register'
   ("w"   . nil)                     ; `window-configuration-to-register'
   ("f"   . nil)                     ; `frameset-to-register'
   ("SPC" . nil)                     ; `point-to-register'
   ("j"   . consult-register-load)   ; shadow `jump-to-register'
   ("r"   . consult-register-store)  ; shadow `copy-to-register'
   ("v"   . consult-register)        ; shadow `view-register'

   ;; Leader -> Search
   :map ctl-z-s-map
   ("f" . eon-consult-find)
   ("g" . eon-consult-grep)
   ("s" . consult-line)
   ("S" . consult-line-multi)

   ;; Leader -> Tab
   :map ctl-z-t-map
   ("b" . consult-buffer-other-tab)

   ;; Leader -> VC/Git
   :map ctl-z-v-map
   ("g" . consult-git-grep)

   ;; Leader -> Window
   :map ctl-z-w-map
   ("b" . consult-buffer-other-window)

   ;; Leader -> Misc
   :map ctl-z-x-map
   ("T" . consult-theme)

   ;; Leader -> Bookmark
   :map ctl-z-ret-map
   ("RET" . consult-bookmark)))

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
          ("e" . consult-flycheck))))

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
