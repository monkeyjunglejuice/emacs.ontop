;;; eon-lang-ocaml.el --- OCaml -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; The aim is to provide a comprehensive OCaml setup.
;; Using this module, you will not need to load `opam-user-setup.el'
;; in Emacs, because the functionality is already included here.
;; It's nevertheless a good idea to install "user-setup" package
;; via "opam install user-setup" or `opam-install-dev-packages',
;; because some of its dependencies are quite useful.
;;
;;; Code:

;; _____________________________________________________________________________
;;; TUAREG

(use-package tuareg :ensure t
  :custom
  (tuareg-electric-indent t)
  (tuareg-display-buffer-on-eval t)
  (tuareg-skip-after-eval-phrase nil)
  ;; (tuareg-interactive-program "ocaml -nopromptcont")
  (tuareg-interactive-echo-phrase t)
  (tuareg-interactive-read-only-input t)
  (tuareg-interactive-scroll-to-bottom-on-output t)
  :hook
  (tuareg-mode . (lambda ()
                   (setq-local compile-command "dune build")))
  (tuareg-interactive-mode . (lambda ()
                               (set-process-query-on-exit-flag
                                (get-process "OCaml") nil)))
  :bind
  (:map tuareg-mode-map
        ("C-c C-e" . tuareg-eval-phrase)
        ("C-c C-r" . tuareg-eval-region)
        ("C-c C-b" . tuareg-eval-buffer)))

;; _____________________________________________________________________________
;;; MERLIN

(use-package merlin :ensure t
  :custom
  (merlin-completion-with-doc t)
  (merlin-report-errors-in-lighter t)
  (merlin-occurrences-show-buffer 'same)
  :config
  ;; Don't show *merlin* buffer in buffers list
  (when (boundp 'eon-boring-buffers)
    (add-to-list 'eon-boring-buffers "\\`\\*merlin"))
  :hook
  ((tuareg-mode tuareg-interactive-mode) . merlin-mode)
  (tuareg-mode . merlin-use-merlin-imenu)
  :bind
  (:map merlin-mode-map
        ("C-M-p"      . merlin-phrase-prev)
        ("C-M-n"      . merlin-phrase-next)
        ("C-c C-t"    . merlin-type-enclosing)
        ("C-c <up>"   . merlin-type-enclosing-go-up)
        ("C-c <down>" . merlin-type-enclosing-go-down)
        ("C-c C-d"    . merlin-document)
        ("C-c d"      . merlin-destruct)
        ("C-c e e"    . merlin-error-check)
        ("C-c e r"    . merlin-error-reset)
        ("C-c e t"    . merlin-toggle-view-errors)
        ("M-g p"      . merlin-error-prev)
        ("M-g n"      . merlin-error-next)))

;; _____________________________________________________________________________
;;; MERLIN-ELDOC
;; <https://github.com/Khady/merlin-eldoc>

(use-package merlin-eldoc :ensure t
  :custom
  (merlin-eldoc-max-lines 3)             ; but not more than 3
  (merlin-eldoc-type-verbosity 'min)     ; don't display verbose types
  (merlin-eldoc-function-arguments nil)  ; don't show function arguments
  (merlin-eldoc-doc nil)                 ; don't show the documentation
  :hook
  (tuareg-mode . merlin-eldoc-setup)
  :bind
  (:map tuareg-mode-map
        ("C-M-b" . merlin-eldoc-jump-to-prev-occurrence)
        ("C-M-f" . merlin-eldoc-jump-to-next-occurrence)))

;; _____________________________________________________________________________
;;; STANDARD REPL (OCAML)

;; Run the standard OCaml toplevel via `tuareg-run-ocaml' "C-c C-s".
;; It’s possible to integrate any toplevel with Dune projects:
;; 1. Navigate to your Dune project
;; 2. "M-x opam-update-env" and choose the Opam switch for your project
;; 3. Execute in the toplevel: #use_output "dune ocaml top";;
;;    (or put this into a project-specific .ocamlinit file)
;; <https://dune.readthedocs.io/en/stable/toplevel-integration.html>

;; "C-c C-s": `tuareg-run-ocaml'
;; "C-c C-z": `tuareg-switch-to-repl'
;; "C-c C-e": `tuareg-eval-phrase'
;; "C-c C-r": `tuareg-eval-region'
;; "C-c C-b": `tuareg-eval-buffer'

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ALTERNATIVE REPL (UTOP)

;; Alternatively, there's Utop that also intergrates with Dune projects.
;; <https://github.com/ocaml-community/utop>
;; <https://dune.readthedocs.io/en/stable/usage.html#launching-the-toplevel-repl>

;; Even though Utop works great in the shell, it works not so well in Emacs.
;; Right now there are some issues with autocompletion via `company-mode'.

;; If Utop dies at launch, try to fix it by updating the Opam environment
;; within your Dune project, either via "M-x opam-update-env" (recommended)
;; or "M-! eval $(opam env)".

;; The keybindings are the same as with the standard toplevel.

(use-package utop :ensure t
  :custom
  ;; Utop will start with Dune project awareness:
  (utop-command "opam exec -- dune utop . -- -emacs")
  :hook
  (tuareg-mode . utop-minor-mode)
  ;; HACK Workaround to get completions in Utop from Merlin
  (utop-mode . (lambda ()
                 (merlin-mode -1)
                 (merlin-mode +1))))

;; _____________________________________________________________________________
;;; INDENTATION
;; <https://github.com/OCamlPro/ocp-indent>

(use-package ocp-indent :ensure t
  :after merlin-mode
  :hook
  (tuareg-mode . ocp-setup-indent))

;; _____________________________________________________________________________
;;; FORMATTING
;; <https://github.com/ocaml-ppx/ocamlformat>

(use-package ocamlformat :ensure t
  :after merlin-mode
  :hook
  (tuareg-mode . ocamlformat-setup-indent))

;; _____________________________________________________________________________
;;; DUNE

;; Major mode for Dune files
(use-package dune :ensure t
  :defer t)

;; _____________________________________________________________________________
;;; OPAM

;; Use the command "M-x opam-switch-set-switch" to select the opam switch for
;; your OCaml project
(use-package opam-switch-mode :ensure t
  :after tuareg
  :config
  (setq tuareg-opam-insinuate t)
  (opam-switch-set-switch (tuareg-opam-current-compiler))
  ;; Install basic package selection via Opam
  (defun opam-switch-install-packages ()
    "Install standard package selection for editor support and development."
    (interactive)
    (message (concat "Opam: installing packages for switch: "
                     (opam-switch--get-current-switch)))
    (start-process
     "opam install"       ; Emacs process name
     "*opam install*"     ; Emacs output buffer
     "opam" "install"     ; shell command
     "--yes"              ; answer "yes" to all questions
     ;; Package selection
     "dune"
     "dune-release"
     "merlin"
     "tuareg"
     "ocaml-lsp-server"
     "ocamlformat"
     "utop"
     "odoc"
     "omod"
     "domainslib"
     )))

;; _____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate OCaml code in Org source code blocks via "C-c C-c"

(use-package ob-ocaml :ensure nil
  :custom
  (org-babel-ocaml-command "ocaml -nopromptcont"))

(use-package org :ensure nil
  :config
  (add-to-list 'org-babel-load-languages '(ocaml . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

;; _____________________________________________________________________________
(provide 'eon-lang-ocaml)
;;; eon-lang-ocaml.el ends here
