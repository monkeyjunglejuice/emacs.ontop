;;; eon-ocaml.el --- OCaml setup  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/eon-ocaml.el")'
;;
;; The aim of Emacs ONTOP is to provide a comprehensive OCaml setup.
;; Using this module, you will not need to load `opam-user-setup.el'
;; in Emacs, because the functionality is already included here.
;; It's nevertheless a good idea to install "user-setup" package
;; via "opam install user-setup" or `opam-install-dev-packages',
;; because some of its dependencies are quite useful.

;;; Code:

;;  ____________________________________________________________________________
;;; TUAREG

(use-package tuareg
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

;;  ____________________________________________________________________________
;;; MERLIN

(use-package merlin
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

;;  ____________________________________________________________________________
;;; MERLIN-ELDOC
;; <https://github.com/Khady/merlin-eldoc>

(use-package merlin-eldoc
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

;;  ____________________________________________________________________________
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

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
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

(use-package utop
  :custom
  ;; Utop will start with Dune project awareness:
  (utop-command "opam exec -- dune utop . -- -emacs")
  :hook
  (tuareg-mode . utop-minor-mode)
  ;; HACK Workaround to get completions in Utop from Merlin
  (utop-mode . (lambda ()
                 (merlin-mode -1)
                 (merlin-mode +1))))

;;  ____________________________________________________________________________
;;; INDENTATION
;; <https://github.com/OCamlPro/ocp-indent>

(use-package ocp-indent
  :after merlin-mode
  :hook
  (tuareg-mode . ocp-setup-indent))

;;  ____________________________________________________________________________
;;; FORMATTING
;; <https://github.com/ocaml-ppx/ocamlformat>

(use-package ocamlformat
  :after merlin-mode
  :hook
  (tuareg-mode . ocamlformat-setup-indent))

;;  ____________________________________________________________________________
;;; DUNE

;; Major mode for Dune files
(use-package dune
  :defer t)

;;  ____________________________________________________________________________
;;; OPAM

;; Use the command "M-x opam-update-env" to select the opam switch for your
;; OCaml project (stolen from Opam's user-setup).

(defun opam-shell-command-to-string (command)
  "Similar to shell-command-to-string, but returns nil unless the process
 returned 0 and ignores stderr (shell-command-to-string ignores return value)."
  (let* ((return-value 0)
         (return-string
          (with-output-to-string
            (setq return-value
                  (with-current-buffer standard-output
                    (process-file shell-file-name nil '(t nil) nil
                                  shell-command-switch command))))))
    (if (= return-value 0) return-string nil)))

(defun opam-update-env (switch)
  "Update the environment to follow current Opam SWITCH configuration."
  (interactive
   (list
    (let ((default
           (car (split-string (opam-shell-command-to-string "opam switch show --safe")))))
      (completing-read
       (concat "opam switch (" default "): ")
       (split-string (opam-shell-command-to-string "opam switch list -s --safe") "\n")
       nil t nil nil default))))
  (let* ((switch-arg (if (= 0 (length switch)) "" (concat "--switch " switch)))
         (command (concat "opam config env --safe --sexp " switch-arg))
         (env (opam-shell-command-to-string command)))
    (when (and env (not (string= env "")))
      (dolist (var (car (read-from-string env)))
        (setenv (car var) (cadr var))
        (when (string= (car var) "PATH")
          (setq exec-path (split-string (cadr var) path-separator)))))))

;; Install the basic package selection for convenience
(defun opam-install-basic-packages ()
  "Install standard package selection for editor support and development."
  (interactive)
  (message (concat "Installing basic packages for switch: "
                   (opam-switch--get-current-switch)))
  (start-process
   "opam-install-basic-packages"        ; Emacs process name
   "*opam-install*"                     ; Emacs output buffer
   "opam" "install"                     ; shell command
   "--yes"                              ; answer "yes" to all questions
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
   ))

;;  ____________________________________________________________________________
;;; PARENTHESIS DISPLAY

;; Rainbow-delimiters color-coding of nested parens is already enabled
;; for all prog-modes in `eon-core.el'
(use-package rainbow-delimiters
  :hook
  (tuareg-interactive-mode . rainbow-delimiters-mode))

;; Make parens styleable, e.g. more or less prominent
;; <https://github.com/tarsius/paren-face>
;; (use-package paren-face
;;   :hook
;;   ((tuareg-mode tuareg-interactive-mode) . paren-face-mode))

;;  ____________________________________________________________________________
;;; ORG-MODE BABEL
;; <https://orgmode.org/worg/org-contrib/babel/index.html>
;; Notebook-like literate programming in Emacs
;; Evaluate OCaml code in Org source code blocks via "C-c C-c"

(use-package ob-ocaml
  :ensure nil
  :custom
  (org-babel-ocaml-command "ocaml -nopromptcont"))

(use-package org
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 (add-to-list 'org-babel-load-languages '(ocaml . t))))))

;;  ____________________________________________________________________________
(provide 'eon-ocaml)
;;; eon-ocaml.el ends here
