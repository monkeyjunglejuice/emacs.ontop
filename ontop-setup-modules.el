;;; ontop-setup-modules.el --- Select your modules  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; Emacs ONTOP is an extension on top of the Emacs ONBOARD starter-kit.
;; `ontop-onboard.el' is already included and loaded by `ontop.el'.

;;; Code:

;;  ____________________________________________________________________________
;;; SETUP

;; --> Enable or disable modules below via commenting and uncommenting.
;;     You can also load modules on the fly via "M-x load-library RET ontop-"
;;     and unload manually loaded modules via "M-x unload-feature RET ontop-"

(setq eon-modules
      '(;; To learn more, visit modules: place the cursor and do `M-x ffap RET'
        onboard                         ; include the Emacs ONBOARD starter-kit
        ontop-core                      ; shared settings and definitions
        ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        ;; ontop-commonlisp                ; Common Lisp IDE
        ;; ontop-company                   ; code autocomplete alternative
        ontop-consult                   ; search and navigation
        ontop-corfu                     ; code autocomplete alternative
        ontop-elixir                    ; Elixir IDE
        ;; ontop-evil                      ; modular editing, VIM keybindings
        ;; ontop-flycheck                  ; syntax checker alternative
        ontop-fonts                     ; curated font sets
        ;; ontop-godmode                   ; modular editing, Emacs keybindings
        ;; ontop-haskell                   ; Haskell IDE
        ontop-helpful                   ; Extended help viewer
        ;; ontop-julia                     ; Julia IDE
        ;; ontop-lua                       ; Lua IDE
        ;; ontop-ocaml                     ; Ocaml IDE
        ;; ontop-pdftools                  ; PDF reader
        ;; ontop-racket                    ; Racket IDE
        ;; ontop-scheme                    ; Scheme IDE
        ontop-vertico                   ; vertical completion
        ontop-webdev                    ; HTML/CSS/JS support

        ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ;; Below is an empty extra module that can run your personal Elisp code.
        ;; Your code will load last, so that you can override or shadow
        ;; any settings that have been defined within any of the files before.
        ontop-setup-personal  ; ---> edit the file `ontop-setup-personal.el'
        ))

;;  ____________________________________________________________________________
(provide 'ontop-setup-modules)
;;; ontop-setup-modules.el ends here
