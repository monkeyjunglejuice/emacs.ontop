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
;;     
;; --> Documentation lives in the module files. To learn more, place the cursor
;;     on a module entry below and do `M-x ffap RET' to visit the file.

(setq eon-modules
      '(;;; NECCESSARY MODULES
        onboard                         ; include the Emacs ONBOARD starter-kit
        ontop-core                      ; shared settings and definitions
        ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ;;; OPTIONAL MODULES
        ;; ontop-company                   ; code completion alternative
        ontop-consult                   ; search and navigation
        ontop-corfu                     ; code completion and templates
        ;; ontop-evil                      ; modular editing, VIM keybindings
        ;; ontop-flycheck                  ; syntax checker alternative
        ontop-fonts                     ; curated font sets
        ;; ontop-godmode                   ; modular editing, Emacs keybindings
        ontop-helpful                   ; Extended help viewer
        ;; ontop-pdftools                  ; PDF reader
        ontop-vertico                   ; vertical completion
        ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ;;; PROGRAMMING LANGUAGE SUPPORT
        ;; ontop-clojure                   ; Clojure and ClojureScript programming
        ;; ontop-commonlisp                ; Common Lisp programming
        ;; ontop-elixir                    ; Elixir programming
        ;; ontop-erlang                    ; Erlang programming
        ;; ontop-gerbil                    ; Gerbil Scheme programming
        ;; ontop-gleam                     ; Gleam programming
        ;; ontop-haskell                   ; Haskell programming
        ;; ontop-julia                     ; Julia programming
        ;; ontop-lfe                       ; Lisp Flavoured Erlang programming
        ;; ontop-lua                       ; Lua programming
        ;; ontop-ocaml                     ; Ocaml programming
        ;; ontop-racket                    ; Racket programming
        ;; ontop-scheme                    ; Scheme programming
        ontop-webdev                    ; HTML/CSS/JS support
        ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ;;; PERSONAL MODULES
        ;; Below is an empty extra module that can run your personal Elisp code.
        ;; Your code will load last, so that you can override or shadow
        ;; any settings that have been defined within any of the files before.
        ontop-setup-personal  ; ---> edit the file `ontop-setup-personal.el'
        ))

;;  ____________________________________________________________________________
(provide 'ontop-setup-modules)
;;; ontop-setup-modules.el ends here
