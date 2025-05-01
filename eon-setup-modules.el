;;; eon-setup-modules.el --- Select your modules  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; Emacs ONTOP is an extension on top of the Emacs ONBOARD starter-kit.

;;; Code:

;;  ____________________________________________________________________________
;;; SETUP

;; --> Enable or disable modules below via commenting and uncommenting ...
;;     ... then apply your changes via "M-x eon-require-modules".
;;     You can also load single modules via "M-x load-library RET eon-"
;;     and unload manually loaded modules via "M-x unload-feature RET eon-"
;;
;; --> Documentation lives in the module files. To learn more, place the cursor
;;     on a module entry below and do `M-x ffap RET' to visit the file.

(setq eon-modules
      '(;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ;;; NATIVE ELISP COMPILATION

        ;; eon-nativecomp                ; improved native compilation

        ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ;;; NECESSARY MODULES

        eon                           ; include the Emacs ONBOARD starter-kit
        eon-core                      ; shared settings and definitions

        ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ;;; OPTIONAL MODULES

        ;; eon-embark                    ; minibuffer actions
        ;; eon-evil                      ; modular editing, VIM keybindings
        ;; eon-flycheck                  ; syntax checker alternative
        ;; eon-fonts                     ; curated font sets
        eon-helpful                   ; extended help viewer
        ;; eon-meow                      ; modular editing, Meow keybindings
        ;; eon-pdftools                  ; PDF reader
        ;; eon-smartparens               ; Structural editing

        ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ;;; PROGRAMMING LANGUAGES

        ;; eon-clojure                   ; Clojure and ClojureScript programming
        ;; eon-commonlisp                ; Common Lisp programming
        ;; eon-elixir                    ; Elixir programming
        ;; eon-erlang                    ; Erlang programming
        ;; eon-gerbil                    ; Gerbil Scheme programming
        ;; eon-gleam                     ; Gleam programming
        ;; eon-haskell                   ; Haskell programming
        ;; eon-julia                     ; Julia programming
        ;; eon-lfe                       ; Lisp Flavoured Erlang programming
        ;; eon-lua                       ; Lua programming
        ;; eon-ocaml                     ; Ocaml programming
        ;; eon-racket                    ; Racket programming
        ;; eon-scheme                    ; Scheme programming
        eon-webdev                    ; HTML/CSS editing

        ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ;;; PERSONAL MODULES

        ;; Below is an empty extra module that can run your personal Elisp code.
        ;; Your code will load last, so that you can override or shadow
        ;; any settings that have been defined within any of the files before.
        ;; ---> edit the file `eon-setup-personal.el'
        eon-setup-personal
        ))

;;  ____________________________________________________________________________
(provide 'eon-setup-modules)
;;; eon-setup-modules.el ends here
