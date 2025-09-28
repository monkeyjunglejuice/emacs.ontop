;;; eon-setup-modules.el --- Modules -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop
;;
;;; Commentary:
;;
;;; Code:

;;  ____________________________________________________________________________
;;; SETUP
;;
;; --> Enable or disable modules below via commenting and uncommenting ...
;;     ... then apply your changes via "M-x eon-require-modules".
;;     You can also load single modules via "M-x eon-load-module RET eon-"
;;     and unload manually loaded modules via "M-x eon-unload-module RET eon-"
;;
;; --> Documentation lives in the module files. To learn more, place the cursor
;;     on a module entry below and do `M-x ffap RET' to visit the file.

(setq
 eon-modules
 '(;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; NATIVE ELISP COMPILATION

   ;; eon-nativecomp                ; improved native compilation

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; NECESSARY MODULES

   eon                           ; The Emacs ONBOARD starter-kit
   eon-core                      ; shared settings and definitions

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; KEYBINDINGS

   eon-evil                      ; modular editing, Vim keybindings
   ;; eon-helix                     ; modular editing, Helix keybindings
   ;; eon-meow                      ; modular editing, Meow keybindings

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; OPTIONAL MODULES

   eon-autoupdate                ; update packages automatically
   eon-consult                   ; navigation and search framework
   ;; eon-embark                    ; minibuffer actions
   ;; eon-flycheck                  ; syntax checker alternative
   ;; eon-fonts                     ; curated font sets
   eon-helpful                   ; extended help viewer
   eon-pdftools                  ; PDF reader
   eon-smartparens               ; structural editing
   eon-snippets                  ; code snippets
   eon-switchwindow              ; window manageement
   eon-vertico                   ; vertical completion

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; PROGRAMMING LANGUAGES

   ;; eon-clojure                   ; Clojure and ClojureScript programming
   eon-commonlisp                ; Common Lisp programming, using Sly
   ;; eon-elixir                    ; Elixir programming
   eon-erlang                    ; Erlang programming
   ;; eon-gleam                     ; Gleam programming
   ;; eon-haskell                   ; Haskell programming
   ;; eon-julia                     ; Julia programming
   ;; eon-lfe                       ; Lisp Flavoured Erlang programming
   eon-lua                       ; Lua programming
   ;; eon-ocaml                     ; Ocaml programming
   ;; eon-racket                    ; Racket programming
   eon-scheme                    ; Scheme programming
   eon-webdev                    ; HTML/CSS editing

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; PERSONAL MODULES

   ;; Below is an empty extra module that runs your personal Elisp code.
   ;; Your code will load last, so that you can override or shadow
   ;; any settings that have been defined within any of the files before.
   ;; ---> edit the file `eon-setup-personal.el'
   eon-setup-personal))

;;  ____________________________________________________________________________
(provide 'eon-setup-modules)
;;; eon-setup-modules.el ends here
