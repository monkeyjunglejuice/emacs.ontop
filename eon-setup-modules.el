;;; eon-setup-modules.el --- Module selection -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

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
;;     on a module symbol below and do "<leader> f o" to visit the file.

(setopt
 eon-modules
 '(;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; MODULES TO LOAD EARLY

   ;; eon-compileangel              ; improved native compilation

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; NECESSARY MODULES

   eon                           ; the Emacs ONBOARD starter-kit
   eon-core                      ; required shared settings and definitions

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; KEYBINDINGS -- if none selected, fall back to Emacs keybindings

   eon-evil                      ; modular editing, Vim keybindings
   ;; eon-helix                     ; modular editing, Helix keybindings
   ;; eon-meow                      ; modular editing, Meow keybindings

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; THEMES

   eon-themes-doom               ; Doom Emacs theme pack

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; OPTIONAL MODULES

   eon-autoupdate                ; update packages automatically
   eon-consult                   ; navigation and search framework
   eon-corfu                     ; code (auto)completion
   ;; eon-embark                    ; minibuffer actions and context menu
   eon-flycheck                  ; syntax checker alternative
   ;; eon-fonts                     ; curated font sets
   eon-helpful                   ; extended help viewer
   ;; eon-lispy                     ; edit parenthesis structurally
   eon-magit                     ; Git user interface inside Emacs
   eon-marginalia                ; rich annotations
   ;; eon-pdftools                  ; PDF reader and editor
   eon-smartparens               ; edit parenthesis structurally
   ;; eon-snippets                  ; code snippets
   eon-switchwindow              ; window management
   eon-todo                      ; highlight todo keywords in comments
   eon-vertico                   ; versatile vertical completion

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; PROGRAMMING LANGUAGES

   ;; eon-lang-clojure              ; Clojure and ClojureScript programming
   eon-lang-commonlisp           ; Common Lisp programming with Sly
   eon-lang-elixir               ; Elixir programming
   eon-lang-erlang               ; Erlang programming
   ;; eon-lang-gleam                ; Gleam programming
   ;; eon-lang-haskell              ; Haskell programming
   ;; eon-lang-julia                ; Julia programming
   ;; eon-lang-lfe                  ; Lisp Flavoured Erlang programming
   eon-lang-lua                  ; Lua programming
   eon-lang-ocaml                ; Ocaml programming
   ;; eon-lang-racket               ; Racket programming
   ;; eon-lang-scheme               ; Scheme programming
   ;; eon-lang-schem-chez           ; Chez Scheme support
   ;; eon-lang-scheme-chibi         ; Chibi Scheme support
   ;; eon-lang-scheme-chicken       ; Chicken Scheme support
   ;; eon-lang-scheme-gambit        ; Gambit Scheme support
   ;; eon-lang-scheme-gauche        ; Gauche Scheme support
   eon-lang-scheme-guile         ; Guile Scheme support
   ;; eon-lang-scheme-kawa          ; Kawa Scheme support
   ;; eon-lang-scheme-mit           ; MIT Scheme support
   ;; eon-lang-scheme-racket        ; Racket support; consider `eon-lang-racket'
   ;; eon-lang-scheme-stklos        ; Stklos Scheme support
   eon-lang-webdev               ; HTML/CSS editing

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
