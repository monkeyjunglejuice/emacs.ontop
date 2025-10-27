;;; eon-setup-modules.el --- Module selection -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; --> Enable or disable modules below by uncommenting and commenting ...
;;     ... then apply your changes via "M-x eon-require-modules".
;;     You can also load single modules via "M-x eon-load-module RET eon-"
;;     and unload manually loaded modules via "M-x eon-unload-module RET eon-"
;;
;; --> Documentation lives in the module files. To learn more, place the cursor
;;     on a module symbol below and do "<leader> f o" to visit the file.
;;
;;; Code:

;;  ____________________________________________________________________________
;;; AVAILABLE MODULES

(setopt
 eon-modules
 '(;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; MODULES TO LOAD EARLY

   ;; eon-compileangel              ; Improved native compilation

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; NECESSARY MODULES

   eon                           ; The Emacs ONBOARD starter-kit
   eon-core                      ; Required shared settings and definitions

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; KEYBINDINGS -- if none selected, fall back to Emacs keybindings

   ;; eon-evil                      ; Modular editing, Vim keybindings
   ;; eon-god                       ; Modular editing, Emacs keybindings
   ;; eon-helix                     ; Modular editing, Helix keybindings
   ;; eon-meow                      ; Modular editing, Meow keybindings

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; THEMES

   ;; eon-theme-catppuccin         ; Soothing pastel themes
   ;; eon-theme-doom               ; Doom Emacs theme pack
   ;; eon-theme-spacemacs          ; Spacemacs theme
   ;; eon-theme-matrix             ; Inspired by "The Matrix" movie

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; OPTIONAL MODULES

   ;; eon-autoindent                ; Indent code immediately
   ;; eon-autoupdate                ; Update packages automatically
   eon-cape                      ; Completion-at-point extensions
   eon-consult                   ; Navigation and search framework
   eon-corfu                     ; Code (auto)completion
   ;; eon-embark                    ; Minibuffer actions and context menu
   ;; eon-flycheck                  ; Syntax checker alternative
   ;; eon-fonts                     ; Curated font sets
   eon-helpful                   ; Extended help viewer
   eon-git                       ; Git user interface
   eon-marginalia                ; Rich annotations
   ;; eon-pdftools                  ; Sophisticated PDF tool suite
   ;; eon-smartparens               ; Edit parenthesis structurally
   ;; eon-snippets                  ; Code snippets
   eon-switchwindow              ; Window management
   eon-todo                      ; Highlight todo keywords in comments
   eon-vertico                   ; Versatile vertical completion UI

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; PROGRAMMING LANGUAGES

   ;; eon-lang-clojure              ; Clojure and ClojureScript programming
   ;; eon-lang-commonlisp           ; Common Lisp programming with Sly
   ;; eon-lang-elixir               ; Elixir programming
   ;; eon-lang-erlang               ; Erlang programming
   ;; eon-lang-gleam                ; Gleam programming
   ;; eon-lang-haskell              ; Haskell programming
   ;; eon-lang-julia                ; Julia programming
   ;; eon-lang-lfe                  ; Lisp Flavoured Erlang programming
   ;; eon-lang-lua                  ; Lua programming
   ;; eon-lang-ocaml                ; Ocaml programming
   ;; eon-lang-racket               ; Racket programming
   ;; eon-lang-scheme               ; Scheme programming with Geiser
   ;; eon-lang-scheme-chez          ; Chez Scheme support
   ;; eon-lang-scheme-chibi         ; Chibi Scheme support
   ;; eon-lang-scheme-chicken       ; Chicken Scheme support
   ;; eon-lang-scheme-gambit        ; Gambit Scheme support
   ;; eon-lang-scheme-gauche        ; Gauche Scheme support
   ;; eon-lang-scheme-guile         ; Guile Scheme support
   ;; eon-lang-scheme-kawa          ; Kawa Scheme support
   ;; eon-lang-scheme-mit           ; MIT Scheme support
   ;; eon-lang-scheme-racket        ; consider `eon-lang-racket' instead
   ;; eon-lang-scheme-stklos        ; Stklos Scheme support
   eon-lang-web                  ; HTML/CSS editing

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;;; AI

   ;; eon-ai                        ; Shared config for AI integration
   ;; eon-gptel                     ; Comprehensive AI Chat client

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
