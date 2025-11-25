;;; eon-setup-modules.el --- Module selection -*- lexical-binding: t; no-byte-compile: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;;
;; --> Setup your user directory first via "M-x eon-user-setup"
;;     or "<leader> x C-u". Visit your user directory via "<leader> x u"
;;     and edit this file there.
;;
;; --> 1. Enable or disable modules below by uncommenting and commenting.
;;     2. Save the file.
;;     3. Restart Emacs to apply your changes or do "M-x eon-load-modules".
;;
;;     You can also load single modules via "<leader> x m"
;;     and unload manually loaded modules via "<leader> x M"
;;
;; --> Documentation lives in the module files. To learn more, place the cursor
;;     on a module name below and do "<leader> f p" to visit a module file.
;;
;; The modules will be loaded one after another, as listed below.
;;
;;; Code:

;; _____________________________________________________________________________
;;; AVAILABLE MODULES

(setopt
 eon-modules
 '(;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; MODULES TO LOAD EARLY

   eon-user                      ; Your personal Emacs Lisp code
   ;; eon-compileangel              ; Improved native compilation

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; NECESSARY MODULES
   ;; Other modules rely on these modules to be enabled:

   eon                           ; Configures built-in Emacs packages
   ;; eon-base                      ; Shared packages and definitions

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; MODAL EDITING - choose one. If none enabled, stay with Emacs keybindings

   ;; eon-evil                      ; Modal editing: Vim keybindings
   ;; eon-god                       ; Modal editing: Emacs keybindings
   ;; eon-helix                     ; Modal editing: Helix keybindings
   ;; eon-meow                      ; Modal editing: Meow keybindings

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; THEMES
   ;; Installs a theme, but it won't be enabled. Do that in your `eon-user.el'.

   ;; eon-theme-catppuccin         ; Soothing pastel themes
   ;; eon-theme-doom               ; Doom Emacs theme pack
   ;; eon-theme-ef                 ; Colorful and legible themes
   ;; eon-theme-matrix             ; Inspired by "The Matrix" movie
   ;; eon-theme-spacemacs          ; Spacemacs themes

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; OPTIONAL MODULES

   ;; eon-activities                ; Suspend/resume windows, buffers, etc.
   ;; eon-autoupdate                ; Update packages automatically
   ;; eon-cape                      ; Completion-at-point extensions
   ;; eon-consult                   ; Navigation and search
   ;; eon-corfu                     ; Code (auto-)completion
   ;; eon-embark                    ; Minibuffer actions and context menu
   ;; eon-everywhere                ; Use Emacs for text input in other apps
   ;; eon-flycheck                  ; Syntax checker alternative
   ;; eon-fonts                     ; Curated font sets
   ;; eon-eat                       ; Emulate a terminal
   ;; eon-git                       ; Magit user interface and friends
   ;; eon-helpful                   ; Extended help viewer
   ;; eon-icons                     ; Icons everywhere
   ;; eon-indent                    ; Indent code immediately
   ;; eon-marginalia                ; Rich annotations
   ;; eon-pdftools                  ; Sophisticated PDF tool suite
   ;; eon-smartparens               ; Edit parenthesis structurally
   ;; eon-snippets                  ; Code snippets
   ;; eon-switchwindow              ; Navigate windows
   ;; eon-todo                      ; Highlight todo keywords in comments
   ;; eon-vertico                   ; Vertical minibuffer completion UI

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; AI / LLM

   ;; eon-ai                        ; Shared functionality for AI integration
   ;; eon-ollama                    ; Local and cloud LLMs
   ;; eon-gptel                     ; Comprehensive AI chat client

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
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
   ;; eon-lang-scheme               ; Shared config for Scheme programming
   ;; eon-lang-scheme-chez          ; Chez Scheme
   ;; eon-lang-scheme-chibi         ; Chibi Scheme
   ;; eon-lang-scheme-chicken       ; Chicken Scheme
   ;; eon-lang-scheme-gambit        ; Gambit Scheme
   ;; eon-lang-scheme-gauche        ; Gauche Scheme
   ;; eon-lang-scheme-guile         ; Guile Scheme
   ;; eon-lang-scheme-kawa          ; Kawa Scheme
   ;; eon-lang-scheme-mit           ; MIT Scheme
   ;; eon-lang-scheme-racket        ; Consider `eon-lang-racket' instead
   ;; eon-lang-scheme-stklos        ; Stklos Scheme
   ;; eon-lang-web                  ; HTML/CSS editing

   ))

;; _____________________________________________________________________________
(provide 'eon-setup-modules)
;;; eon-setup-modules.el ends here
