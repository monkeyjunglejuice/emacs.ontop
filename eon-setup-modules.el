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
;;     on a module symbol below and do "<leader> f p" to visit the file.
;;
;;; Code:

;; _____________________________________________________________________________
;;; AVAILABLE MODULES

(setopt
 eon-modules
 '(;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; MODULES TO LOAD EARLY

   ;; eon-compileangel              ; Improved native compilation

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; NECESSARY MODULES

   eon                           ; The Emacs ONBOARD starter-kit
   eon-base                      ; Shared packages and definitions

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; MODAL EDITING - if none selected, fall back to Emacs keybindings

   ;; eon-evil                      ; Modal editing: Vim keybindings
   ;; eon-god                       ; Modal editing: Emacs keybindings
   ;; eon-helix                     ; Modal editing: Helix keybindings
   ;; eon-meow                      ; Modal editing: Meow keybindings

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; THEMES

   ;; eon-theme-catppuccin         ; Soothing pastel themes
   ;; eon-theme-doom               ; Doom Emacs theme pack
   ;; eon-theme-spacemacs          ; Spacemacs themes
   ;; eon-theme-matrix             ; Theme inspired by "The Matrix" movie

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; OPTIONAL MODULES

   eon-activities                ; Suspend/resume windows, buffers, etc.
   ;; eon-autoupdate                ; Update packages automatically
   eon-cape                      ; Completion-at-point extensions
   eon-consult                   ; Navigation and search
   ;; eon-corfu                     ; Code (auto-)completion
   ;; eon-embark                    ; Minibuffer actions and context menu
   ;; eon-flycheck                  ; Syntax checker alternative
   ;; eon-fonts                     ; Curated font sets
   eon-git                       ; Magit user interface and friends
   eon-helpful                   ; Extended help viewer
   ;; eon-icons                     ; Icons everywhere
   ;; eon-indent                    ; Indent code immediately
   eon-marginalia                ; Rich annotations
   ;; eon-pdftools                  ; Sophisticated PDF tool suite
   ;; eon-smartparens               ; Edit parenthesis structurally
   ;; eon-snippets                  ; Code snippets
   eon-switchwindow              ; Navigate windows
   eon-todo                      ; Highlight todo keywords in comments
   eon-vertico                   ; Vertical minibuffer completion UI

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
   ;; eon-lang-scheme-racket        ; consider `eon-lang-racket' instead
   ;; eon-lang-scheme-stklos        ; Stklos Scheme
   eon-lang-web                  ; HTML/CSS editing

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; AI / LLM

   ;; eon-ai                        ; Shared functionality for AI integration
   ;; eon-gptel                     ; Comprehensive AI chat client

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; PERSONAL MODULE

   ;; Below is an empty extra module that runs your personal Elisp code.
   ;; Your code will load last, so that you can override or shadow
   ;; any settings that have been defined within any of the files before.
   ;; ---> edit the file `eon-setup-personal.el'
   eon-setup-personal))

;; _____________________________________________________________________________
(provide 'eon-setup-modules)
;;; eon-setup-modules.el ends here
