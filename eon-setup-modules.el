;;; eon-setup-modules.el --- Module selection -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; --> Setup your user directory first via "M-x eon-user-setup"
;;     or "<leader> x C-u" (doesn't overwrite existing files).
;;     Visit your user directory via "<leader> x u" and edit this file there.
;;
;; --> 1. Enable or disable modules below by uncommenting and commenting.
;;     2. Save the file.
;;     3. Restart Emacs to apply your changes, or do "M-x eon-load-modules".
;;
;;     You can also load single modules via "<leader> x m"
;;     and unload manually loaded modules via "<leader> x M"
;;
;; --> Documentation lives in the module files. To learn more, place the cursor
;;     on a module name below and do "<leader> f ." to visit a module file.
;;
;; The modules will be loaded one after another, as listed below.
;; The loading process handels module dependencies, so that required modules
;; will be loaded even if they are not explicitly enabled here.
;; Conflicting modules will be prevented from loading.
;;
;;; Code:

;; _____________________________________________________________________________
;;; AVAILABLE MODULES

(setopt
 eon-modules
 '(;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; CORE MODULES
   ;; Basic and shared functionality.

   eon                              ; Configures built-in Emacs packages
   eon-base                         ; Configures generally useful packages

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; MODAL EDITING - choose one; if none enabled, stay with Emacs keybindings.

   ;; eon-evil                      ; Vim keybindings
   ;; eon-god                       ; Emacs keybindings
   ;; eon-helix                     ; Helix keybindings (experimental)
   ;; eon-meow                      ; Meow keybindings

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; THEMES
   ;; Install a theme, but it won't be enabled. Do that in your
   ;; `eon-personal.el' or `init.el'

   ;; eon-theme-batppuccin          ; Soothing pastel themes
   ;; eon-theme-doom                ; Doom Emacs theme pack
   ;; eon-theme-matrix              ; Inspired by "The Matrix" movie
   ;; eon-theme-spacemacs           ; Spacemacs themes

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; OPTIONAL MODULES

   ;; eon-activities                ; Suspend/resume windows, buffers, etc.
   ;; eon-autoupdate                ; Update packages automatically
   ;; eon-cape                      ; Completion-at-point extensions
   ;; eon-consult                   ; Navigation and search
   ;; eon-corfu                     ; Code (auto-)completion
   ;; eon-dired                     ; Classic Dired with improvements
   ;; eon-embark                    ; Minibuffer actions and context menu
   ;; eon-everywhere                ; Use Emacs for text input in other apps
   ;; eon-flycheck                  ; Syntax check; alternative for Flymake
   ;; eon-git                       ; Magit user interface and friends
   ;; eon-helpful                   ; Extended help viewer
   ;; eon-icons                     ; Icons everywhere
   ;; eon-indent                    ; Indent code immediately
   ;; eon-marginalia                ; Rich annotations
   ;; eon-paredit                   ; Edit Lisp code structurally
   ;; eon-paren-face                ; Less visible parenthesis
   ;; eon-pdftools                  ; Sophisticated PDF tool suite
   ;; eon-rainbow-delimiters        ; Color-code nested parenthesis
   ;; eon-reader                    ; Read EPUBs in Emacs
   ;; eon-smartparens               ; More general Paredit alternative
   ;; eon-switchwindow              ; Navigate windows
   ;; eon-tempel                    ; Code snippets and language server support
   ;; eon-todo                      ; Highlight todo keywords in comments
   ;; eon-vertico                   ; Vertical minibuffer completion UI
   ;; eon-vterm                     ; Faster terminal emulator
   ;; eon-yasnippet                 ; Code snippets and language server support

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; OPERATING SYSTEM

   ;; eon-system-packages           ; Manage OS packages within Emacs

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; LLM / AI

   ;; eon-ai                        ; Shared functionality for LLM integration
   ;; eon-ollama                    ; Local and cloud LLMs
   ;; eon-gptel                     ; Comprehensive LLM integration
   ;; eon-gptel-agent               ; Tools and presets for Gptel
   ;; eon-agent-shell               ; ACP-powered frontend for coding agents

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; PROGRAMMING LANGUAGES

   ;; eon-lang-clojure              ; Clojure and ClojureScript programming
   ;; eon-lang-commonlisp-slime     ; Common Lisp programming with Slime
   ;; eon-lang-commonlisp-sly       ; Common Lisp programming with Sly
   ;; eon-lang-elixir               ; Elixir programming
   ;; eon-lang-erlang               ; Erlang programming
   ;; eon-lang-gleam                ; Gleam programming
   ;; eon-lang-haskell              ; Haskell programming
   ;; eon-lang-janet                ; Janet programming
   ;; eon-lang-julia                ; Julia programming with Snail
   ;; eon-lang-lean                 ; Lean programming
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
   ;; eon-lang-unison               ; Unison programming
   ;; eon-lang-web                  ; HTML/CSS editing

   ;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
   ;;; PERSONAL MODULES

   eon-personal                     ; Your personal Emacs Lisp code

   ))

;; _____________________________________________________________________________
(provide 'eon-setup-modules)
;;; eon-setup-modules.el ends here
