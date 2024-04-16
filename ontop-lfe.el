;;; ontop-lfe.el --- Lisp Flavoured Erlang  -*- lexical-binding: t; -*-
;; This file is part of Emacs ONTOP
;; https://github.com/monkeyjunglejuice/emacs.ontop

;;; Commentary:
;; You can also use this file/configuration independently from Emacs ONTOP
;; Load it from anywhere via `(load-file "/path/to/ontop-lfe.el")'.

;;; Code:

;;  ____________________________________________________________________________
;;; USE-PACKAGE
;; <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package nil))

(eval-when-compile
  (require 'use-package))

;;  ____________________________________________________________________________
;;; LFE
;; <https://github.com/lfe/lfe>
;; <https://github.com/lfe/rebar3>

(use-package lfe-mode
  :ensure t
  :custom
  (inferior-lfe-program "lfe")
  (inferior-lfe-program-options nil))

;;  ____________________________________________________________________________
;;; ERLANG
;; <https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html>

(use-package erlang
  :ensure t)

;;  ____________________________________________________________________________
(provide 'ontop-lfe)
;;; ontop-lfe.el ends here
