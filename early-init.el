;;; package -- Early init file to get ther before emacs does  -*- lexical-binding: t; -*-

;;; Commentary:
;; Silence warnings about obsolete functions from packages we don't control.
;; Suppress the specific obsolete warning from yasnippet until it's fixed upstream
;; 2025-08-09 - yasnippets hasn't been updated yet and it's annoying.
;; Try again later.
;;; Code:
(defvar my-emacs-start-time (current-time))
(setq byte-compile-warnings '(not obsolete))
