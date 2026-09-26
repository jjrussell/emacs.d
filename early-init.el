;;; package -- Early init file to get ther before emacs does  -*- lexical-binding: t; -*-

;;; Commentary:
;; Silence warnings about obsolete functions from packages we don't control.
;; Suppress the specific obsolete warning from yasnippet until it's fixed upstream
;; 2025-08-09 - yasnippets hasn't been updated yet and it's annoying.
;; Try again later.
;;; Code:
(defvar my-emacs-start-time (current-time))
;; Prefer a newer .el over a stale .elc: if a source file is newer than its
;; compiled file, load the source. Set here (before any package loads) so it
;; applies to the whole session. Guards against editing a file and forgetting
;; to recompile -- the old .elc would otherwise be loaded silently.
(setq load-prefer-newer t)
(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)

;; Never paint a native tool-bar on any frame (initial, subsequent, or
;; emacsclient). Pinning this in default-frame-alist before the first frame is
;; created suppresses the startup flash and the macOS titlebar toolbar capsule,
;; which the later (tool-bar-mode -1) in init.el alone does not prevent.
(push '(tool-bar-lines . 0) default-frame-alist)

