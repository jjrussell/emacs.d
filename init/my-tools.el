;;; package -- Non-built-in tools config -*- lexical-binding: t; -*-
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file conatins initialization for third party packages that I have found.
;; These are all found in
;; * my-emacs-home/site-lisp/tools
;; * my-emacs-home/site-lisp/ for the bigger packages
;; * my-emacs-home/elpa for elpa packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hydra   definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :demand t)

(defhydra hydra-mine (:exit t :color teal :hint nil)
  ("R" (lambda ()
         "Reload .emacs file and recompile any init files that need it."
         (interactive)
         ;; Make sure that the newest versions of init files are compiled.
         (byte-recompile-directory my-emacs-init-dir 0)
         ;;(byte-recompile-directory my-emacs-local-store)
         ;; don't force recompile, compile if no .elc is present and load file when done

         ;; 2019-04-23 trying not compiling init.el so customization takes effect without recompiling
         ;; is it that much slower?
	 ;; recompile init.el (arg 0 = compile even if no .elc yet), then load it.
	 ;; `byte-recompile-file's old LOAD 4th arg is advertised-obsolete.
	 (byte-recompile-file user-init-file nil 0)
	 (load-file user-init-file)
         (my-after-init-hook)
         ) "Reload emacs config" :column "Quick Open")
  ("e" (lambda () (interactive) (find-file user-init-file)) "init.el")
  ("j" (lambda () (interactive) (find-file "~/.bashrc")) ".bashrc")
  ("J" (lambda () (interactive) (find-file "~/.jshrc")) ".jshrc")
  ("s" (lambda () (interactive) (switch-to-buffer "*scratch*")) "*scratch*")
  ("m" (lambda () (interactive) (switch-to-buffer "*Messages*")) "*Messages")


  ("\\" my-indent-buffer "indent buffer" :column "Text")
  
  ("u" duplicate-thing "duplicate line")

  ("i" insert-short-date "insert short date ")
  ("C-i" insert-long-date "insert long date")
  ("M-i" insert-time "insert time")
  ("C-M-i" insert-full-date-and-time "insert full date and time")
  ("#" my-comment-block "command block")

  ("t" org-capture "org-capture")
  ("C-e" edebug-defun "edebug-defun")
  ("M-p" fill-paragraph "fill-paragraph")
  ("C-M-p" fill-region "fill-region")
  ("b" browse-url-at-point "browse-url-at-point")
  ("f" my-init-flyspell "toggle flyspell")
  ("o" my-occur-all-buffers "occur all buffers")
  
  ("!" shell-command "shell-command" :column "Utilities")
  ("a" consult-ripgrep "ripgrep")
  ("y" paste-stack-in-project "paste-stack-in-project")
  ("M-t" toggle-window-dedicated "toggle-window-dedicated")
  ("n" my-truncate-toggle "toggle truncate lines")
  ("1" my-open-terminal-here "open terminal cwd")
  ("3" my-open-filemanager-here "open file manager cwd")
  ("C-k" my-delete-file-of-buffer "Delete file of buffer")
  
  ("q"  nil "cancel" :color blue :column "Hydra") )
(global-set-key (kbd "C-x d") 'hydra-mine/body)

(when nil  ; ─── DISABLED: hydra-lsp (replaced by hydra-eglot below) ──────────
(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown))
) ; ─── END DISABLED hydra-lsp ────────────────────────────────────────────────

(defhydra hydra-eglot (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_r_] reconnect            [_d_] find-def     [_i_] find-impl    [_o_] eldoc
 [_F_] diagnostics      [_S_] shutdown             [_R_] find-refs    [_t_] find-type    [_n_] rename
 [_x_] code action      [_e_] events buffer"
  ("d" xref-find-definitions)
  ("R" xref-find-references)
  ("i" eglot-find-implementation)
  ("t" eglot-find-typeDefinition)
  ("n" eglot-rename)
  ("o" eldoc-doc-buffer)

  ("f" eglot-format-buffer)
  ("F" flymake-show-buffer-diagnostics)
  ("x" eglot-code-actions)

  ("r" eglot-reconnect)
  ("S" eglot-shutdown)
  ("e" eglot-events-buffer))

(global-set-key (kbd "C-c l") 'hydra-eglot/body)

(defhydra hydra-projectile (:color teal
                                   :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim                              _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   consult-ripgrep)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))
;; 
(global-set-key (kbd "C-x m") 'hydra-projectile/body)



(use-package vterm
  :ensure t)

(use-package ai-code
  ;; :straight (:host github :repo "tninja/ai-code-interface.el") ;; if you want to use straight to install, no need to have MELPA setting above
  :config
  ;; use codex as backend, other options are 'claude-code, 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'cursor, 'kiro, 'codebuddy, 'aider, 'claude-code-ide, 'claude-code-el
  (ai-code-set-backend 'claude-code)
  ;; Enable global keybinding for the main menu
  (global-set-key (kbd "C-c a") #'ai-code-menu)
  ;; Optional: Use eat if you prefer, by default it is vterm
  ;; (setq ai-code-backends-infra-terminal-backend 'eat) ;; the way to config all native supported CLI. for external backend such as claude-code-ide.el and claude-code.el, please check their config
  ;; Optional: Enable @ file completion in comments and AI sessions
  (ai-code-prompt-filepath-completion-mode 1)
  ;; Optional: Ask AI to run test after code changes, for a tighter build-test loop
  (setq ai-code-auto-test-type 'test-after-change)
  ;; Optional: In AI session buffers, SPC in Evil normal state triggers the prompt-enter UI
  (with-eval-after-load 'evil (ai-code-backends-infra-evil-setup))
  ;; Optional: Turn on auto-revert buffer, so that the AI code change automatically appears in the buffer
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1) ;; set to 1 second for faster update
  ;; (global-set-key (kbd "C-c a C") #'ai-code-toggle-filepath-completion)
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

(use-package org
  :ensure nil ; org is built-in, no need to install
  :hook
  (org-mode . (lambda ()
                ;; Org mode parser requires a tab-width of 8
                (setq-local tab-width 8))))

;; markdown-mode: org-modern-style treatment for Markdown files.
;; Scales headings, hides raw markup (**, _, #) while keeping the styling,
;; and syntax-highlights fenced code blocks. Toggle markup visibility live
;; with `markdown-toggle-markup-hiding'.
(defvar my-markdown-prose-fonts
  '(;; serif
    "Charter" "Georgia" "Palatino"
    ;; sans-serif
    "Verdana" "Avenir Next" "Optima" "Carlito" "Helvetica Neue" "Inter")
  "Candidate variable-pitch fonts to audition in Markdown buffers.
Serif families first, then sans-serif.  \"Carlito\" is a
metric-compatible Calibri substitute; \"Inter\" is Obsidian's default
text font.")

(defvar my-markdown-prose-font "Inter"
  "Default variable-pitch font applied in Markdown buffers.")

(defvar my-markdown-prose-height 1.2
  "Height multiplier for the prose font (leaves monospace/code untouched).")

(defvar-local my-markdown-prose-font-index 0
  "Index into `my-markdown-prose-fonts' for cycling.")

(defface my-markdown-prose-face
  '((t :inherit variable-pitch))
  "Prose face for Markdown buffers.
`my-markdown-set-prose-font' sets its :family/:height; mixed-pitch
reads from this face (via buffer-local `mixed-pitch-face'), so the
display refreshes when the font changes.")

(defun my-markdown-set-prose-font (family)
  "Set the prose FAMILY live in Markdown buffers and refresh the display.
Interactively, pick from `my-markdown-prose-fonts' with completion.

mixed-pitch captures the family/height from `mixed-pitch-face' only
when it (re)applies, so we update the dedicated face, point
`mixed-pitch-face' at it, and re-run `mixed-pitch-mode'."
  (interactive (list (completing-read "Prose font: " my-markdown-prose-fonts nil nil)))
  (set-face-attribute 'my-markdown-prose-face nil
                      :family family :height my-markdown-prose-height)
  (setq-local mixed-pitch-face 'my-markdown-prose-face)
  (setq-local mixed-pitch-set-height t) ; honor :height above (default is nil)
  (mixed-pitch-mode 1)                  ; idempotent: re-captures the new family
  (force-window-update (current-buffer))
  (message "Prose font: %s" family))

(defun my-markdown-cycle-prose-font ()
  "Switch to the next font in `my-markdown-prose-fonts' for quick comparison."
  (interactive)
  (setq my-markdown-prose-font-index
        (mod (1+ my-markdown-prose-font-index) (length my-markdown-prose-fonts)))
  (my-markdown-set-prose-font (nth my-markdown-prose-font-index my-markdown-prose-fonts)))

(defun my-markdown-visual-tweaks ()
  "Give Markdown buffers room to breathe: line spacing plus a readable,
enlarged prose font (monospace/code faces stay at their normal height).
Also enables `mixed-pitch-mode' (via `my-markdown-set-prose-font'), so it
is intentionally NOT a separate hook \(a bare `mixed-pitch-mode' hook
toggles and would race with this setup)."
  (setq-local line-spacing 0.25)
  (visual-line-mode 1)                  ; soft word-wrap at the window edge
  (olivetti-mode 1)                     ; centered, margined text column
  (setq my-markdown-prose-font-index
        (or (seq-position my-markdown-prose-fonts my-markdown-prose-font #'string=) 0))
  (my-markdown-set-prose-font my-markdown-prose-font)
  ;; valign aligns tables on-screen only, leaving the raw text untouched.
  (valign-mode 1))

(defun my-markdown-next-checkbox ()
  "Move point inside the next GFM checkbox [ ]."
  (interactive)
  (when (re-search-forward "^\\s-*[-*+] \\[\\([ xX]\\)\\]" nil t)
    (goto-char (match-beginning 1))))

(defun my-markdown-prev-checkbox ()
  "Move point inside the previous GFM checkbox [ ]."
  (interactive)
  (beginning-of-line)
  (when (re-search-backward "^\\s-*[-*+] \\[\\([ xX]\\)\\]" nil t)
    (goto-char (match-beginning 1))))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode) ; GitHub-Flavored Markdown (derived from markdown-mode)
  :custom
  (markdown-header-scaling t)
  (markdown-header-scaling-values '(1.6 1.4 1.2 1.1 1.0 1.0))
  (markdown-hide-markup t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-table-align-p nil) ; don't reflow raw table text; valign handles display
  :hook (markdown-mode . my-markdown-visual-tweaks)
  :bind (:map markdown-mode-map
              ("M-p" . nil)                          ; free M-p (was markdown-previous-link)
              ("M-[" . markdown-promote)
              ("M-]" . markdown-demote)
              ("C-M-n" . my-markdown-next-checkbox)
              ("C-M-p" . my-markdown-prev-checkbox)
              ("C-c m f" . my-markdown-set-prose-font)    ; pick a font by name
              ("C-c m c" . my-markdown-cycle-prose-font)  ; cycle to next candidate
              ("C-c m h" . markdown-toggle-markup-hiding)  ; show/hide raw markup
              ("C-c m t" . markdown-toc-generate-or-refresh-toc))) ; table of contents

;; mixed-pitch: variable-pitch font for prose, monospace for code/tables.
;; Used by markdown-mode above for the org-modern reading feel.
(use-package mixed-pitch
  :ensure t
  :defer t
  :config
  ;; This mixed-pitch version predates `markdown-table-face' and omits it
  ;; from the defaults, so tables render in the proportional prose font and
  ;; never align. Keep tables fixed-pitch so columns line up.
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'markdown-table-face))

;; valign: pixel-perfect table alignment that respects variable-pitch fonts
;; (Inter) and hidden markup -- fixes the ragged tables `markdown-table-align'
;; can't, since it aligns by displayed pixel width rather than raw chars.
(use-package valign
  :ensure t
  :defer t
  :custom (valign-fancy-bar t)) ; draw bars with box-drawing chars

;; olivetti: center the text in a comfortable measure with wide margins
;; (distraction-free "writeroom" look).
(use-package olivetti
  :ensure t
  :defer t
  :custom (olivetti-body-width 120))

;; markdown-toc: generate / refresh a table of contents in the document.
(use-package markdown-toc
  :ensure t
  :defer t)

(use-package treemacs
  :ensure t
  :bind
  (("C-c t" . treemacs-select-window)
   ("C-c T" . my/treemacs-close)
   :map treemacs-mode-map
   ("C-c C-p" . treemacs-projectile)
   ("C-c t g" . treemacs-hide-gitignored-files-mode))
  :custom
  (treemacs-width 35)
  (treemacs-is-never-other-window t)
  (treemacs-show-hidden-files t)
  (treemacs-indent-guide-style 'line)
  :config
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-indent-guide-mode t)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-fringe-indicator-mode 'always)

  (defun my/treemacs-close ()
    "Close the treemacs window from any buffer."
    (interactive)
    (pcase (treemacs-current-visibility)
      ('visible (delete-window (treemacs-get-local-window)))))

  (defvar my/treemacs-show-gitignored-paths
    '("~/Library/CloudStorage/GoogleDrive-jorussell@hubspot.com/My Drive/assistant")
    "Project roots where gitignored files should be visible.")

  (defun my/treemacs-adjust-gitignore-visibility (&rest _)
    (when-let* ((ws (treemacs-current-workspace))
                (projects (treemacs-workspace->projects ws)))
      (let ((dominated (seq-some
                        (lambda (proj)
                          (let ((root (treemacs-project->path proj)))
                            (seq-some
                             (lambda (path)
                               (string-prefix-p (expand-file-name path)
                                                (expand-file-name root)))
                             my/treemacs-show-gitignored-paths)))
                        projects)))
        (treemacs-hide-gitignored-files-mode (if dominated -1 1)))))

  (add-hook 'treemacs-select-functions #'my/treemacs-adjust-gitignore-visibility)
  (add-hook 'treemacs-switch-workspace-hook #'my/treemacs-adjust-gitignore-visibility)
  (add-hook 'treemacs-workspace-first-found-functions
            (lambda (&rest _) (my/treemacs-adjust-gitignore-visibility))))

(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

;; winum: numbers each window (shown in the mode-line). The M-1..M-9
;; window-selection bindings were removed so those keys are free for
;; tab-bar tab switching (see the tab-bar block below).
(use-package winum
  :ensure t
  :custom
  (winum-auto-assign-0-to-minibuffer t)
  :config
  (winum-mode))

;; nerd-icons-dired: file type icons in dired
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("^" . (lambda () (interactive) (find-alternate-file "..")))))

(use-package dired-x
  :ensure nil
  :after dired)

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

;; nerd-icons-ibuffer: file type icons in ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; https://github.com/hlissner/emacs-doom-themes
;; Enable custom neotree theme (nerd-icons must be installed!)
;; Make sure nerd-icons is installed
(use-package nerd-icons
  :ensure t
  :config
  (unless (member "Symbols Nerd Font Mono" (font-family-list))
    (nerd-icons-install-fonts t)))

;; Configure doom-themes
(use-package doom-themes
  :ensure t
  :config
  (when (fboundp 'my/apply-system-appearance)
    (my/apply-system-appearance ns-system-appearance))
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-buffer-modification-icon t)
  :init (doom-modeline-mode 1))

(use-package diminish
  :ensure t
  :config
  (diminish 'subword-mode)
  (diminish 'aggressive-indent-mode)
  (diminish 'eldoc-mode))

;; Window and editing behavior
(winner-mode 1)
(delete-selection-mode 1)
(customize-set-variable 'help-window-select t)
(setq-default cursor-in-non-selected-windows nil)

(defun my/stop-using-minibuffer ()
  "Kill the minibuffer when clicking outside of it."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook #'my/stop-using-minibuffer)

;; ws-butler: strip trailing whitespace only from lines you touched
(use-package ws-butler
  :ensure t
  :diminish
  :hook (prog-mode text-mode))

;; string-inflection: cycle between camelCase, snake_case, PascalCase, etc.
(use-package string-inflection
  :ensure t
  :bind ("C-c C-u" . string-inflection-all-cycle))

;; transpose-frame: swap vertical/horizontal split layout
(use-package transpose-frame
  :ensure t
  :bind ("C-x 5 t" . transpose-frame))

;; wgrep: make grep/ripgrep result buffers editable
(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t))

;; Colorize nested braces and things
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(electric-pair-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Replaces auto-indent
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; github-browse-at-point
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define-key global-map [(control O)] 'github-browse-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The mighty TAB key and all of its magic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So the tab key works like this.
;; Smart-tab owns the tab key. smart-tab is enabled globally in customization.
;; It is configured to use hippie-expand to provide completions
;; or auto-complete if auto-complete-mode is enabled

;; snippets for most modes. Custom snippets go in ~/.emacs.d/snippets
;; yasnippet autoloads ~/.emacs.d/snippets

;; (yas-global-mode 1)
;; (yasnippet-snippets-initialize) ; 2022-04-23 this function appears to be gone
;; yas-expand is explicitly unbound from TAB as it is at the front of the
;; list of hippie-expand functions to try which smart-tab will use.
;; hippie-expand is configured in customize
;; (define-key yas-minor-mode-map [(tab)] nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)

;; make sure we can get to hippie-expand at the normal spot for dabbrev
;; in case auto-complete is using tab key
(global-set-key (kbd "M-/") 'hippie-expand)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completing-read: vertico + consult + orderless + marginalia
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; vertico: vertical completing-read UI (replaces ido+smex+ido-vertical)
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :custom
  (vertico-count 20)
  (vertico-cycle t))

;; orderless: fuzzy/flexible matching (replaces flx-ido)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

;; marginalia: annotations in completing-read (shows docstrings, file info, etc.)
(use-package marginalia
  :ensure t
  :init (marginalia-mode))


;; consult: enhanced completing-read commands (replaces ioccur, browse-kill-ring, helm-mini)
(use-package consult
  :ensure t
  :bind (;; drop-in replacements via remap
         ([remap switch-to-buffer]          . consult-buffer)
         ([remap goto-line]                 . consult-goto-line)
         ([remap yank-pop]                  . consult-yank-pop)
         ([remap bookmark-jump]             . consult-bookmark)
         ([remap imenu]                     . consult-imenu)
         ([remap repeat-complex-command]    . consult-complex-command)
         ([remap project-switch-to-buffer]  . consult-project-buffer)
         ([remap Info-search]               . consult-info)
         ;; additional bindings
         ("C-x B"   . consult-buffer-other-window)
         ;; <C-tab> intentionally left unbound here so tab-bar's built-in
         ;; `tab-next' works (browser-style next-tab). consult-buffer is still
         ;; on C-x b (remap of switch-to-buffer) and C-c h below.
         ("C-c h"   . consult-buffer)
         ;; navigation (M-g prefix)
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake)
         ("M-g o"   . consult-outline)
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g I"   . consult-imenu-multi)
         ;; search (M-s prefix)
         ("M-s d"   . consult-find)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ;; isearch integration
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)
         ;; minibuffer history
         :map minibuffer-local-map
         ("M-s"     . consult-history)
         ("M-r"     . consult-history))
  :custom
  (consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"))

;; C-x C-b: global ibuffer (previously perspective's persp-ibuffer)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; which-key: after pressing a prefix key, shows available continuations
;; Activates on a timer (default 1 second) -- just pause after a prefix like C-x or C-c
(use-package which-key
  :ensure t
  :diminish
  :custom
  (which-key-idle-delay 0.5)
  (which-key-separator " → ")
  :config
  (which-key-mode 1))

;; Fix ido-ubiquitous for newer packages
;; http://whattheemacsd.com/setup-ido.el-01.html
;; (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
;;   `(eval-after-load ,package
;;      '(defadvice ,cmd (around ido-ubiquitous-new activate)
;;         (let ((ido-ubiquitous-enable-compatibility nil))
;;           ad-do-it))))

;; (ido-ubiquitous-use-new-completing-read webjump 'webjump)
;; (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
;; (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile and project functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind (("M-p" . projectile-find-file)
         ("M-t" . consult-imenu)
         ("M-T" . consult-imenu-multi)
         ("M-*" . xref-go-back))
  :custom
  (projectile-cache-file "~/.emacs.local/projectile.cache")
  (projectile-enable-caching nil)
  (projectile-globally-ignored-files '("*.elc" "TAGS"))
  (projectile-remember-window-configs t)
  (projectile-sort-order 'recently-active)
  (projectile-completion-system 'default)
  (projectile-switch-project-action 'projectile-dired)
  (projectile-tags-backend 'find-tag)
  ;; You have a very extensive list of root files, this keeps it neat
  (projectile-project-root-files-bottom-up '(".prj" ".claude" ".omo" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs"))
  (projectile-project-root-files
   '("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt"
     "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs"
     "Rakefile"))
  ;; projectile-session-mode: each project gets its own tab-bar tab with a
  ;; restorable, disk-backed window/buffer session (replaces perspective).
  (projectile-session-directory "~/.emacs.local/projectile-sessions/")
  ;; When first entering a project (fresh tab, no saved session), land in
  ;; dired -- matching the old `projectile-switch-project-action'.
  (projectile-session-default-action 'projectile-dired)
   (projectile-session-restore-on-switch t)
   (projectile-session-autosave t)
   :config
   (projectile-session-mode +1))

;; Browser-style navigation for the projectile-session tabs.
;; Ctrl-TAB / Ctrl-Shift-TAB (next / previous tab) are built into tab-bar and
;; already active. For "jump to tab N": M-1..M-8 select that tab, M-9 the last
;; tab, M-0 the most recently visited (Cmd is Meta on this Mac). These keys were
;; freed up by removing the winum window-selection bindings above. Numeric prefix
;; args are still available on C-1..C-9. `tab-bar-tab-hints' shows the numbers in
;; the bar so you can see what to press.
(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-tab-hints t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tag handling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update tags file whenever I switch files and do a tags thing.
;; sets advice on tags functions to update the file
;; 2022-04-23 no package available. Not sure what replaced it. Don't use tags much
;; (require 'etags-table)


;; use ido to find imenu tags
;; ido-anywhere does this for all open buffers. Like that better
;;(global-set-key (kbd "M-t") 'idomenu)

;; Try to find a tag but if it fails use imenu-anywhere
;; Could never get this to work. If find-tag is ever called in this function and fails then
;; imenu-anywhere returns "No imenu tags". I tried it with and without artificially failing
;; condition-case but imenu-anywhere only doesn't work when find-tags is called
;;

(defun find-default-tag ()
  "If there's a TAGS file somewhere, use that. otherwise go to the default imenu
at point."
  (interactive)

  (let ((tag-name (find-tag-default))
        (use-imenu nil))
    (if (locate-dominating-file default-directory "TAGS")
        (condition-case nil
            ;; if this fails and we catch the error below then we get to call imenu-anywhere
            ;; but it always has no tags. find-tag must do something that imenu-anywhere cna't
            ;; deal with. Not sure what.
            (xref-find-definitions tag-name)
          (user-error
           (setq use-imenu t)
           )
          )
      (setq use-imenu t)
      )


    (cond (use-imenu
           (imenu-anywhere)))
    )
  )



;; live viewing of markdown preview. Requires brew install grip
(use-package grip-mode
  :ensure t
  :config
  (setq grip-preview-use-webkit nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Vundo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vundo
  :ensure t
  :custom
  (vundo-popup-timeout 2.0)
  :config
  (require 'vundo-popup)
  (vundo-popup-mode 1)
  :bind (("C-c u" . vundo)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ack - using ack-and-a-half  https://github.com/jhelwig/ack-and-a-half
;; Use projectile-ag instead
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Other packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;brings some sanity to the page up page down commands as well as one line scolling
(use-package pager
  :ensure t
  :bind* (("C-v" . pager-page-down)
          ("<next>" . pager-page-down)
          ("s-v" . pager-page-up)
          ("<prior>" . pager-page-up)
          ("C-q" . pager-row-up)
          ("C-z" . pager-row-down)))
;; (require 'pager)
;; (global-set-key [(control v)] 'pager-page-down)
;; (global-set-key [next]  'pager-page-down)
;; (global-set-key [(meta v)] 'pager-page-up)
;; (global-set-key [prior] 'pager-page-up)
;; (global-set-key [(control q)] 'pager-row-up)
;; (global-set-key [(control z)] 'pager-row-down)
;; (global-set-key [(control Q)] 'quoted-insert)


;; multiple cursor mode
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; vscode bindings
(global-set-key (kbd "M-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-<up>") 'mc/mark-previous-like-this)

(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)

;; expand region on each successive use of this.
;;(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-.") 'expreg-expand)
(global-set-key (kbd "C-,") 'expreg-contract)



;; better duplicate buffer name handling
(use-package uniquify
  :ensure nil
  :config
  ;; uniquify-buffer-name-style is not a defcustom, so we use :config
  (setq uniquify-buffer-name-style 'forward)
  :custom
  (uniquify-min-dir-content 1)
  (uniquify-trailing-separator-p t))

(require 'newcomment) ;; loads stuff for my cool comment block function my-comment-block

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(define-key global-map (kbd "<f8>") 'switch-to-minibuffer)

;; by default, activating the minibuffer with ido global mode doesn't fire the
;; window-numbering-update function in window-numbering. Put it in this hook so that the minibuffer
;; is set to M-0
;; 2015-03-30 this must have been fixed in window numbering mode as this now causes an error
;; that the minibuffer is double numbered as 0. So everythings works ok with this.
;; (add-hook 'ido-minibuffer-setup-hook
;;           (function
;;            (lambda ()
;;              (when (and window-numbering-auto-assign-0-to-minibuffer
;;                         (active-minibuffer-window))
;;                (window-numbering-assign (active-minibuffer-window) 0))
;;              )))


;; toggles single and double quotes
(global-set-key (kbd "C-'") 'toggle-quotes)

;; Distraction free writing
(use-package writeroom-mode
  :ensure t
  :bind (("C-M-j" . writeroom-mode))
  :custom
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-width 150)
  (writeroom-restore-window-config t)
  (writeroom-mode-line t)
  (writeroom-border-width 0)
  (writeroom-global-effects
   '(writeroom-set-fullscreen writeroom-set-alpha writeroom-set-menu-bar-lines
			      writeroom-set-tool-bar-lines
			      writeroom-set-vertical-scroll-bars
			      writeroom-set-internal-border-width))
  (writeroom-disable-fringe t))

;; Project workspaces are now handled by `projectile-session-mode' (enabled in
;; the projectile use-package block above): each project gets its own tab-bar
;; tab with a restorable, disk-backed session. This replaces the previous
;; perspective + persp-projectile setup.

;; Takes over these keys to let you type them fast in succesion to perform commands.
;; Using common keys can result in a slight delay on the lead key but mostly I've never noticed this
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define-global ",." "<>\C-b")
  (key-chord-define-global "xx" 'smex)
  (key-chord-define-global "hh" 'switch-to-previous-buffer))
;; What other ones would be useful?


;; visible bookmarks
(use-package bm
  :ensure t
  :bind (("<C-f2>" . bm-toggle)
         ("<f2>"   . bm-next)
         ("<S-f2>" . bm-previous)))

;; buffer-move - Move buffers to other open windows by direction
;; On the mac, C-up/down/left/right switches desktops
;; However, if you press C-option-direction emacs sees it as just C-direction
;; and OSX will let it through so these keybindings work.
(global-set-key (kbd "<C-M-up>")     'buf-move-up)
(global-set-key (kbd "<C-M-down>")   'buf-move-down)
(global-set-key (kbd "<C-M-left>")   'buf-move-left)
(global-set-key (kbd "<C-M-right>")  'buf-move-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Avy - jump to any visible text by character (replaces ace-jump-mode)
;; avy-goto-char-timer: type chars then pick label; avy-goto-line for lines
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-char-timer)
         ("C-c C-SPC" . avy-goto-line)
         ("C-x SPC" . avy-pop-mark)
         :map isearch-mode-map
         ("C-'" . avy-isearch))
  :custom
  (avy-timeout-seconds 0.4)
  (avy-style 'de-bruijn)
  (avy-background t)
  (avy-all-windows t)
  :config
  ;; Avy dispatch actions: after triggering avy (C-c SPC + chars), press a
  ;; dispatch key BEFORE selecting a candidate to act on it at a distance:
  ;;   w = copy word/sexp at target to kill ring (stay put)
  ;;   k = kill at target (stay put)
  ;;   t = teleport: yank target to point
  ;;   y = yank: paste target text at point
  ;;   m = mark at target
  ;;   z = zap from point to target
  ;; Just select the candidate normally (letter/number) to jump as usual.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ace window mode - ace jump but for windows
;; https://github.com/abo-abo/ace-window
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dired config
;; package to make dired not suck.  It reuses one window instead of
;; spawning 5x10^56 new dired buffers
;; 2025-08-11 commented out. I don't use this anymore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'dired-load-hook
;;           (lambda ()
;;             (load "dired-x")
;;             ))

;; ;; enable using 'a' in dired buffers to replace the buffer instead of spawning a new one
;; (put 'dired-find-alternate-file 'disabled nil)

;; ;;redefine dired to use this to make one dired buffer which gets reused
;; (defun dired (&optional path)
;;   (interactive)
;;   (dired-single-magic-buffer path))

;; (defun my-dired-single-hook ()
;;   "Bunch of stuff to run for dired, either immediately or when it's
;;          loaded."
;;   (define-key dired-mode-map [return] 'dired-single-buffer)
;;   (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
;;   (define-key dired-mode-map [?^]
;;     (function (lambda nil (interactive)(dired-single-buffer ".."))))
;;   )

;; ;; if dired's already loaded, then the keymap will be bound
;; (if (boundp 'dired-mode-map)
;;     ;; we're good to go; just add our bindings
;;     (my-dired-single-hook)
;;   ;; it's not loaded yet, so add our bindings to the load-hook
;;   (add-hook 'dired-load-hook 'my-dired-single-hook))

;; ;;Remap dired keybindings to dired-single replacement functions
;; (global-set-key [(f5)] 'dired-single-magic-buffer)
;; (global-set-key [(control f5)] (function
;;                                 (lambda nil (interactive)
;;                                   (dired-single-magic-buffer
;;                                    default-directory))))
;; (global-set-key [(shift f5)] 'dired-single-toggle-buffer-name)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; agent-shell: AI agent interface (Claude Code, Codex, Gemini)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my/worktree-root "~/src/wt/"
  "Root directory for centralized worktrees.")

(use-package agent-shell
  :demand t
  :bind
  ("C-c s" . agent-shell-help-menu)
  (:map projectile-command-map
        ("<SPC>" . my/agent-shell-dwim)
        ("@" . agent-shell-send-current-file))
  (:map agent-shell-mode-map
        ("RET" . newline)
        ("C-c C-c" . shell-maker-submit)
        ("C-c C-k" . agent-shell-interrupt)
        ("C-x n" . agent-shell-prompt-compose))

  :custom
  (visual-fill-column-width 120)
  (agent-shell-confirm-interrupt nil)
  (agent-shell-prefer-session-resume nil)
  (agent-shell-preferred-agent-config 'claude-code)
  (agent-shell-google-authentication (agent-shell-google-make-authentication :login t))
  (agent-shell-openai-authentication
   (agent-shell-openai-make-authentication
    :codex-api-key (lambda ()
                     (string-trim
                      (shell-command-to-string "hsauthctl idp get-token")))))
  (agent-shell-context-sources '(files region error))
  (agent-shell-agent-configs
   (list (agent-shell-anthropic-make-claude-code-config)
         (agent-shell-openai-make-codex-config)
         (agent-shell-google-make-gemini-config)))
  (agent-shell-mcp-servers '(((name . "linear")
                              (type . "http")
                              (headers . [])
                              (url . "https://mcp.linear.app/mcp"))))

  :config
  (defun my/agent-shell-dwim (&optional arg)
    "Smart agent-shell dispatcher.

   With prefix ARG, delegate to `agent-shell' with the prefix.
   If in an agent-shell buffer, switch to the previous buffer without closing the window.
   If the project's agent shell is visible but not focused, focus it.
   If the project's agent shell exists but is not visible, toggle it.
   Otherwise start a new shell with `agent-shell'."
    (interactive "P")
    (let* ((shell-buffers (agent-shell-project-buffers))
           (visible-window (cl-some (lambda (buf) (get-buffer-window buf)) shell-buffers)))
      (cond
       (arg
        (agent-shell arg))
       ((derived-mode-p 'agent-shell-mode)
        (switch-to-prev-buffer))
       (visible-window
        (select-window visible-window))
       (shell-buffers
        (agent-shell-toggle))
       (t
        (agent-shell)))))

  (defun my/agent-shell-focus-input (_frame)
    "Focus the latest permission button or input area in agent-shell buffers."
    (when (derived-mode-p 'agent-shell-mode)
      (or (agent-shell-jump-to-latest-permission-button-row)
          (goto-char (point-max)))))

  (add-hook 'window-selection-change-functions #'my/agent-shell-focus-input))

;; Hide/show tool call blocks in agent-shell buffers.
;;
;; Tool calls are identified by the presence of an :inverse-video face in
;; their label-left — that's the status box ([done], [running], etc.) that
;; agent-shell renders only for tool calls, not for thought or message blocks.
;;
;; Each block is tagged with a `my/tool-call-block' text property at render
;; time via `agent-shell-section-functions'.  The toggle command then walks
;; the buffer flipping the `invisible' property on those regions.

(defvar-local my/agent-shell-tool-calls-hidden nil
  "Non-nil when tool call blocks are hidden in this buffer.")

(defun my/agent-shell--tool-call-section-p (range)
  "Return non-nil if RANGE is a tool call block.
Detected by scanning the label-left region for an :inverse-video face,
which is unique to tool call status boxes."
  (when-let* ((ll (map-elt range :label-left))
              (start (map-elt ll :start))
              (end (map-elt ll :end)))
    (catch 'found
      (let ((pos start))
        (while (< pos end)
          (let ((face (get-text-property pos 'font-lock-face)))
            (when (and (listp face) (member '(:inverse-video t) face))
              (throw 'found t)))
          (setq pos (or (next-single-property-change pos 'font-lock-face nil end)
                        end)))))))

(defun my/agent-shell--tag-section (range)
  "Tag tool call blocks for later hiding.
Called from `agent-shell-section-functions' with inhibit-read-only already t."
  (when (my/agent-shell--tool-call-section-p range)
    (let* ((padding (map-elt range :padding))
           (block (map-elt range :block))
           (start (or (map-elt padding :start) (map-elt block :start)))
           (end (map-elt block :end)))
      (when (and start end)
        (put-text-property start end 'my/tool-call-block t)
        (when my/agent-shell-tool-calls-hidden
          (put-text-property start end 'invisible 'my-tool-call-hidden))))))

(add-hook 'agent-shell-section-functions #'my/agent-shell--tag-section)

(defun my/agent-shell-toggle-tool-calls ()
  "Toggle visibility of tool call blocks in the current agent-shell buffer.
When hidden, new tool calls arriving during the session are also hidden
automatically.  Individual blocks can still be expanded with TAB when
visible."
  (interactive)
  (let ((inhibit-read-only t)
        (hide (not my/agent-shell-tool-calls-hidden)))
    (save-excursion
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (if (get-text-property pos 'my/tool-call-block)
              (let ((end (or (next-single-property-change pos 'my/tool-call-block nil (point-max))
                             (point-max))))
                (put-text-property pos end 'invisible (if hide 'my-tool-call-hidden nil))
                (setq pos end))
            (setq pos (or (next-single-property-change pos 'my/tool-call-block nil (point-max))
                          (point-max)))))))
    (setq my/agent-shell-tool-calls-hidden hide)
    (message "Tool calls %s" (if hide "hidden" "visible"))))

(with-eval-after-load 'agent-shell
  (keymap-set agent-shell-mode-map "C-c h" #'my/agent-shell-toggle-tool-calls))


;; agent-shell-manager: sidebar to list and switch between agent shells
(use-package agent-shell-manager
  :vc (:url "https://github.com/jethrokuan/agent-shell-manager" :rev :newest)
  :after agent-shell
  :bind
  ("C-c S" . agent-shell-manager-toggle)
  (:map agent-shell-mode-map
        ("C-c S" . agent-shell-manager-toggle))
  (:map agent-shell-manager-mode-map
        ("C-g" . quit-window))
  :custom
  (agent-shell-manager-side 'bottom)
  (agent-shell-manager-transient t))

;; agent-shell dashboard: tile all agent-shell buffers in a grid
(defun my/tile-agent-shells ()
  "Tile all agent-shell buffers in a grid in the current frame."
  (interactive)
  (let ((bufs (seq-filter
               (lambda (b)
                 (eq (buffer-local-value 'major-mode b) 'agent-shell-mode))
               (buffer-list))))
    (when (null bufs)
      (user-error "No agent-shell buffers found"))
    (delete-other-windows)
    (let* ((n (length bufs))
           (cols (ceiling (sqrt n)))
           (rows (ceiling (/ (float n) cols))))
      (dotimes (_ (1- rows))
        (split-window-below))
      (balance-windows)
      (let ((row-wins (window-list nil 'no-mini)))
        (dolist (w row-wins)
          (select-window w)
          (dotimes (_ (1- cols))
            (split-window-right))))
      (balance-windows)
      (let ((all-wins (window-list nil 'no-mini)))
        (cl-loop for buf in bufs
                 for win in all-wins
                 do (set-window-buffer win buf))
        (cl-loop for win in (nthcdr n all-wins)
                 do (delete-window win))))))

(defun my/switch-or-create-tab (name)
  "Select the tab-bar tab named NAME, creating it if it doesn't exist."
  (let ((names (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs))))
    (if (member name names)
        (tab-bar-select-tab-by-name name)
      (tab-bar-new-tab)
      (tab-bar-rename-tab name))))

(defun my/agent-shell-tab ()
  "Switch to (or create) an agents tab with tiled agent-shell buffers."
  (interactive)
  (my/switch-or-create-tab "agents")
  (my/tile-agent-shells))

(bind-key "C-c M-a" #'my/agent-shell-tab)

;; agent-shell-macext: macOS-native notifications and file handling
(use-package agent-shell-macext
  :vc (:url "https://github.com/cxa/agent-shell-macext" :rev :newest)
  :hook (agent-shell-mode . agent-shell-macext-setup)
  :custom
  (agent-shell-macext-file-copy-policy 'always-original)
  (agent-shell-macext-notifications t)
  (agent-shell-macext-notify-current-buffer nil))

;; dispatch-task: pick or create a worktree, then start an agent-shell in it
(defun my/worktree-repo-name ()
  "Return the basename of the current repo's toplevel directory."
  (when-let ((root (magit-toplevel)))
    (file-name-nondirectory (directory-file-name root))))

(defun my/worktree-list-external ()
  "Return alist of (PATH . BRANCH) for worktrees under `my/worktree-root' for the current repo."
  (when-let ((repo (my/worktree-repo-name)))
    (let ((prefix (expand-file-name (file-name-concat my/worktree-root repo))))
      (seq-filter
       (lambda (entry)
         (string-prefix-p prefix (car entry)))
       (mapcar (lambda (wt)
                 (let* ((path (car wt))
                        (branch (nth 2 wt)))
                   (cons path (or branch "(detached)"))))
               (magit-list-worktrees))))))

(defun my/dispatch-task ()
  "Pick or create a worktree, then start an agent-shell in it."
  (interactive)
  (unless (magit-toplevel)
    (user-error "Not in a git repository"))
  (let* ((repo (my/worktree-repo-name))
         (existing (my/worktree-list-external))
         (new-label "[new worktree]")
         (choices (cons
                   (cons new-label nil)
                   (mapcar (lambda (entry)
                             (let ((dir (file-name-nondirectory
                                         (directory-file-name (car entry))))
                                   (branch (cdr entry)))
                               (cons (format "%s (%s)" dir branch)
                                     (car entry))))
                           existing)))
         (choice (completing-read "Worktree: " choices nil t))
         (worktree-path
          (or (alist-get choice choices nil nil #'string=)
              (let* ((branch (read-string "Branch name: "))
                     (path (expand-file-name
                            (file-name-concat my/worktree-root repo branch))))
                (make-directory (file-name-directory path) t)
                (magit-worktree-branch path branch "HEAD")
                (unless (file-exists-p path)
                  (user-error "Failed to create worktree at %s" path))
                path))))
    (my/switch-or-create-tab (file-name-nondirectory (directory-file-name worktree-path)))
    (agent-shell--new-shell :location worktree-path)))

(provide 'my-tools)
(message "Done loading my-tools.el")

;; Local Variables:
;; no-byte-compile: t
;; End:
