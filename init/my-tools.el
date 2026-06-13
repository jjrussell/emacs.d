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
	 (byte-recompile-file user-init-file nil 0 t)
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

;; winum: M-1 through M-9 to jump to numbered windows (replaces window-numbering)
(use-package winum
  :ensure t
  :custom
  (winum-auto-assign-0-to-minibuffer t)
  :bind (("M-0" . winum-select-window-0-or-10)
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9))
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

(use-package embrace
  :bind ("C-," . embrace-commander))



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
         ("<C-tab>" . consult-buffer)
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

;; C-x C-b now handled by perspective's persp-ibuffer binding above

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
  (projectile-project-root-files-bottom-up '(".prj" ".claude" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs"))
  (projectile-project-root-files
   '("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt"
     "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs"
     "Rakefile")))

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
           (set 'use-imenu t)
           )
          )
      (set 'use-imenu t)
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
(global-set-key (kbd "C-.") 'er/expand-region)


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

;; perspective: workspaces that scope buffers per project
(use-package perspective
  :ensure t
  :bind (("C-x C-b" . persp-ibuffer)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-modestring-short t)
  (persp-sort 'created)
  (persp-state-default-file (expand-file-name "perspectives" user-emacs-directory))
  :init
  (persp-mode))

(use-package persp-projectile
  :ensure t
  :after (perspective projectile)
  :bind ([remap projectile-switch-project] . projectile-persp-switch-project))

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
(provide 'my-tools)
(message "Done loading my-tools.el")

;; Local Variables:
;; no-byte-compile: t
;; End:
