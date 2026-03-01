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
  ("a" projectile-ag "ag")
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
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
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

(use-package treemacs
  :ensure t
  :bind
  ("<f8>" . treemacs-select-window))


;; https://github.com/hlissner/emacs-doom-themes
;; Enable custom neotree theme (nerd-icons must be installed!)
;; Make sure nerd-icons is installed
(use-package nerd-icons
  :ensure t)

;; Configure doom-themes
(use-package doom-themes
  :ensure t
  :config
  ;; This is the crucial part: load your chosen theme AFTER the package is loaded.
  (load-theme 'doom-solarized-dark t)

  ;; The treemacs and org integrations are now handled by separate functions.
  ;; Call the ones you need here.
  (doom-themes-treemacs-config) ;; For Treemacs
  (doom-themes-org-config))     ;; For Org Mode

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
(global-aggressive-indent-mode 1)


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
;; autocomplete popups everywhere. Sometimes its even smart
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)
;; ;; Don't put hippie expand here because it is at the front of smart-tab list
;; (set-default 'ac-sources
;;              '(
;;                ac-source-yasnippet
;;                ac-source-imenu
;;                ;;ac-source-dictionary
;;                ac-source-words-in-buffer
;;                ac-source-words-in-same-mode-buffers
;;                ac-source-filename
;;                ac-source-abbrev
;;                ;;ac-source-semantic
;;                ))

;; (custom-set-variables
;;  '(ac-etags-requires 1))

;; (eval-after-load "etags"
;;   '(progn
;;      (ac-etags-setup)))
;; ;; added to ruby mode hook in my-development.el

;; (global-auto-complete-mode t)
;; (set-default 'ac-fuzzy-enable t)
;; (setq ac-fuzzy-enable t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Company mode - alternative to auto-complete
;; Trying it out 2016-04-01
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; no config needed yet. global-company-mode in customize 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ido - makes switching between anything in emacs amazing with type-ahead search, fuzzy matching etc.
;; Can be made to work with selecting anything, commands, files, whatever
;; Imporoved buffer selection with C-x b if available
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;; enable ido style completion in M-x menu
(use-package smex
  :bind ("M-x" . smex))

(use-package ido
  :ensure nil ; ido is built-in
  :config
  (ido-mode 1)  ; actually I kind of like it with file
  (ido-everywhere 1)
  (ido-vertical-mode 1)
  (flx-ido-mode 1) ; fuzzy matching for ido https://github.com/lewang/flx
  :custom
  (ido-create-new-buffer 'prompt)
  (ido-enable-flex-matching t) ;; disable ido faces to see flx highlights.
  (ido-use-faces t)
  (ido-max-window-height 30)) 


;; use ido absolutely everywhere where completing-read would be
;; 2022-04-23 stopped working https://github.com/mrkkrp/ido-ubiquitous
;; doesn't appear to be in melpa. Forked from DarwinAwardWinner/ido-completing-read-plus
;; which I have installed so this is likely abandoned. 
;; (ido-ubiquitous-mode 1) 


(define-key global-map [(control x)(control b)] 'ibuffer)
(global-set-key [(control x) (b)] 'ido-switch-buffer)
(global-set-key (kbd "<C-tab>") 'ido-switch-buffer)
(with-eval-after-load 'ido
  (define-key ido-common-completion-map [(control n)] 'ido-next-match)
  (define-key ido-common-completion-map [(control p)] 'ido-prev-match))

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
  :bind (("M-." . projectile-find-tag)
         ("M-p" . projectile-find-file)
         ("M-t" . imenu-anywhere)
         ("M-*" . pop-tag-mark))
  :custom
  (projectile-cache-file "~/.emacs.local/projectile.cache")
  (projectile-enable-caching nil)
  (projectile-globally-ignored-files '("*.elc" "TAGS"))
  (projectile-remember-window-configs t)
  (projectile-sort-order 'recently-active)
  (projectile-switch-project-action 'projectile-commander)
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
  ;; Ensure grip-mode uses xwidgets
  (setq grip-use-xwidgets t)
  
  ;; Force the xwidget/grip buffer to open in a side-by-side split on the right
  (add-to-list 'display-buffer-alist
               '("^\\*xwidget-webkit:\\|^\\*grip" 
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.5))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Vundo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [(control _)] 'vundo)


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

(defun my-yank-pop ()
  "If previous command was not a yank, search kill ring"
  (interactive)
  (if (eq last-command 'yank)
      (yank-pop)
    ;; incremental search through the kill ring. Awesome.
    (browse-kill-ring)))

(global-set-key "\M-y" 'my-yank-pop)

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

;; perspective
;; if perspective mode is enabled either by customize or in any other file it causes weird errors with
;; ido-buffer-switch. If we load it after all lisp is loaded and files are reopened it works. Wonky.
(add-hook 'desktop-after-read-hook (lambda () (persp-mode t)))

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

;; incremental occur awesomeness
(global-set-key (kbd "M-s i") 'ioccur)

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
;; Acejump mode - marking words for direct jumping
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark))
  :config
  (ace-jump-mode-enable-mark-sync))

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
;; HELM - https://github.com/emacs-helm/helm
;; Don't use this much
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (helm-mode 1)
;; (require 'helm-config)
;; (require 'helm-themes)
(global-set-key (kbd "C-c h") 'helm-mini)


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
;; byte-compile-warnings: (not free-vars obsolete)
;; End:
