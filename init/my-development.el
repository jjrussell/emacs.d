;; This file contains modes for editing different programming languages
;; that do not come standard with emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General purpose development tools
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; git support
;; Mosty use a combination of egg and magit with their default settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-file-dispatch)
         (:map magit-status-mode-map
               ("q" . my/magit-quit-session)))
  :config
  (defun my/magit-fullscreen (orig-fun &rest args)
    "Save window config, run magit-status, then delete other windows."
    (window-configuration-to-register :magit-fullscreen)
    (apply orig-fun args)
    (delete-other-windows))

  (advice-add 'magit-status :around #'my/magit-fullscreen)

  (defun my/magit-quit-session ()
    "Restore the previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package forge
  :ensure t
  :after magit
  :init
  )

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(defun shell-git-status(dir)
  (interactive)
  "Function called from emacsclient to show git status from pwd"
  (find-file dir)
  (magit-status)
  (delete-other-windows)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lsp-mode (DISABLED — replaced by eglot; see block below)
;; Original config from: https://raw.githubusercontent.com/neppramod/java_emacs/master/emacs-configuration.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; treesit-auto: manages tree-sitter grammar installation and mode remapping
;; Automatically remaps e.g. ruby-mode -> ruby-ts-mode, python-mode -> python-ts-mode etc.
;; treesit-auto is disabled due to a performance bug: it calls treesit-language-available-p
;; for every language on every file open, causing a 2-3 second freeze.
;; https://github.com/renzmann/treesit-auto/issues/84
;; If that issue is resolved, re-enable this and remove the manual remap below:
;;
;; (use-package treesit-auto
;;   :ensure t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (global-treesit-auto-mode))

;; Grammar sources for treesit-install-language-grammar.
;; treesit-auto populated this automatically; we do it manually here.
;; Add entries here alongside any new candidates below.
(setq treesit-language-source-alist
      '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (cmake      . ("https://github.com/tree-sitter/tree-sitter-cmake"))
        (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua        . ("https://github.com/MunifTanjim/tree-sitter-lua"))
        (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
        (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (toml       . ("https://github.com/tree-sitter-grammars/tree-sitter-toml"))
        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))))

;; Manual remap: same effect as treesit-auto but computed once at startup.
;; Each entry: (old-mode grammar-symbol . ts-mode)
;; Add new languages here (and a source above); they activate once the grammar is installed.
(let ((candidates
       '((bash-mode        bash       . bash-ts-mode)
         (sh-mode          bash       . bash-ts-mode)
         (c-mode           c          . c-ts-mode)
         (c++-mode         cpp        . c++-ts-mode)
         (cmake-mode       cmake      . cmake-ts-mode)
         (css-mode         css        . css-ts-mode)
         (dockerfile-mode  dockerfile . dockerfile-ts-mode)
         (go-mode          go         . go-ts-mode)
         (java-mode        java       . java-ts-mode)
         (js-mode          javascript . js-ts-mode)
         (javascript-mode  javascript . js-ts-mode)
         (js-json-mode     json       . json-ts-mode)
         (json-mode        json       . json-ts-mode)
         (lua-mode         lua        . lua-ts-mode)
         (python-mode      python     . python-ts-mode)
         (ruby-mode        ruby       . ruby-ts-mode)
         (rust-mode        rust       . rust-ts-mode)
         (toml-mode        toml       . toml-ts-mode)
         (typescript-mode  typescript . typescript-ts-mode)
         (yaml-mode        yaml       . yaml-ts-mode))))
  (setq major-mode-remap-alist
        (delq nil
              (mapcar (lambda (entry)
                        (when (treesit-language-available-p (cadr entry) t)
                          (cons (car entry) (cddr entry))))
                      candidates))))

;; corfu: lightweight completion-at-point UI (replaces company)
;; Works with eglot's completion-at-point-functions integration natively
(use-package corfu
  :ensure t
  :custom
  (corfu-auto nil)
  (corfu-quit-no-match 'separator)
  (global-corfu-minibuffer nil)
  :bind (:map corfu-map
              ("TAB" . corfu-insert)
              ([tab] . corfu-insert)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

;; cape: additional completion-at-point sources for corfu
(use-package cape
  :ensure t
  :init
  (defun my/cape-dabbrev-no-minibuffer () (unless (minibufferp) (cape-dabbrev)))
  (defun my/cape-file-no-minibuffer () (unless (minibufferp) (cape-file)))
  (defun my/cape-keyword-no-minibuffer () (unless (minibufferp) (cape-keyword)))
  (add-hook 'completion-at-point-functions #'my/cape-dabbrev-no-minibuffer)
  (add-hook 'completion-at-point-functions #'my/cape-file-no-minibuffer)
  (add-hook 'completion-at-point-functions #'my/cape-keyword-no-minibuffer))

(use-package yasnippet :config (yas-global-mode))
(use-package yasnippet-snippets :ensure t)
(use-package flycheck :ensure t :init (global-flycheck-mode))
(when nil  ; ─── DISABLED: lsp-mode (replaced by eglot below) ───────────────

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list)))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-enable nil
              lsp-ui-doc-delay 1.5
	      lsp-ui-doc-position 'bottom
              lsp-ui-doc-max-width 100
              lsp-ui-sideline-enable nil
              lsp-ui-peek-list-width 80)
  :config
  (defun my-lsp-ui-peek--truncate-right (len s)
    "Truncate S from the left so the rightmost LEN chars are visible."
    (if (> (string-width s) len)
        (concat ".." (substring s (- (length s) (max (- len 2) 0))))
      s))
  (advice-add 'lsp-ui-peek--truncate :override #'my-lsp-ui-peek--truncate-right))
(use-package consult-lsp
  :ensure t
  :after (lsp-mode consult)
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . consult-lsp-symbols)))
(defun my/lsp-java-per-project-workspace (orig-fn &rest args)
  "Advise `lsp-java--ls-command' to use per-project workspace dirs.
Each project root gets its own JDTLS data directory under
jdtls-workspaces/, so worktrees and separate repos never
cross-pollinate their indexes.  The workspace name is derived
from the path relative to ~ with / replaced by !, e.g.
  ~/src/WorkflowMigration  → src!WorkflowMigration
  ~/src/feature/WFM        → src!feature!WFM"
  (let* ((root (or (lsp-workspace-root)
                   (and (bound-and-true-p projectile-mode)
                        (projectile-project-root))
                   (locate-dominating-file default-directory ".git")))
         (name (when root
                 (replace-regexp-in-string
                  "/" "!"
                  (file-relative-name (directory-file-name root) "~"))))
         (lsp-java-workspace-dir
          (if name
              (expand-file-name name
                                (expand-file-name "jdtls-workspaces"
                                                  user-emacs-directory))
            lsp-java-workspace-dir))
         (lsp-java-workspace-cache-dir
          (expand-file-name ".cache/" lsp-java-workspace-dir)))
    (message "[JDTLS] per-project workspace: %s" lsp-java-workspace-dir)
    (apply orig-fn args)))

(use-package lsp-mode
  :ensure t
  :hook (
	 (lsp-mode . lsp-enable-which-key-integration)
	 (java-mode . #'lsp-deferred)
	 (java-ts-mode . #'lsp-deferred)
	 )
  :init (setq
	 lsp-keymap-prefix "C-c l"
	 lsp-enable-file-watchers nil
	 lsp-enable-imenu t
	 lsp-progress-via-spinner t
	 lsp-headerline-breadcrumb-enable nil
	 read-process-output-max (* 3 1024 1024) ; 3 MB — JDTLS sends large payloads
	 lsp-completion-provider :capf
	 lsp-idle-delay 0.500
	 lsp-response-timeout 120
	 lsp-keep-workspace-alive nil
	 lsp-log-io nil
	 lsp-lens-enable nil
	 lsp-signature-auto-activate nil
	 lsp-modeline-code-actions-enable nil
	 lsp-modeline-diagnostics-enable nil
	 ;; Let HSJF own all formatting. Without these, JDTLS reformats
	 ;; as you type (on-type-formatting) and on every newline
	 ;; (indentation), overriding hsjf-format-on-save-mode.
	 lsp-enable-on-type-formatting nil
	 lsp-enable-indentation nil
	 lsp-enable-symbol-highlighting nil
	 lsp-auto-guess-root nil          ; don't merge roots into mega-workspaces
	 )
  :config
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  )
(defun my/lsp-java-clean-workspaces (&optional days)
  "Delete JDTLS workspace directories not modified in DAYS (default 30).
Also available interactively to pick which workspaces to remove."
  (interactive)
  (let* ((ws-root (expand-file-name "jdtls-workspaces" user-emacs-directory))
         (days (or days 30))
         (cutoff (time-subtract (current-time) (days-to-time days)))
         (dirs (and (file-directory-p ws-root)
                    (directory-files ws-root t "^[^.]"))))
    (if (not dirs)
        (when (called-interactively-p 'any) (message "No JDTLS workspaces found."))
      (if (called-interactively-p 'any)
          (let* ((annotated (mapcar (lambda (d)
                                      (let ((size (string-trim (shell-command-to-string
                                                                 (format "du -sh %s | cut -f1" (shell-quote-argument d)))))
                                            (age (format-time-string "%Y-%m-%d" (file-attribute-modification-time (file-attributes d)))))
                                        (cons (format "%s  (%s, last modified %s)" (file-name-nondirectory d) size age) d)))
                                    dirs))
                 (selected (completing-read-multiple "Delete workspaces (comma-separated): " annotated)))
            (dolist (s selected)
              (when-let* ((path (cdr (assoc s annotated))))
                (delete-directory path t)
                (message "Deleted %s" path))))
        (dolist (d dirs)
          (when (time-less-p (file-attribute-modification-time (file-attributes d)) cutoff)
            (delete-directory d t)
            (message "Auto-cleaned stale JDTLS workspace: %s" (file-name-nondirectory d))))))))

(run-with-idle-timer 10 nil #'my/lsp-java-clean-workspaces 30)

(defun my/lsp-java-nuke-workspace (&optional project)
  "Kill JDTLS servers and delete workspace indexes.
With no prefix arg, nukes ALL per-project workspaces.
With prefix arg, prompts to select which project workspaces to delete."
  (interactive "P")
  (let ((jdtls-dir (expand-file-name "jdtls-workspaces" user-emacs-directory)))
    ;; Shut down all LSP workspaces gracefully
    (when (fboundp 'lsp-workspace-shutdown)
      (dolist (workspace (lsp-workspaces))
        (ignore-errors (lsp-workspace-shutdown workspace))))
    ;; Force-kill any lingering jdtls processes
    (shell-command "pkill -f 'eclipse.jdt.ls' 2>/dev/null")
    (if (and project (file-directory-p jdtls-dir))
        ;; Selective: pick which project workspaces to nuke
        (let* ((dirs (directory-files jdtls-dir t "^[^.]"))
               (names (mapcar #'file-name-nondirectory dirs))
               (selected (completing-read-multiple "Nuke workspaces: " names)))
          (dolist (name selected)
            (let ((path (expand-file-name name jdtls-dir)))
              (when (file-directory-p path)
                (delete-directory path t)
                (message "Deleted workspace: %s" name)))))
      ;; Nuke everything
      (when (file-directory-p jdtls-dir)
        (delete-directory jdtls-dir t)
        (message "Deleted all JDTLS workspaces"))
      ;; Also clean legacy shared workspace if it still exists
      (let ((legacy (expand-file-name "workspace" user-emacs-directory)))
        (when (file-directory-p legacy)
          (delete-directory legacy t)
          (message "Deleted legacy shared workspace"))))
    (message "Done. Open a Java file to re-index.")))

(use-package lsp-java
  :ensure t
  :config
  (setq lsp-java-vmargs
        '("-XX:+UseG1GC"
          "-XX:+UseStringDeduplication"
          "-XX:GCTimeRatio=4"
          "-XX:AdaptiveSizePolicyWeight=90"
          "-Dsun.zip.disableMemoryMapping=true"
          "-Xmx2G"
          "-Xms1G"
          "-Xss4m")
        ;; Autobuild is the #1 performance killer — it rebuilds ALL indexed
        ;; projects on every file change. With 20+ repos, this is devastating.
        lsp-java-autobuild-enabled nil
        lsp-java-max-concurrent-builds 1
        lsp-java-completion-max-results 20
        ;; Skip directories that bloat the index without adding navigation value
        lsp-java-import-exclusions ["**/node_modules/**"
                                    "**/build/**"
                                    "**/.gradle/**"
                                    "**/bin/**"
                                    "**/.metadata/**"
                                    "**/archetype-resources/**"
                                    "**/META-INF/maven/**"])
  (add-to-list 'auto-mode-alist '("\\.class)\\'" . java-ts-mode))

  ;; ── Force jdtls to single-root mode ──────────────────────────
  ;; lsp--client is a cl-defstruct; byte-compiled lsp-mode inlines
  ;; accessor reads as (aref client N).  Neither setf nor advice on
  ;; lsp--client-multi-root can intercept those.  We must aset the
  ;; actual struct slot to nil so every code path reads nil.
  ;;
  ;; Use cl-struct-slot-info for a dynamic index lookup so this
  ;; survives lsp-mode adding/removing slots in future versions.
  (when-let ((client (gethash 'jdtls lsp-clients)))
    (let ((idx 0))
      (dolist (slot-desc (cl-struct-slot-info 'lsp--client))
        (when (eq (car slot-desc) 'multi-root)
          (aset client idx nil)
          (message "[JDTLS] disabled multi-root on jdtls client (slot %d)" idx))
        (setq idx (1+ idx)))))

  ;; Belt-and-suspenders: also advise lsp--find-multiroot-workspace
  ;; (a regular defun, always dispatched) to block server reuse for jdtls.
  (defun my/jdtls-single-root (orig-fn session client project-root)
    "Prevent jdtls from reusing an existing server for a new project root."
    (unless (eq (lsp--client-server-id client) 'jdtls)
      (funcall orig-fn session client project-root)))
  (advice-add 'lsp--find-multiroot-workspace :around #'my/jdtls-single-root)
  (advice-add 'lsp-java--ls-command :around #'my/lsp-java-per-project-workspace))

) ; ─── END DISABLED lsp-mode ─────────────────────────────────────────────────

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HubSpot Java Format (HSJF) — format on save via reformatter.el
;;
;; ~/.bin/hsjf handles jar/classpath/JVM-flag complexity.
;; reformatter.el pipes the buffer through it on save.
;;
;; Defines:
;;   hsjf-format-buffer        — reformat current buffer manually
;;   hsjf-format-on-save-mode  — minor mode toggled by the hooks below
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package reformatter
  :ensure t
  :config
  (reformatter-define hsjf-format
    :program (expand-file-name "~/.bin/hsjf")
    :args '("-")
    :lighter " HSJF"))

(add-hook 'java-mode-hook #'hsjf-format-on-save-mode)
(add-hook 'java-ts-mode-hook #'hsjf-format-on-save-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Eglot — built-in LSP client (Emacs 29+)
;; One server per project via project.el (default behavior).
;; JS/TS servers are hooked in the JS/TS section below.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eglot
  :ensure nil  ; built-in since Emacs 29
  :hook ((python-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  (read-process-output-max (* 3 1024 1024))
  :bind (:map eglot-mode-map
              ("M-."     . xref-find-definitions)
              ("M-,"     . xref-go-back)
              ("M-?"     . xref-find-references)
              ("C-c C-r" . eglot-rename)
              ("C-c C-a" . eglot-code-actions)
              ("M-9"     . flymake-show-buffer-diagnostics))
  :config
  (add-to-list 'eglot-stay-out-of 'format))

(use-package eglot-java
  :ensure t
  :hook ((java-mode    . eglot-java-mode)
         (java-ts-mode . eglot-java-mode)))

(use-package consult-eglot
  :ensure t
  :after (eglot consult)
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Go
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; In my-development.el
;; Make sure you have the Go LSP server installed: go install
;; golang.org/x/tools/gopls@latest
(use-package go-mode
  :ensure t
  :hook (go-mode . eglot-ensure))

;; Non-LSP go mode config
;; (defun my-go-mode-hook ()
;;   "My go mode hook."
;;   (add-hook 'before-save-hook 'gofmt-before-save) ; Call Gofmt before saving
;;   (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
;;   ;; Godef jump key binding                                                      
;;   ;; godef requires go get github.com/rogpeppe/godef
;;   (local-set-key (kbd "M-.") 'godef-jump)
;;   (flycheck-mode)

;;   (set (make-local-variable 'company-backends) '(company-go))
;;   (company-mode)

;;   (setq compile-command "go build -v && go test -v && go vet")
;;   (define-key (current-local-map) "\C-c\C-c" 'compile)
;;   (go-eldoc-setup)
;;   (setq gofmt-command "goimports")

;;   ;; for some reason GOPATH isn't getting caught when this runs in
;;   ;; init.el
;;   (exec-path-from-shell-initialize)
;;   )

;; (add-hook 'go-mode-hook 'my-go-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ruby
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rspec-mode
  :ensure t
  :hook ((ruby-mode . rspec-mode) (ruby-ts-mode . rspec-mode))
  :custom
  (rspec-command-options "--format documentation --drb")
  (rspec-use-bundler-when-possible nil)
  (rspec-use-rake-when-possible nil)
  (rspec-use-rvm t))

(defun my-ruby-mode-hook ()
  (setq require-final-newline nil)
  (auto-fill-mode -1)

  (ruby-end-mode)
  (require 'ruby-interpolation) ; electric #{} in ruby strings by entering #

  ;; http://stackoverflow.com/questions/6453955/how-do-i-prevent-emacs-from-adding-coding-information-in-the-first-line
  (setq ruby-insert-encoding-magic-comment nil)
  ;; highlight beginging of blocks when point is on end
  ;; Requires ruby-end package
  (setq ruby-block-highlight-toggle 'overlay) ; other option is minibuffer or t for both

  (local-set-key "\r" 'newline-and-indent)
  
  ;; emacs ruby mode defaut is global but (default) is the default in rvm now
  (setq rvm--gemset-default "(default)")

  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))

  (ad-activate 'rspec-compile)

  ;; bundle exec for some reason doesn't pick up the same bundle as bundle installed on the
  ;; command line, however modern versions of rvm just pick it up without the bundle exec
  ;; command so leave it off and it all works fine. 
  (setq rspec-use-bundler-when-possible nil)

  ;; Set emacs environment to use the default rvm ruby
  (rvm-use-default)

  ;; align rules for ruby code
  ;; https://github.com/jimweirich/emacs-setup-esk/blob/master/ruby-align.el
  (add-to-list 'align-rules-list
               '(ruby-comma-delimiter
                 (regexp . ",\\(\\s-*\\)[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-hash-literal
                 (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
                 (group 2 3)
                 (repeat . t)
                 (modes  . '(ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-hash-literal2
                 (regexp . "[a-z0-9]:\\(\\s-*\\)[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-assignment-literal
                 (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-xmpfilter-mark
                 (regexp . "\\(\\s-*\\)# => [^#\t\n]")
                 (repeat . nil)
                 (modes  . '(ruby-mode))))
  )



(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'ruby-ts-mode-hook 'my-ruby-mode-hook)

(defun update-ruby-tags ()
  "Find the project root using projectile and if it looks like a ruby project then update the tags
in an asynchronous process.

Dependent emacs packages:
- projectile for finding project root

Dependent gems:
- ripper-tags a much better ruby tagger than ctags"
  (interactive)
  (let ((project-root (funcall 'projectile-project-root)))
    (if (and project-root
             (or (file-exists-p (expand-file-name ".ruby-version" project-root ))
                 (file-exists-p (expand-file-name "Gemfile" project-root ))
                 ))

        (let* ((default-directory project-root)
               ;;(ripper-tags-executable (concat (getenv "HOME") "/.rvm/bin/rvm " default-ruby-version " do ripper-tags"))
               (ripper-tags-executable "rvm default do ripper-tags")
               (ruby-tags-command (concat "BUNDLE_GEMFILE='' " ripper-tags-executable " -R --exclude=db/migrate --exclude=db --exclude=vendor --exclude=lib/one_offs --exclude=spec --tag-file=TAGS"))
               )
          (message (concat "Updating tags file in " project-root " with command " ruby-tags-command))
          (call-process-shell-command (concat ruby-tags-command "&") nil 0)
          ;;same command but use this instead to debug any problems as output goes to a buffer
                                        ;(async-shell-command ruby-tags-command nil)
          )
      )
    )
  )

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'update-ruby-tags nil t)))
(add-hook 'ruby-ts-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'update-ruby-tags nil t)))


(add-to-list 'auto-mode-alist '("\\.rb" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec" . ruby-ts-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-ts-mode))
(use-package inf-ruby
  :commands (inf-ruby run-ruby inf-ruby-setup-keybindings))

(use-package align
  :commands (align-regexp align-entire align-current align-newline-and-indent))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Unix shell scripts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-sh-mode-hook ()
  (setq indent-tabs-mode nil)
  (cond ((equal (point-max) 1)
         (create-boilerplate))))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-emacs-lisp-doc-defun ()
  (interactive)
  (beginning-of-defun)
  (end-of-line)
  (newline-and-indent)
  (insert "\"\"")
  (backward-char))

(defun my-emacs-lisp-mode-hook ()
  "Personal hook for emacs-lisp"
  (define-key emacs-lisp-mode-map [(control c) (d)] 'my-emacs-lisp-doc-defun)
  (define-key emacs-lisp-mode-map [(control m) ] 'newline-and-indent)
  )
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; JavaScript / TypeScript via tree-sitter (Emacs 29+)
;; js-ts-mode, tsx-ts-mode, typescript-ts-mode replace js2-mode/rjsx-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.gs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

(dolist (hook '(js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook))
  (add-hook hook #'add-node-modules-path)
  (add-hook hook (lambda ()
                   (setenv "PATH" (mapconcat 'identity exec-path ":"))))
  (add-hook hook #'eglot-ensure))

;; lsp-pyright disabled; eglot handles pyright directly (install: pip install pyright)
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-ts-mode . lsp-deferred))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C Mode - really? C?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-c-mode-hook ()
  "Hook for C-style languages -
 Used for c, c++,java-mode,installscript etc
"
  (let (gcc-compiler
        gcc-command)
    (cond ((string-equal major-mode "c-mode")
           (define-key c-mode-map [(control c) (h)] 'ff-find-other-file)
           (setq gcc-command "gcc -ggdb -Wall -o "))
          ((string-equal major-mode "c++-mode")
           (define-key c++-mode-map [(control c) (h)] 'ff-find-other-file)
           (setq gcc-command "g++ -ggdb -Wall -o "))
          ((string-equal major-mode "java-mode")
           (setq gcc-command "javac ")))
    (cond ((and (not(or (file-exists-p "makefile")
                        (file-exists-p "Makefile")))
                buffer-file-name)
           ;; if there is a makefile and buffer-file-name is non-nil
           (set (make-local-variable 'compile-command)
                (concat gcc-command
                        (file-name-sans-extension buffer-file-name) " "
                        (buffer-file-name))))
          ((or (file-exists-p "makefile")
               (file-exists-p "Makefile"))
           (set (make-local-variable 'compile-command)
                "make")))
    (setq compilation-read-command nil)
    (setq comment-auto-fill-only-comments t) ; autofill in comments only
    (define-key global-map [(control c)(control v)(control c)] 'compile))
  (set-indent))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'java-mode-hook 'my-c-mode-hook)
(add-hook 'java-mode-hook 'set-indent-java t)
(add-hook 'java-ts-mode-hook 'set-indent-java t)

;; GObject preprocessor language
(add-to-list 'auto-mode-alist '("\\.gob\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.CPP\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]ul" . installscript-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XML style markup languages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-nxml-mode-hook ()
  (set-indent)
  (define-key nxml-mode-map [(control m)] 'newline-and-indent))
(rng-nxml-mode-init)
(add-to-list 'auto-mode-alist '("\\.xml" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.svg" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.wsdd" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml" . nxml-mode))
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.htm\\'"))
(add-to-list 'auto-mode-alist '("\\.ism" . nxml-mode)) ; install shield
(add-to-list 'auto-mode-alist '("\\.cfg" . nxml-mode)) ; OCCI install config
                                        ;(add-to-list 'auto-mode-alist '("\\.rhtml" . nxml-mode)) ; OCCI install config
(add-hook 'nxml-mode-hook 'set-indent)

;; XSLT-process mode
(autoload 'xslt-process-mode "xslt-process" "Emacs XSLT processing" t)

;; DTD mode
(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'dtd-etags "tdtd"
  "Execute etags on FILESPEC and match on DTD-specific regular expressions."
  t)
(autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

;; Turn on font lock when in DTD mode
(add-hook 'dtd-mode-hooks
          'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
        '("\\.dcl$" . dtd-mode)
        '("\\.dec$" . dtd-mode)
        '("\\.dtd$" . dtd-mode)
        '("\\.ele$" . dtd-mode)
        '("\\.ent$" . dtd-mode)
        '("\\.mod$" . dtd-mode))
       auto-mode-alist))


(autoload 'longlines-mode "longlines"
  "Minor mode for editing long lines." t)

(provide 'my-development)
(message "Done loading my-development.el")

;; Local Variables:
;; byte-compile-warnings: (not free-vars obsolete)
;; End:
