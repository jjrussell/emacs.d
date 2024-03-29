;; This file contains modes for editing different programming languages
;; that do not come standard with emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General purpose development tools
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-aggressive-indent-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; git support
;; Mosty use a combination of egg and magit with their default settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require-try 'egg)
;;(require-try 'vc-git) ; meh
(use-package magit
  :commands magit-status)

(defun shell-git-status(dir)
  (interactive)
  "Function called from emacsclient to show git status from pwd"
  (find-file dir)
  (magit-status)
  (delete-other-windows)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lsp-mode - condfig lifted from https://raw.githubusercontent.com/neppramod/java_emacs/master/emacs-configuration.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; default auto-completion for lsp-mode
(use-package company :ensure t)
(use-package yasnippet :config (yas-global-mode))
(use-package yasnippet-snippets :ensure t)
(use-package flycheck :ensure t :init (global-flycheck-mode))
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list)))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))
(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 1.5
	      lsp-ui-doc-position 'bottom
              lsp-ui-doc-max-width 100
	      ))
(use-package helm-lsp
  :ensure t
  :after (lsp-mode)
  :commands (helm-lsp-workspace-symbol)
  :init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))
(use-package lsp-mode
  :ensure t
  :hook (
	 (lsp-mode . lsp-enable-which-key-integration)
	 (java-mode . #'lsp-deferred)
	 )
  :init (setq
	 lsp-keymap-prefix "C-c l"              ; this is for which-key integration documentation, need to use lsp-mode-map
	 lsp-enable-file-watchers nil
	 read-process-output-max (* 1024 1024)  ; 1 mb
	 lsp-completion-provider :capf
	 lsp-idle-delay 0.500
	 )
  :config
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  ;;(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  )
(use-package lsp-java 
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Go
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun my-go-mode-hook ()
  "My go mode hook."
  (add-hook 'before-save-hook 'gofmt-before-save) ; Call Gofmt before saving
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  ;; Godef jump key binding                                                      
  ;; godef requires go get github.com/rogpeppe/godef
  (local-set-key (kbd "M-.") 'godef-jump)
  (flycheck-mode)

  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  
  (setq compile-command "go build -v && go test -v && go vet")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")

  ;; for some reason GOPATH isn't getting caught when this runs in
  ;; init.el
  (exec-path-from-shell-initialize)
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ruby
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-ruby-version "2.1.5")

(defun my-ruby-mode-hook ()
  (interactive)
  (setq require-final-newline nil)
  (require 'rspec-mode)
  (auto-fill-mode -1)

  ;; rsense suddenly got unstable 2013-09-07 would hang on autocomplete
  ;; (setq rsense-home (concat my-site-lisp-dir "/rsense-0.3"))
  ;; (add-to-list 'load-path (concat rsense-home "/etc"))
  ;; (require 'rsense)
  ;; (add-to-list 'ac-sources 'ac-source-rsense-method)
  ;; (add-to-list 'ac-sources 'ac-source-rsense-constant)
  (ruby-end-mode)
  (require 'ruby-interpolation) ; electric #{} in ruby strings by entering #

  ;; http://stackoverflow.com/questions/6453955/how-do-i-prevent-emacs-from-adding-coding-information-in-the-first-line
  (setq ruby-insert-encoding-magic-comment nil)
  ;; highlight beginging of blocks when point is on end
  ;; Requires ruby-end package
  (setq ruby-block-highlight-toggle 'overlay) ; other option is minibuffer or t for both

  (local-set-key "\r" 'newline-and-indent)
  (flymake-ruby-load)
  
  (ac-etags-setup)
  (ac-etags-ac-setup)
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


(add-to-list 'auto-mode-alist '("\\.rb" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec" . ruby-mode))
(add-to-list 'interpreter-mode-alist  '("ruby" . ruby-mode))
(use-package ruby-mode)
(use-package inf-ruby
  :commands (inf-ruby run-ruby inf-ruby-setup-keybindings))

(use-package align
  :commands (align-regexp align-entire align-current align-newline-and-indent))


;; Modified slightly from
;; http://stackoverflow.com/questions/4412739/emacs-ruby-mode-indentation-behavior
;; This makes functions args not wrapped in parens to indent subsequent args on
;; newlines two spaces from the original method call.
;; 2013-05-14 New versino of ruby-mode came out. trying without this.
(defadvice ruby-indent-line (after line-up-args activate)
  (let (indent prev-indent arg-indent)
    (save-excursion
      (back-to-indentation)
      (when (zerop (car (syntax-ppss)))

        (setq indent (current-column))
        (skip-chars-backward " \t\n")
        (when (eq ?, (char-before))
          (ruby-backward-sexp)
          (back-to-indentation)
          (setq prev-indent (current-column))
          (skip-syntax-forward "w_.")
          (skip-chars-forward " ")
          (setq arg-indent (current-column)))))
    (when prev-indent
      (let ((offset (- (current-column) indent)))
        (cond ((< indent prev-indent)
               (indent-line-to prev-indent))
              ((= indent prev-indent)
               ;; this line will indent the next line arg under the arg
               ;; on the first line.  The change I made indents the next line
               ;; arg to two more than the method call
               ;; (indent-line-to arg-indent)))
               (indent-line-to (+ 2 prev-indent))))
        (when (> offset 0) (forward-char offset))))))

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
(add-hook 'sh-set-shell-hook 'flymake-shell-load)
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
;; Javascript Mode
;; Relevant packages
;; js2-mode - fancier JS editor
;; rjsx-mode - built on js2-mode with JSX support. Use this instead
;; js2-refactor - refactoring
;; xref-js2 - tagging and searching
;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;; js-mode which js2 is based on binds "M-." which conflicts with xref, so
;; unbind it.
(eval-after-load 'rjsx-mode
  '(progn
     (js2r-add-keybindings-with-prefix "C-c C-r")
     (define-key js2-mode-map [(control k)] #'js2r-kill)

     (add-hook 'js2-mode-hook (lambda ()
                                (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

     (add-hook 'rjsx-mode-hook (lambda ()
                                 (add-node-modules-path)
                                 (setenv "PATH" (mapconcat 'identity exec-path ":"))))
     (require 'prettier-js)
     (add-hook 'rjsx-mode-hook #'prettier-js-mode)
     (add-hook 'rjsx-mode-hook (lambda ()
                                 (js2-highlight-vars-mode)
                                 ;; don't let highlight mode clobber my useful keybindings with their silly nonesense
                                 (setq js2-highlight-vars-local-keymap (make-sparse-keymap))))

     
     (define-key js-mode-map [(meta .)] 'js2-jump-to-definition) ; better than xref-find-definitions in JS
     (define-key js-mode-map (kbd "≥") 'js2-jump-to-definition) ; alt-. on mac. Duplicate to be consistent with intellij
     (define-key js-mode-map [(control meta .)] 'xref-find-definitions) ; backup to js2-jump-to-definition
     )) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CSS & SCSS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cool but compiles sass and scss which litters build artifacts in git
;; (add-hook 'sass-mode-hook 'flymake-sass-load)
;; (add-hook 'scss-mode-hook 'flymake-sass-load)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJS templates multi-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'mmm-auto)

(defun my-mmm-auto-mode-hook ()
  (interactive)
  (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
  (mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)
  ;; (mmm-add-mode-ext-class 'html-erb-mode nil 'html-js)
  ;; (mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)

  ;;nXML as primary mode (supports only JS and CSS subregions):
  (mmm-add-mode-ext-class 'nxml-web-mode nil 'html-js)
  (mmm-add-mode-ext-class 'nxml-web-mode nil 'html-css)
  )

(setq mmm-global-mode 'auto)
(add-hook 'mmm-major-mode-hook 'my-mmm-auto-mode-hook)
(add-to-list 'auto-mode-alist '("\\.xhtml\\'" . nxml-web-mode))

(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))

(setq mmm-submode-decoration-level 1
      mmm-parse-when-idle t)

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
(add-to-list 'auto-mode-alist '("\\.html" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.htm" . nxml-mode))
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
