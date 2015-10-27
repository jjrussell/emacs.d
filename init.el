;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Personal variables paths and stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External programs
;; My own terminal script wrapper for xplatform terminal fun
(setq my-terminal-program "my-terminal")
(setq my-terminal-program-args nil)

;; My own terminal script wrapper for xplatform file browsing fun
(setq my-filemanager-progam "my-file-browser")

;; set root of personal emacs repository and common directories used
;; in functions and other variables below
(setq user-emacs-directory (expand-file-name ".emacs.d" "~")
      my-emacs-init-dir (expand-file-name "init" user-emacs-directory)
      my-site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)

      ;;Place where local stuff is kept e.g. desktop definitions
      my-emacs-local-store (expand-file-name ".emacs.local" "~"))


;; Just in case these directories aren't there yet create them.
;; If localstore doesn't exsist emacs won't exit correctly, which is really annoying
(mapcar (lambda (dir) (make-directory dir t))
        (list user-emacs-directory
              my-emacs-local-store
              (expand-file-name  "semantic.cache" my-emacs-local-store)))

;; put the buffer name in the frame title
;; system-name returns the FIRST name for 127.0.0.1 in /etc/hosts
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; System Checks
;;Are we on UNIX?
(setq my-unixp (or (eq system-type 'gnu/linux)
                    (eq system-type 'linux)
                    (eq system-type 'darwin)
                    (eq system-type 'usg-unix-v))) ; solaris, blargh


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; enable/disable/customize for builtin functionality
;; Most of this could probably go into emacs' built in customize tool
;; but some have comments and meh. Not worth it to move it.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(subword-mode 1) ; makes next-word etc work on camel case words
(cond (window-system (global-hl-line-mode 1)))  ; highlight current line. Looks terrible in terminal
(column-number-mode t)        ; show column number next to line number

;;  used to use this to delete highlighted region when you backspace or start typing
;;  I'm still on the fence as to whether or not I like that.
;;  Nothing else cua does (C-c for copy C-x for paste) is useful to me.
;;  Hrm. actually delete-selection-mode does this. What was CUA for again?
;;(cua-mode -1)

;; hide mousey controls. You're using emacs. Seriously.
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1) ; WHY WOULD ANYONE DO THAT!? WITH THE BLINKING AND THE FLASHING!!


;; Removing whitespace on save is a good idea but it produces a lot of noise in diffs with other people's files
;; (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
;; (add-hook 'focus-out-hook 'save-buffer) ; save buffers when emacs loses focus

(grep-compute-defaults) ;detects grep system capabilities
(menu-bar-enable-clipboard) ; merges native x clipboard and emacs yank.  Why isn't this the default?

(setq-default indent-tabs-mode nil)     ; use spaces instead of tabs #flameon
(setq
 x-select-enable-clipboard t ; use native clipboard for yanking
 font-use-system-font t
 make-backup-files nil            ; no annoying ~files
 ring-bell-function 'ignore     ; Blessed silence
 query-replace-highlight t
 completion-ignore-case t               ;ignore case on completions
 read-file-name-completion-ignore-case t ;ignore case on find-file
 inhibit-startup-message t        ; don't show splash screen
 max-lisp-eval-depth 999               ; fixes ecb in deep directories
 line-number-display-limit-width 500 ; still show line numbers with long lines
 require-final-newline nil ; adds new lines to the end of buffers
 next-line-add-newlines nil ; adds new lines to the end of buffers
 mouse-yank-at-point t          ; yank at point, not at click, but who uses the mouse anyway?
 yank-at-point t               ; yank at point, not where the mouse is
 auto-save-directory (concat my-emacs-local-store "/auto-save-list")
 auto-save-list-file-prefix ".saves-"
 parens-require-spaces nil ; no whitespace padding around parens
 ns-pop-up-frames nil ; don't open new frames when the OS uses emacs to open a file
 confirm-kill-emacs 'y-or-n-p ; prompt to exit
 )
(fset 'yes-or-no-p 'y-or-n-p) ; Make all "yes or no" prompts "y or n" instead
;; this fixes the meta key with ubuntu on the macbook
;;(setq x-super-keysym 'meta)

(electric-indent-mode 1) ; global minor mode for auto indent on newline

;; pretty font colors
(require 'font-lock)
(font-lock-mode t)
(global-font-lock-mode t)
(set 'font-lock-maximum-decoration t) ; Maximum colors. All the way to 11

;; keep a list of recently opened files.
(recentf-mode t)
(define-key global-map [(control x) (control meta f)] 'recentf-open-files)

;; turn on default disabled functionality
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Use autopair instead. Electric pair blows
;; Elecric pair mode even adds double quotes in comments. WTF? Fixed by defadvice below
;; Still, using autopair package. Its better
;; (electric-pair-mode 1)   ; global minor mode for auto pair parens and such

;; helper functions for making electric-pair-mode suck less, but we're using
;; autopair instead which does all of this
;; (defun point-at-string-or-comment-p ()
;;   "Helper function for advice below to determine if we are in a string or comment"
;;   (or (consp (memq 'font-lock-string-face (text-properties-at (point))))
;;       (consp (memq 'font-lock-comment-face (text-properties-at (point))))))

;; (defadvice electric-pair-post-self-insert-function (around
;;                                                     electric-pair-apostrophe
;;                                                     activate)
;;   "Fix electric-pair to handle apostrophes inside strings or comments"
;;   (if (not (eq last-command-event ?\'))
;;       ;; Only deal with single-quote characters
;;       ad-do-it
;;     (let* ((char-before-insert (char-before (1- (point))))
;;         (char-before-syntax (and char-before-insert
;;                                  (char-syntax char-before-insert))))
;;       ;; We want the character before the self-insert-command
;;       (if (and (point-at-string-or-comment-p)
;;             (eq char-before-syntax ?w))
;;        ;; Single-quotes inside a string or comment, and immediately
;;        ;; following a word, are actually apostrophes and should not
;;        ;; be paired.
;;        t
;;      ad-do-it))))

;; (defadvice delete-backward-char (before
;;                                  delete-double-quotes
;;                                  activate)
;;   "If the point is between two quotes, delete-backward-char will delete both"
;;   (let* ((char-after-point (char-after (point)))
;;          (char-before-point (char-before (point)))
;;          (double-quote ?\"))
;;     (when (and
;;            (eq char-after-point double-quote)
;;            (eq char-before-point double-quote))
;;       (delete-char 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom keybindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set my standard prefix for personal key bindings
;; so they don't clobber anything else
(defvar my-prefix-map (make-sparse-keymap))
(define-key global-map [(control x) (d)] my-prefix-map)

(define-key global-map [(control x) (!)] 'shell-command)
(define-key global-map [(control x) (f1)] 'shell-toggle)
(define-key global-map [(control x)(control r)] 'revert-buffer)
(define-key global-map [(meta r)] 'replace-string)
(define-key global-map [(meta g)] 'goto-line)
(define-key global-map [(control x)(meta f)] 'find-file-at-point)
(define-key my-prefix-map "t" 'org-capture)
; (define-key my-prefix-map "t" 'whitespace-cleanup)
(define-key my-prefix-map "k" 'scroll-bar-mode)
(define-key my-prefix-map "m" 'menu-bar-mode)
(define-key my-prefix-map "g" 'grep)
(global-set-key (kbd "C-c q") 'auto-fill-mode) ; toggle auto-fill-mode
(define-key my-prefix-map [(meta p)] 'fill-paragraph)
(define-key my-prefix-map [(control meta p)] 'fill-region)
(define-key global-map [3 9] (lambda () (interactive)
                               (insert-tab))) ; Control-C TAB as a vector
(define-key global-map [(control shift k)] 'kill-this-buffer)

(define-key global-map [(control x)(^)] 'join-line)

(define-key my-prefix-map "b" 'browse-url-at-point)
(define-key my-prefix-map "R"
  '(lambda ()
     "Reload .emacs file and recompile any init files that need it."
     (interactive)
     ;; Make sure that the newest versions of init files are compiled.
     (byte-recompile-directory my-emacs-init-dir 0)
     ;;(byte-recompile-directory my-emacs-local-store)
     ;; don't force recompile, compile if no .elc is present and load file when done
     (byte-recompile-file user-init-file nil 0 t)
     (my-after-init-hook)
     ))
(define-key my-prefix-map "e"
  '(lambda () "Open .emacs file." (interactive)
     (find-file user-init-file)))
(define-key my-prefix-map "j"
  '(lambda () "Open .bashrc file." (interactive) (find-file "~/.bashrc")))
(define-key my-prefix-map "J"
  '(lambda () "Open .bashrc file." (interactive) (find-file "~/.jshrc")))
(define-key my-prefix-map "r"
  '(lambda () "Open TODO file." (interactive) (find-file "~/Dropbox/RIGHT_NOW.txt")))

;; default is now C-h e
(define-key global-map [(control h) (e)]
  '(lambda () (interactive) (switch-to-buffer "*Messages*")))
(define-key my-prefix-map "s"
  '(lambda () (interactive) (switch-to-buffer "*scratch*")))

;; don't use anymore.  Use wmctrl -r :ACTIVE: -e '0,0,-1,1170,-1' instead
;; Works on all types of windows
;; Still here because I'm used to it and it works on Windows as well
(define-key my-prefix-map "w"
  '(lambda () (interactive) (set-frame-width (selected-frame)100)))
(define-key my-prefix-map [(meta w)]
  '(lambda () (interactive) (set-frame-width (selected-frame) 200)))
(define-key my-prefix-map [(meta h)]
  '(lambda () (interactive) (set-frame-height (selected-frame) 87)))
(define-key my-prefix-map [(meta b)]
  '(lambda () (interactive) (set-frame-height (selected-frame) 87)(set-frame-width (selected-frame) 200)))
(define-key my-prefix-map [(control meta h)]
  '(lambda () (interactive)
     (set-frame-height (selected-frame) (+(frame-height) 10)) ))

(global-set-key "\M-`" 'other-frame) ; mac-like frame switching behavior
                                        ;(global-set-key "\M-`" 'next-multiframe-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom auto-mode for common file extensions/patterns
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.bat" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash*" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.properties" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.ebuild" . sh-mode))
(add-to-list 'auto-mode-alist '("/etc/*" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.conf" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.jshrc" . sh-mode))
(add-to-list 'auto-mode-alist '("XF86Config*" . sh-mode))
(add-to-list 'auto-mode-alist '("xorg.conf*" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.emacs*" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("MAKEFILE" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.mak" . makefile-mode))
(add-to-list 'auto-mode-alist '("ChangeLog" . change-log-mode))
(add-to-list 'auto-mode-alist '("notes\\.txt" . outline-mode))
(add-to-list 'auto-mode-alist '("httpd.conf" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.ds" . lisp-mode)) ; devilspie config
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom load path discovery and loading of 3rd party files and packages
;; Up until this point we've just customized built in emacs stuff.
;;
;; Load these before OS/Platform specific settings so we can use
;; exec-path-from-shell-initialize to load env vars for OSX which is too stupid to do it by itself
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Add tools to the end of load-path so things included in emacs are used first
(add-to-list 'load-path my-emacs-init-dir)
(add-to-list 'load-path my-emacs-local-store)

;; List all directories in my site-lisp packages directory
;; Add any of those directories to the load path
;; This is mostly superceded by ELPA packages now.
(cond ((file-directory-p my-site-lisp-dir)
       (mapcar (lambda (load-path-dir)
                 (let ((site-lisp-entry-nolisp (expand-file-name
                                                load-path-dir
                                                my-site-lisp-dir))
                       (site-lisp-entry-lisp (expand-file-name
                                              (concat load-path-dir "/lisp")
                                              my-site-lisp-dir)))

                   (if (file-directory-p site-lisp-entry-lisp)
                       ;;then
                       (add-to-list 'load-path site-lisp-entry-lisp)
                     ;;else just add the directory
                     (add-to-list 'load-path site-lisp-entry-nolisp))
                   ))
               (directory-files my-site-lisp-dir nil "^\\w")
               )
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ELPA Packages <3 <3 <3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make packages avaialble during init. By default they are loaded after init.el loads
(package-initialize)

;; Make sure that this list of packages are always installed. 
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages  '(ac-etags ack ack-and-a-half ag anything async auto-complete auto-indent-mode autopair bm browse-kill-ring buffer-move bundler coffee-mode col-highlight color-identifiers-mode color-theme color-theme-sanityinc-solarized company concurrent crontab-mode csv-mode ctable dash dash-functional deferred dired-single duplicate-thing ecb edit-server egg enh-ruby-mode ensime epc epl etags-table exec-path-from-shell expand-region f find-file-in-project findr floobits flx flx-ido flycheck flycheck-pyflakes flymake flymake-coffee flymake-cursor flymake-easy flymake-go flymake-jslint flymake-json flymake-python-pyflakes flymake-ruby flymake-sass flymake-shell fold-dwim fold-this fuzzy gh gist git-commit-mode git-rebase-mode git-timemachine github-browse-file gmail-message-mode go-autocomplete go-mode groovy-mode ham-mode haml-mode helm helm-themes highlight html-to-markdown hungry-delete ibuffer-vc ido-ubiquitous ido-vertical-mode idomenu imenu-anywhere inf-ruby inflections ioccur jira json-mode json-reformat json-snatcher jump key-chord kill-ring-search let-alist logito lua-mode magit magit-filenotify magit-find-file magit-gh-pulls magit-gh-pulls magit-log-edit markdown-mode+ markdown-mode mmm-mode multiple-cursors osx-plist pager pcache persp-projectile perspective pkg-info popup pos-tip projectile projectile-rails python-mode rake rbenv real-auto-save rinari robe rsense rspec-mode ruby-block ruby-compilation ruby-end ruby-interpolation ruby-tools rvm s sbt-mode scala-mode2 scss-mode smart-tab smex sr-speedbar tabulated-list toggle-quotes undo-tree vertica visual-fill-column vline wgrep window-numbering writeroom-mode xml-rpc yafolding yaml-mode yasnippet )
  "List of all packages used in my config. The code below will check to make sure all of these are installed before continuing. This keeps me from having to commit all my elpa packages to my own emacs git repo. 
Found on https://news.ycombinator.com/item?id=5459921"
  )
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; loading and customization for third party packages, elpa and otherwise
(require 'my-tools)

;; Load programming language modes and heavier dev tools
(require 'my-development)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; OS/Platform specific settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(cond (my-unixp
       ;; Try to turn the mode-line red when we are running as root
       ;; This doesn't always work as UID is not exported and USER isn't
       ;; updated when you use 'su'.
       (if (or (string= (getenv "USER") "root")
               (string= (getenv "UID") "0")
               (string= (getenv "EUID") "0")
               (string= (getenv "HOME") "/root"))
           (set-face-background 'mode-line "red"))

       ;; Don't run the emacs server if we are running minimal emacs config
       (cond ((not (getenv "EMACS_MINIMAL"))
              ;; Emacs server starts a listener on emacs that emacsclient can send files to for opening.
              ;; This lets you have just one emacs running and everything else sends stuff to it.
              ;; 
              ;; Calls emacsclient to check if another instance of emacs
              ;; is already running a server. The lambda is the process-filter
              ;; which gets the output and calls start-server if no other emacs server
              ;; was found by the emacsclient process.
              (let ((process-connection-type nil))
                (set-process-filter
                 (start-process "my-process" nil "emacsclient" "--eval" "t")
                 (lambda (process output)
                   (if (equal output "t\n")
                       (message "Not starting server, one instance already running.")
                     (message "Starting emacs server...")
                     (server-start)))
                 ))))

       ;; OSX. Mostly a unix, but still needs some help
       (cond ((eq system-type 'darwin)
                 
              ;; actually makes the command key behave on the mac. Its awesome
              ;; most of this good stuff is found at
              ;; http://lojic.com/blog/2010/03/17/switching-from-carbonemacs-to-emacs-app/
              (setq mac-option-key-is-meta nil)
              (setq mac-command-key-is-meta t)
              (setq mac-command-modifier 'meta)
              (setq mac-option-modifier nil)

              ;; use spotlight instead of locate command to find stuff
              (setq locate-command "mdfind")

              ;; lets emacs find stuff on the path.  This is only necessary on OSX because on
              ;; linux emacs inherits the shell variables we set in our .bashrc config file. On OSX
              ;; applications do not inherit the shell environment. WTF Apple? Seriously.
              (exec-path-from-shell-initialize)
              (setenv "PATH" (mapconcat 'identity exec-path ":"))
              (setenv "LANG" "en_US.UTF-8")
              (setenv "LC_CTYPE" "en_US.UTF-8")

              ;; this puts windows in the background on the mac
              ;; who needs mark paragraph anyway?
              (global-set-key (kbd "M-h") 'ns-do-hide-emacs)

              ;; I want this back to switch between emacs windows
              ;; it was mapped to tmm-menubar.  Whatever
              (global-unset-key [?\M-`])

              ;; if emacs is launched from spotlight or quicksilver it gets the
              ;; default-directory / which is useless. Set it to home.
              (unless (string-match (getenv "HOME") default-directory)
                (cd (getenv "HOME"))
                )
              ))
       ;; end of OSX specific
       )
      ;;End of UNIX specific
      ((eq system-type 'windows-nt)
       ;; put windows specific stuff here.
       ;; Most of this assumes Cygwin is installed.

       ;; Setup Emacs to run bash as its primary shell.
       (setq shell-file-name "/bin/bash"
             shell-command-switch "-c"
             explicit-shell-file-name shell-file-name
             explicit-sh-args '("-login" "-i"))
       (setenv "SHELL" shell-file-name)

       (if (boundp 'w32-quote-process-args)
           (setq w32-quote-process-args ?\"))
       ;; Start gnuserv if available for reuse of one emacs window
       (setq server-program
             (expand-file-name "gnuserv/gnuserv.exe" my-site-lisp-dir))

       (cond ((file-readable-p server-program)
              (require-try 'gnuserv)
              (gnuserv-start)
              (setq gnuserv-frame (selected-frame))))
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End initial configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Custom function definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All of these next three are superceded by projectile
;; (defun find-source-project ()
;;   "let me pick a project from my source directory"
;;   (interactive)
;;   (dired (read-directory-name "Select project directory:" my-src nil t nil))
;;   )
;; (global-set-key [(control x) (meta p)]  'find-source-project)

;; (defun ffip-prompt ()
;;   "prompt for a directory in source directory and run find-file-in-project with that as the root"
;;   (interactive)
;;   (let ((default-directory (read-directory-name "Select project directory to find file:" my-src nil t nil)))
;;     ;;(find-file-in-project)
;;     (projectile-find-file)
;;     )
;;   )
;; (global-set-key [(control x) (p)]  'ffip-prompt)

;; (defun ack-prompt ()
;;   (interactive)
;;   (ack-and-a-half nil (read-directory-name "Select project directory to ack:" my-src nil t nil))
;;   )
;; (global-set-key [(control x) (d) (meta a)]  'ack-prompt)

;; set split window keys to behave like tmux
(global-set-key (kbd "C-x -")  'split-window-vertically)
(global-set-key (kbd "C-x |")  'split-window-horizontally)
(global-set-key (kbd "C-x \\")  'split-window-horizontally)

;; override defaults to select the newly created buffer when splitting
(global-set-key (kbd "C-x 2")  'split-window-vertically)
(global-set-key (kbd "C-x 3")  'split-window-horizontally)

(global-set-key (kbd "<C-S-right>")  'windmove-right)
(global-set-key (kbd "<C-S-left>")  'windmove-left)
(global-set-key (kbd "<C-S-up>")  'windmove-up)
(global-set-key (kbd "<C-S-down>")  'windmove-down)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(global-set-key [(meta shift up)]  'move-line-up)

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(meta shift down)]  'move-line-down)

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(global-set-key (kbd "C-c t") 'visit-term-buffer)

(defun my-home ()
  "Home - begin of text, once more - begin of line, once more - screen,
once more - buffer."
  (interactive)
  (cond
   ((and (eq last-command 'my-home) (eq last-last-command 'my-home)
         (eq last-last-last-command 'my-home))
    (goto-char (point-min)))
   ((and (eq last-command 'my-home) (eq last-last-command 'my-home))
    (move-to-window-line 0))
   ((eq last-command 'my-home)
    (beginning-of-line))
   (t (beginning-of-line-text)))

  (if (boundp 'last-last-command)
      (setq last-last-last-command last-last-command))
  (setq last-last-command last-command))

(defun my-end ()
  "End - end of line, once more - screen, once more - buffer."
  (interactive)
  (cond
   ((and (eq last-command 'my-end) (eq last-last-command 'my-end))
    (goto-char (point-max)))
   ((eq last-command 'my-end)
    (move-to-window-line -1)
    (end-of-line))
   (t(end-of-line)))
  (setq last-last-command last-command))
;; override default keybindings
(define-key global-map [?\C-a] 'my-home)
(define-key global-map [?\C-e] 'my-end)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert whatever
key to which this function is bound.  Usually '%'."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(define-key global-map "%" 'match-paren)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-Insert Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; found https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el
;; mentioned in http://emacsrocks.com/e13.html about multiple curosrs
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(define-key global-map [(control x)(control shift e)] 'eval-and-replace)

(defun my-open-line ()
  "Makes open line indent the line you were on before."
  (interactive )
  (beginning-of-line-text)
  (setq cc (current-column))
  (save-excursion
    (beginning-of-line)
    (open-line 1))
  ;; condition is for correctly handling blank lines
  (if (not (eq cc 0)) (previous-line))
  (dotimes (number cc)
    (insert " "))
  )

(define-key global-map [(control o)] 'my-open-line)

;; insert date in various formats
(defun insert-full-date-and-time ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y %k:%M:%S")))
(define-key my-prefix-map [(control meta i)] 'insert-full-date-and-time)

(defun insert-long-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))
(define-key my-prefix-map [(control i)] 'insert-long-date)

(defun insert-short-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(define-key my-prefix-map [(i)] 'insert-short-date)

(defun insert-time ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%k:%M:%S")))
(define-key my-prefix-map [(meta i)] 'insert-time)

(defun insert-filename (&optional arg)
  "Insert the current filename at point."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (cond (filename
           (if arg
               (insert filename)
             (insert (file-name-nondirectory filename)))))))

(defun toggle-comment-on-line-or-region (&optional arg)
  "comment or uncomment current line or selected region , and go to the next line
https://github.com/kaushalmodi/.emacs.d/blob/13bc1313e786ce1f1ab41d5aaff3dc39dfc57852/setup-files/setup-editing.el#L110-117
Found in comment of http://endlessparentheses.com/implementing-comment-line.html?source=rss"
  (interactive)
  (if (region-active-p)
      (comment-dwim arg)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (next-line))
(global-set-key (kbd "M-;") #'toggle-comment-on-line-or-region)

(defun my-comment-block (&optional arg)
  "Write a big comment block to the screen. Uses newcomment package"
  (interactive "P")
  (indent-according-to-mode)
  (if (or (not comment-start) (not (boundp 'comment-start)))
      (setq comment-start "* "))
  (insert (comment-padright comment-start 60))

  ;; newline-indent won't work without this.  If it isn't set, set it
  ;; temporarily and reset when we're done.
  (cond ((boundp 'c-syntactic-indentation)
         (setq c-syntactic-indentation-old c-syntactic-indentation)
         (setq c-syntactic-indentation t)))

  ;; Not all modes have comment-continue, if it's nil, use comment-start
  (let ((filler comment-start))
    ;; If comment-continue isn't set, get its value from the second half
    ;; of comment-start.  This code was lifted from newcomment.el
    (unless (or comment-continue (string= comment-end ""))
      (set (make-local-variable 'comment-continue)
           (concat (if (string-match "\\S-\\S-" comment-start) " " "|")
                   (substring comment-start 1))))
    (if comment-continue
        (setq filler comment-continue))

    (newline-and-indent)
    ;; With a prefix arg do the small comment block
    (cond ((not arg)
           (insert (comment-padright filler 1)) (newline-and-indent)))

    (insert (comment-padright filler 1)) (newline-and-indent)

    (cond ((not arg)
           (insert (comment-padright filler 1)) (newline-and-indent)))

    (if (string= comment-end "")        ; if there is no comment end
        (insert (comment-padright comment-start 60)) ; use comment start
      (insert (comment-padleft comment-end 60)))) ; else close the comment

  (cond ((boundp 'c-syntactic-indentation)
         (setq c-syntactic-indentation c-syntactic-indentation-old)))

  (cond ((not arg)
         (previous-line 1)))
  (previous-line 1)
  (end-of-line)
  (insert " "))

(define-key my-prefix-map "#" 'my-comment-block)

(defun my-tab-line (&optional arg)
  "Inserts a tab at the beginning of the current line.  Optional
argument removes a tab from the beginning of the line."
  (interactive)
  (let ((beg (point))
        (reset-point t))

    ;; if we are deleting a tab and we are the beginning of the line
    ;; don't reset the point, just leave it at the beginning
    (if (and (eq (char-before) 10) arg)
        (setq reset-point nil))
    (beginning-of-line)
    (if (not arg)
        (insert-tab)
      (cond ((eq (char-after) 9)        ; only delete tabs
             (delete-char 1)
             (setq beg (- beg 2)))))   ; if its not tab, don't add one
    (cond (reset-point
           (goto-char (+ beg (if indent-tabs-mode 1 tab-width)))))
    ))

(defun my-insert-outline-mode-local-variable ()
  "Add the text at the end of the current buffer that will make this buffer
load in outline mode when opened. "
  (interactive)
  (save-excursion
    ;; check for existing Local Variables: section
    (cond ((not (search-forward "Local Variables:" nil t nil))
           (goto-char (point-max))
           (newline)(newline)
           (insert "Local Variables:")(newline)
           (insert "mode:outline")(newline)
           (insert "End:")(newline))
          ((not (search-forward "mode:outline" nil t nil))
           ;; if there is a Local Variable: line but no outline mode,
           ;; re run the search to get to the end of Local Variables:
           ;; then insert mode:outline
           (goto-char (search-backward "Local Variables:" nil t nil))
           (end-of-line) (newline)
           (insert "mode:outline"))))

  (save-buffer))

;; (defun my-notes-send-to-journal ()
;;   "Kills the current line and puts it in the journal of my notes page
;; for today, creating today's entry if it doesn't exist."
;;   (beginning-of-line)
;;   (kill-line)

;;   )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Buffer formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
(global-set-key (kbd "C-c k") 'kill-other-buffers)

(defun my-indent-buffer ()
  "Indents the entire buffer according to the current mode."
  (interactive)
  (indent-region (point-min) (point-max) nil)
  (message "Buffer indented."))
(define-key my-prefix-map [(\\)] 'my-indent-buffer)

(defun fill-line ()
  "Fills the current line."
  (interactive)
  (point-to-register 'iwdw-register)
  (beginning-of-line)
  (let ((beg (point))) (forward-line 1) (fill-region beg (point)))
  (register-to-point 'iwdw-register))
(define-key global-map [(meta p)] 'fill-line)

(defun unwrap-line ()
  "Remove all newlines until we get to two consecutive ones.
    Or until we reach the end of the buffer.
    Great for unwrapping quotes before sending them on IRC."
  (interactive)
  (let ((start (point))
        (end (copy-marker (or (search-forward "\n\n" nil t)
                              (point-max))))
        (fill-column (point-max)))
    (fill-region start end)
    (goto-char end)
    (newline)
    (goto-char start)))

(defun unfill-paragraph (arg)
  (interactive "*P")
  (let (beg end)
    (forward-paragraph 1)
    (setq end (copy-marker (- (point) 2)))
    (backward-paragraph 1)
    (setq beg (point-marker))
    (forward-char)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))))

(defun unfill-region (arg)
  (interactive "*P")
  (let (beg end tmp)
    (setq end (region-end))
    (setq beg (region-beginning))
    (cond ((> beg end)
           (setq tmp beg)
           (setq beg end)
           (setq end tmp)))
    (setq end (- end 2))
    (goto-char beg)
    (forward-char)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))))

(defun unfill-buffer (arg)
  (interactive "*P")
  (let (beg end)
    (setq end (- (point-max) 2))
    (setq beg (point-min))
    (goto-char (point-min))
    (forward-char)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))))

(defun my-untabify-buffer ()
  "Run Untabify against the current buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (message "Untabify Complete"))
(define-key my-prefix-map [f9] 'my-untabify-buffer)

(defun my-tabify-buffer ()
  "Run Untabify against the current buffer."
  (interactive)
  (tabify (point-min) (point-max))
  (message "Tabify Complete"))
(define-key my-prefix-map [(shift f9)] 'my-tabify-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Management Settings and Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from Emacs Prelude http://stackoverflow.com/a/9414763/1666065
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun revert-all-buffers ()
  "Revert all visited files without confirmation."
  (interactive)
  (let ((buf-list (buffer-list)) current-buff)
    (while buf-list
      (setq current-buff (car buf-list))
      (if (buffer-file-name current-buff)
          (progn
            ;; (message "reverting file %s" (buffer-name current-buff))
            (set-buffer current-buff)
            (revert-buffer nil t)))
      (setq buf-list (cdr buf-list)))))

(defun toggle-window-dedicated ()
  "Toggle the dedicated state of the current window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))
(define-key my-prefix-map [(meta t)] 'toggle-window-dedicated)

(defun clean-buffers (arg)
  "Cleans temporary and old buffers as defined by the regexs in
clean-buffer-list.  If called with an ARG, then cleans up buffers specified
by the regexs in clean-buffers-extended-list."
  (interactive "P")
  (mapcar '(lambda (buffer)
             (let ((buffer (buffer-name buffer)))
               (mapcar '(lambda (re)
                          (if (string-match re buffer)
                              (progn
                                (message "Deleting %s" buffer)
                                (kill-buffer buffer))))
                       (append clean-buffer-list
                               (if arg
                                   clean-buffer-extended-list
                                 nil)))))
          (buffer-list)))
;; regular expressions of buffers to clean up
(setq clean-buffer-list '("sent \\(reply\\)\\|\\(mail\\) to" "Completions" "\\*Help\\*" "\\*Apropos\\*" "\\*compilation\\*" "\\*Compile-log*" "\\*Customize*" "\\*Backtrace\\*" "\\*Saved Directories Menu*" "\\*Ediff Registry*" "\\*Desktop Menu*" "\\*sawfish*" "\\*Shell Command Output*" "\\*dired*" "\\*grep\\*" "\\*info\\*" "\\*Occur*" "\\*mail\\*" "\\*tramp*" "\\*doc\\*" "\\*spurious\\*"))
(defvar clean-buffer-extended-list '("*Manual-")
  "List of extended regexs of buffers to clean up when M-x clean buffers
is called with an argument")
(define-key global-map [?\C-x ?d ?c] 'clean-buffers)

(defun diff-buffer-against-file (context)
  "diff the current [edited] buffer and the file of the same name"
  (interactive "P")
  (let (  ($file buffer-file-name)
          ($tempFile "/tmp/emacs.diff")
          ($tempBuffer "emacs.diff"))
    (delete-other-windows)
    (push-mark (point) t)
    (generate-new-buffer $tempFile)
    (copy-to-buffer $tempBuffer (point-min) (point-max))
    (set-buffer $tempBuffer)
    (write-file $tempFile)
    (shell-command
     (concat (if context "diff -c " "diff ") $file " " $tempFile))
    (kill-buffer $tempFile)
    (pop-mark)
    )
  )

(defun load-buffer ()
  "Try to load the lisp code in the current buffer."
  (interactive)
  (load-file (buffer-file-name)))

(defun require-try (&rest args)
  "Helpful function for loading things if they exist and not crashing if
they do not. Use require if args are symbols, load-library if they are strings,
fail with a message and continue if some aren't available"
  (let (lib)
    (condition-case err
        (mapcar (lambda (e)
                  (setq lib e)
                  (cond
                   ((stringp e) (message "using load-library")(load-library e))
                   ((symbolp e) (require e)))) args)
      (file-error
       (progn (message "Couldn't load extension: %s" lib) nil)))))

(defun my-reopen-file ()
  "Try to load the lisp code in the current buffer."
  (interactive)
  (let ((my-buffer-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (find-file my-buffer-name)))

(defun my-truncate-toggle ()
  "Toggle the truncate-lines variable state in the current buffer"
  (interactive)
  (cond (truncate-lines
         (set-variable 'truncate-lines nil)
         (set-variable 'truncate-partial-width-windows nil))
        ((not truncate-lines)
         (set-variable 'truncate-lines t)
         (set-variable 'truncate-partial-width-windows t))))

(define-key my-prefix-map "n" 'my-truncate-toggle)

(defun my-hs-hide-level-1 ()
  (interactive)
  (hs-hide-level 1)
  (forward-sexp 1))
(setq hs-hide-all-non-comment-function 'my-hs-hide-level-1)

(defun my-init-flyspell ()
  (interactive)
  (flyspell-mode)
  (if flyspell-mode
      (flyspell-buffer))
  )
(define-key my-prefix-map [(f)] 'my-init-flyspell)


(defun my-current-buffer-dir ()
  "Print the directory in which the file associated with the current buffer
is located.  This is not always the same as default-directory although I
don't know why not."
  (interactive)
  (if (buffer-file-name)
      (message "Buffer's file is located in: %s"
               (file-name-directory buffer-file-name))
    (message "This buffer is not associated with a file.")))

(define-key global-map [(control shift f5)] 'my-current-buffer-dir)

(defun my-sync-pwd-with-current-buffer ()
  "Set the default-directory to be the directory of the file that the
current buffer is visiting."
  (interactive)
  (if (buffer-file-name)
      (cd (file-name-directory buffer-file-name))
    (message "This buffer is not associated with a file.")))
(define-key my-prefix-map [(f5)] 'my-sync-pwd-with-current-buffer)


(defun set-indent()
  "Sets custom indent for c and java style set java-mode
indent to spaces instead of tabs -- tab size 4"
  (interactive)
  (setq indent-tabs-mode nil)   ; use spaces instead of tabs
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inline-open 0))

(defun region-length ()
  "Compute the length of the marked region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

(defun my-occur-all-buffers (regexp &optional hidden-bufs-too)
  "Show all lines in all visible buffers containing a match for REGEXP.
With prefix arg HIDDEN-BUFS-TOO, show lines matching in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur (if hidden-bufs-too
                   (buffer-list)
                 (delq nil
                       (mapcar (lambda (buf)
                                 (unless (string-match "^ " (buffer-name buf))
                                   buf))
                               (buffer-list))))
               regexp))
(define-key my-prefix-map "o" 'my-occur-all-buffers)

(defun set-better-terminal-ediff-faces ()
  (interactive)
  (set-face-foreground 'ediff-odd-diff-face-C "Navy")
  (set-face-foreground 'ediff-odd-diff-face-A "Black")
  (set-face-foreground 'ediff-even-diff-face-Ancestor "Black")
  (set-face-foreground 'ediff-even-diff-face-B "Black")
  (set-face-foreground 'ediff-fine-diff-face-A "White")
  (set-face-foreground 'ediff-current-diff-face-A "Black")
  (set-face-foreground 'ediff-current-diff-face-B "Black")
  (set-face-foreground 'ediff-current-diff-face-C "Black"))


(defun face-colors-rgb ()
  "List face, foreground color, foreground r g b, background color and
   background r g b.

   From: sandipchit...@yahoo.com (Sandip Chitale)

   Newsgroups: gnu.emacs.sources

   Date: 21 Oct 2003 00:23:04 -0700"
  (interactive)
  (set-buffer (get-buffer-create "*faces colors rgb*"))
  (setq truncate-lines t)
  (toggle-read-only -1)
  (let ((f)
        (c)
        (rgb))
    (insert (format "%-35.35s %-15.15s %s %-15.15s %s\n"
                    "Face"
                    "Foreground    "
                    "   R   G   B  "
                    "Background    "
                    "   R   G   B  "))
    (insert (format "%-35.35s %-15.15s %s %-15.15s %s\n"
                    "-----------------------------------"
                    "--------------"
                    "--------------"
                    "--------------"
                    "--------------"))
    (dotimes (idx (length (face-list)))
      (setq f (nth (1- idx) (face-list)))
      (insert (propertize (format "%-35.35s" f ) 'face f))
      (setq fg (face-attribute f :foreground))
      (setq bg (face-attribute f :background))
      (setq rgb (color-values fg))
      (setq r "-")
      (setq g "-")
      (setq b "-")
      (if rgb
          (progn (setq r (/ (nth 0 rgb) 256))
                 (setq g (/ (nth 1 rgb) 256))
                 (setq b (/ (nth 2 rgb) 256)))
        )
      (insert (format " %-15.15s [%03.3s %03.3s %03.3s] " fg r g b))
      (setq rgb (color-values bg))
      (setq r "-")
      (setq g "-")
      (setq b "-")
      (if rgb
          (progn (setq r (/ (nth 0 rgb) 256))
                 (setq g (/ (nth 1 rgb) 256))
                 (setq b (/ (nth 2 rgb) 256)))
        )
      (insert (format " %-15.15s [%03.3s %03.3s %03.3s]" bg r g b))
      (insert "\n")))
  (switch-to-buffer "*faces colors rgb*")
  (delete-trailing-whitespace)
  (toggle-read-only 1)
  (goto-char (point-min)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hooks and customizations for Standard modes and derivatives thereof
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-boilerplate ()
  "Will create a boilerplate file for known modes if the file is empty"
  (interactive)
  (setq boilerplates '( ("cperl-mode" .
                         "#!/usr/bin/env perl\n\nuse warnings;\nuse strict;\n\n")
                        ("sh-mode" . "#!/bin/bash\n\n")
                        ("ruby-mode" . "#!/bin/bash/env ruby\n\n")
                        ("python-mode" . "#!/usr/bin/env python\n\n" )))
  (while (car (car boilerplates))
    (if (string= major-mode (car(car boilerplates)))
        (insert (cdr (car boilerplates))))
    (setq boilerplates (cdr boilerplates))))

(defun my-text-mode-hook ()
  "Personal text mode customizations."
  (auto-fill-mode -1)
  (setq-default indent-tabs-mode t)     ; use tabs instead of spaces
  ;;  (setq indent-line-function 'insert-tab)
  ;;  (flyspell-mode)
  (setq require-final-newline nil)
                                        ;(store-regexp-match-in-register "^[A-Z]+:" nil)
  ;;(refill-mode) ; writing in the middle of a paragraph refills as you type
  (setq tab-width 4))
(add-hook 'text-mode-hook 'my-text-mode-hook)

;; (defun my-outline-mode-hook ()
;;   (interactive)
;;   "Personal outline mode customizations."
;;   ;;text mode hook is automatically run when outline mode starts
;;   (turn-off-auto-fill)
;;   (setq-default indent-tabs-mode t)
;;   (setq indent-line-function 'indent-relative-maybe)
;;   (setq comment-start nil)           ; This makes auto-fill work
;;   (setq auto-fill-inhibit-regexp nil)        ; so does this
;;   (setq comment-line-break-function 'indent-new-comment-line)
;;   (setq paragraph-start "\f\\|[      ]*$")
;;   (setq paragraph-separate "[        \f]*$")
;;   ;; level marker is the tab character
;;   (setq outline-regexp "\\([ ]+\\|\*+\\)")
;;                                      ;  (setq outline-regexp "\\([[:space:]]+\\|\*+\\|__\\)")

;;   (store-regexp-match-in-register "^__" nil)

;;   ;; clear out any old auto-registers we've created
;;   (delete-stale-register-entries-in-buffer)

;;   (if (boundp indent-line-function)
;;       (setq indent-line-function 'my-tab-line)
;;     (define-key outline-mode-map [(control i)] 'my-tab-line)
;;     (define-key outline-mode-map [(tab)] 'my-tab-line))

;;   (define-key outline-mode-map [(control m)] 'indent-new-comment-line)
;;   (define-key outline-mode-map [(control meta n)] 'outline-forward-same-level)
;;   (define-key outline-mode-map [(control meta p)] 'outline-backward-same-level)


;;   (define-key outline-mode-map [(backtab)]
;;     (lambda () (interactive) (my-tab-line t))) ;only works with X11
;;   )
;; (add-hook 'outline-mode-hook 'my-outline-mode-hook)
;; (add-hook 'outline-mode-hook
;;        (lambda ()
;;          ;; convert spaces to tabs on save.
;;          ;; The t makes it only happen in outline mode
;;          (add-hook 'after-save-hook (lambda () (tabify 0 (buffer-size))) nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External process commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))
(global-set-key (kbd "C-c o") 'open-with)

(defun my-open-terminal-here ()
  "Opens the terminal defined by the variable my-terminal-program at
  the current working directory."
  (interactive)
  (if (not (boundp 'my-terminal-program)) ;use default if not set
      (setq my-terminal-program "xterm"))

  ;; only need this if we need to call with args
  ;;(eval (append (list 'call-process my-terminal-program nil 0 nil) my-terminal-program-args))
  (call-process my-terminal-program nil t nil)
  (message "Terminal opened."))
(define-key my-prefix-map "1" 'my-open-terminal-here)


(defun my-open-filemanager-here ()
  "Opens the graphical file manager defined by the variable
  my-filemanager-progam at the current working directory."
  (interactive)
  (if (not (boundp 'my-filemanager-progam)) ;use default if not set
      (setq my-filemanager-progam "nautilus"))
  (call-process my-filemanager-progam nil 0 nil "."))
(define-key my-prefix-map "3" 'my-open-filemanager-here)

(defun my-delete-file-of-buffer ()
  "Delete the file that the current buffer is visiting and kill the buffer"
  (interactive)
  (cond ((y-or-n-p (format "Delete file: %s? " (buffer-file-name)))
         (delete-file (buffer-file-name))
         (kill-buffer (current-buffer)))))
(define-key my-prefix-map [(control k)] 'my-delete-file-of-buffer)

                                                             
                                                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-print-buffer-code (&optional filename)
  "Print buffer with settings for printing code."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (setq ps-landscape-mode nil           ; landscape mode
        ps-number-of-columns 1
        ps-n-up-printing 4              ;n sheets per page
        ps-n-up-border-p nil ; draw a border around each sheet on a page
        ps-line-number t                ; print line numbers
        ps-line-number-step 5           ; number every nth line
        ps-line-number-color 0 ;print numbers in gretscale n, 0 is balck
        ps-spool-duplex t               ;print on both sides
        ps-left-header (quote (ps-get-buffer-name))
        ps-right-header (quote ("/pagenumberstring load"
                                ps-time-stamp-mon-dd-yyyy))
        ps-print-header t               ; print headers
        )
  (ps-print-buffer filename)
  (if filename
      (message (concat "Buffer written to " filename))
    (message "Buffer printed with code settings")))

(defun my-print-buffer-plain (&optional filename)
  "Print buffer with settings for printing plain pages."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (setq ;;page layout
   ps-number-of-columns 1
   ps-n-up-printing 1                   ;n sheets per page
   ps-spool-duplex nil                  ; don't pad with extra pages
   ps-n-up-border-p nil     ;draw a border around each sheet on a page
   ps-landscape-mode nil                ; landscape mode

   ;; print line numbers
   ps-line-number nil
   ps-line-number-step 5                ; number every nth line
   ps-line-number-color 0    ;print numbers in gretscale n, 0 is balck

   ;; print headers
   ps-print-header nil
   ps-left-header (quote (ps-get-buffer-name))
   ps-right-header (quote ("/pagenumberstring load"
                           ps-time-stamp-mon-dd-yyyy))
   )
  (ps-print-buffer filename)
  (if filename
      (message (concat "Buffer written to " filename))
    (message "Buffer printed with plain settings")))

(defun my-print-buffer-plain-header (&optional filename)
  "Print buffer with settings for printing plain pages with header."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (setq ps-spool-duplex t               ;print on both sides
        ;;page layout
        ps-number-of-columns 1
        ps-n-up-printing 1              ;n sheets per page
        ps-spool-duplex nil             ; don't pad with extra pages
        ps-n-up-border-p nil ;draw a border around each sheet on a page
        ps-landscape-mode nil           ; landscape mode

        ;; print line numbers
        ps-line-number nil
        ps-line-number-step 5           ; number every nth line
        ps-line-number-color 0 ;print numbers in gretscale n, 0 is balck

        ;; print headers
        ps-print-header t
        ps-left-header (quote (ps-get-buffer-name))
        ps-right-header (quote ("/pagenumberstring load"
                                ps-time-stamp-mon-dd-yyyy))
        )
  (ps-print-buffer filename)
  (if filename
      (message (concat "Buffer written to " filename))
    (message "Buffer printed with plain header settings")))

(define-key my-prefix-map [(p) (p)] 'my-print-buffer-plain)
(define-key my-prefix-map [(p) (c)] 'my-print-buffer-code)
(define-key my-prefix-map [(p) (h)] 'my-print-buffer-plain-header)
(define-key my-prefix-map [(p) (z)]
  (lambda () (interactive)
    (my-print-buffer-plain-header"~/z.ps")))

;; this doesn't appear to work on the mac. Maybe on linux? Found it here
;; http://www.emacswiki.org/emacs/EdiffMode#toc1
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))
(add-to-list 'command-switch-alist '("diff" . command-line-diff))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code folding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-hs-mode-hook ()
  (define-key global-map [(control c) (control s)] 'hs-toggle-hiding)
  )
(add-hook 'hs-minor-mode-hook 'my-hs-mode-hook)

(defun my-hs-toggle-hiding ()
  "Check to see if we need to activate hs-minor-mode first."
  (interactive)
  (if (not (fboundp 'hs-toggle-hiding))
      (hs-minor-mode))
  (hs-toggle-hiding))
(define-key my-prefix-map [(h)] 'my-hs-toggle-hiding)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; By default text-scale-adjust only changes the font size of the current buffer.
;; These functions change the default font size so all of emacs scales up and down
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun text-zoom-in ()
  "Zoom in all text"
  (interactive)
  (cond ((not(boundp 'my-face-default-height))
         (setq my-face-default-height (face-attribute 'default :height))
         ))
  (let ((new-height (+ (face-attribute 'default :height) 10)))
        (set-face-attribute 'default nil :height new-height)
        (message (concat "New font size set to " (number-to-string new-height)))
      )
  )
(defun text-zoom-out ()
  "Zoom out all text"
  (interactive)
  (cond ((not(boundp 'my-face-default-height))
         (setq my-face-default-height (face-attribute 'default :height))
         ))
  (let ((new-height (- (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new-height)
    (message (concat "New font size set to " (number-to-string new-height)))
    )
  )

(defun text-zoom-default ()
  "Set text to default size"
  (interactive)
  (cond ((not(boundp 'my-face-default-height))
         (setq my-face-default-height (face-attribute 'default :height))
         ))
  (let ((new-height my-face-default-height))
    (set-face-attribute 'default nil :height new-height)
    (message (concat "New font size set to " (number-to-string new-height)))
    )
  )
;; these only do the buffer you're in
;; (define-key global-map [(control =)] 'text-scale-increase)
;; (define-key global-map [(control -)] 'text-scale-decrease)
(define-key global-map [(control =)] 'text-zoom-in)
(define-key global-map [(control -)] 'text-zoom-out)
(define-key global-map [(control \))] 'text-zoom-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Random functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun my-semantic-dont-parse ()
  "Function called by semantic with no args.  If it returns nil semantic
won't parse the buffer."
  (if (string= (file-name-extension (buffer-name)) "rul")
      t
    nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End of custom function definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; emacs customizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;auto generated customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(CUA-mode-inhibit-delay 30000)
 '(CUA-mode-remap-cx-shift-only t)
 '(abbrev-mode t t)
 '(ac-auto-show-menu t)
 '(ac-auto-start 2)
 '(ac-delay 0.1)
 '(ac-etags-requires 1)
 '(ac-use-fuzzy t)
 '(ack-and-a-half-arguments
   (quote
    ("--ignore-dir=vendor" "--ignore-dir=log" "--column" "--context")))
 '(ack-and-a-half-executable "/usr/local/bin/ag")
 '(ack-and-a-half-prompt-for-directory t t)
 '(ag-arguments
   (quote
    ("--context" "--ignore-dir=log" "--ignore-dir=vendor" "--all-text" "--smart-case" "--nogroup" "--column" "--")))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(auto-indent-blank-lines-on-move nil)
 '(auto-indent-disabled-modes-list
   (quote
    (compilation-mode conf-windows-mode diff-mode inferior-ess-mode dired-mode eshell-mode fundamental-mode log-edit-mode makefile-gmake-mode org-mode snippet-mode texinfo-mode text-mode wl-summary-mode coffee-mode yaml-mode nil)))
 '(auto-indent-global-mode t nil (auto-indent-mode))
 '(auto-indent-untabify-on-visit-file t)
 '(auto-save-timeout 30)
 '(auto-save-visited-file-name nil)
 '(bookmark-automatically-show-annotations nil)
 '(bookmark-default-file "~/.emacs.local/emacs.bmk")
 '(bookmark-save-flag 0)
 '(bookmark-use-annotations nil)
 '(bs-max-window-height 30)
 '(canlock-password "e0f10ec3fe2e8bcd3d1b789cfcad28b660ba3672")
 '(clearcase-checkout-arguments (quote ("-unr")))
 '(clearcase-checkout-switches "-unr")
 '(clearcase-diff-on-checkin t)
 '(clearcase-use-normal-diff t)
 '(coffee-tab-width 2)
 '(color-theme-legal-frame-parameters "\\(color\\|mode\\|font\\|height\\|width\\)$")
 '(column-highlight-mode nil)
 '(comment-auto-fill-only-comments t)
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".copyarea.db")))
 '(cperl-break-one-line-blocks-when-indent nil)
 '(cperl-continued-statement-offset 0)
 '(cperl-electric-keywords nil)
 '(cperl-electric-linefeed t)
 '(cperl-extra-newline-before-brace nil)
 '(cperl-extra-newline-before-brace-multiline nil)
 '(cperl-fix-hanging-brace-when-indent nil)
 '(cperl-font-lock t)
 '(cperl-hairy nil)
 '(cperl-highlight-variables-indiscriminately t)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-indent-region-fix-constructs nil)
 '(cperl-label-offset 0)
 '(cperl-merge-trailing-else t)
 '(cperl-under-as-char t)
 '(css-indent-offset 2)
 '(cua-auto-mark-last-change t)
 '(cua-enable-cua-keys nil)
 '(cua-enable-cursor-indications t)
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "green")
 '(cua-prefix-override-inhibit-delay 1)
 '(cua-read-only-cursor-color "red")
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(desktop-path (quote ("~/.emacs.local/")))
 '(desktop-save t)
 '(desktop-save-mode t)
 '(diary-file (expand-file-name (concat my-emacs-init-dir "/diary")))
 '(directory-abbrev-alist nil)
 '(dired-find-subdir t)
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*")
 '(dired-recursive-deletes (quote top))
 '(dired-view-command-alist
   (quote
    (("[.]ps\\'" . "gv -spartan -color")
     ("[.]pdf\\'" . "xpdf")
     ("[.]dvi\\'" . "xdvi -sidemargin 0.5 -topmargin 1")
     ("[.]doc\\'" . "winword")
     ("[.]ppt\\'" . "powerpnt"))))
 '(dired-x-hands-off-my-keys nil)
 '(ecb-create-layout-file "~/.emacs.d/init/ecb-user-layouts.el")
 '(ecb-directories-menu-user-extension-function (quote ignore))
 '(ecb-download-url "http://prdownloads.sourceforge.net/ecb/")
 '(ecb-excluded-directories-regexp "^\\(CVS\\|.\\|..\\)$")
 '(ecb-excluded-directories-regexps (quote ("^\\(\\.git\\|\\.sass-cache\\|\\.\\|\\.\\.\\)$")))
 '(ecb-fix-window-size (quote width))
 '(ecb-history-menu-user-extension-function (quote ignore))
 '(ecb-layout-name "left15")
 '(ecb-layout-window-sizes
   (quote
    (("my-ecb-layout-leftright"
      (ecb-directories-buffer-name 0.23267326732673269 . 0.9821428571428571)
      (ecb-methods-buffer-name 0.23267326732673269 . 0.625)
      (ecb-history-buffer-name 0.23267326732673269 . 0.35714285714285715))
     ("left15"
      (ecb-directories-buffer-name 0.3016759776536313 . 0.5760869565217391)
      (ecb-methods-buffer-name 0.3016759776536313 . 0.41304347826086957))
     ("leftright1"
      (ecb-directories-buffer-name 0.212707182320442 . 0.5869565217391305)
      (ecb-sources-buffer-name 0.212707182320442 . 0.043478260869565216)
      (ecb-history-buffer-name 0.212707182320442 . 0.358695652173913)
      (ecb-methods-buffer-name 0.20165745856353592 . 0.9891304347826086))
     ("leftright2"
      (ecb-directories-buffer-name 0.143646408839779 . 0.6521739130434783)
      (ecb-sources-buffer-name 0.143646408839779 . 0.33695652173913043)
      (ecb-methods-buffer-name 0.16298342541436464 . 0.6521739130434783)
      (ecb-history-buffer-name 0.16298342541436464 . 0.33695652173913043))
     ("left3"
      (ecb-directories-buffer-name 0.3016759776536313 . 0.29347826086956524)
      (ecb-sources-buffer-name 0.3016759776536313 . 0.34782608695652173)
      (ecb-methods-buffer-name 0.3016759776536313 . 0.34782608695652173)))))
 '(ecb-major-modes-show-or-hide (quote (nil)))
 '(ecb-methods-menu-user-extension-function (quote ignore))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-show-sources-in-directories-buffer (quote always))
 '(ecb-sources-menu-user-extension-function (quote ignore))
 '(ecb-tip-of-the-day nil)
 '(ecb-tip-of-the-day-file "~/.emacs.local/ecb-tip-of-day.el")
 '(ecb-toggle-layout-sequence (quote ("left15" "left7" "leftright1")))
 '(ecb-tree-incremental-search (quote substring))
 '(ecb-tree-indent 2)
 '(ecb-tree-truncate-lines
   (quote
    (ecb-directories-buffer-name ecb-methods-buffer-name)))
 '(ecb-use-speedbar-instead-native-tree-buffer (quote nil))
 '(ecb-wget-setup (quote cons))
 '(echo-keystrokes 0.1)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eshell-directory-name "~/.emacs.local/eshell/")
 '(eshell-prefer-to-shell t nil (eshell))
 '(etags-table-search-up-depth 100)
 '(exec-path-from-shell-variables
   (quote
    ("PATH" "MANPATH" "LANG" "LC_CTYPE" "MY_ENG" "MY_ENV" "MY_EMAIL_PERSONAL" "MY_NAME")))
 '(fci-rule-color "#073642")
 '(file-coding-system-alist
   (quote
    (("\\.elc\\'" emacs-mule . emacs-mule)
     ("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix)
     ("\\.reg\\'" utf-16-le . utf-16-le)
     ("\\.tar\\'" no-conversion . no-conversion)
     ("" undecided))))
 '(fill-column 100)
 '(flx-ido-mode t)
 '(flymake-no-changes-timeout 10)
 '(flymake-start-syntax-check-on-newline nil)
 '(glasses-face (quote bold))
 '(glasses-original-separator "")
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(global-auto-complete-mode t)
 '(global-auto-revert-mode t)
 '(global-hungry-delete-mode t)
 '(global-smart-tab-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(global-visual-line-mode t)
 '(grep-command "grep -2EInir ")
 '(grep-highlight-matches (quote auto-detect))
 '(grep-use-null-device (quote auto-detect))
 '(help-window-select t)
 '(hippie-expand-try-functions-list
   (quote
    (yas-hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(hs-isearch-open t)
 '(icicle-mode t)
 '(icicle-show-Completions-initially-flag t)
 '(ido-create-new-buffer (quote prompt))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-max-file-prompt-width 0)
 '(ido-max-prospects 0)
 '(ido-max-window-height 20)
 '(ido-vertical-mode t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(jde-build-function (quote (jde-ant-build)))
 '(jde-check-version-flag nil)
 '(jde-compile-option-classpath (quote (".")))
 '(jde-compile-option-debug (quote ("all" (t nil nil))))
 '(jde-compile-option-sourcepath (quote (".")))
 '(jde-complete-function (quote jde-complete-minibuf))
 '(jde-complete-signature-display (quote ("Eldoc")))
 '(jde-debugger (quote ("JDEbug")))
 '(jde-enable-abbrev-mode t)
 '(jde-gen-conditional-padding-2 "")
 '(jde-gen-k&r t)
 '(jde-gen-method-signature-padding-2 "")
 '(jde-gen-method-signature-padding-3 " ")
 '(jde-global-classpath (quote (".")))
 '(jde-jdk-registry (quote (("1.4.2" . "$JAVA_HOME"))))
 '(jde-sourcepath (quote (".")))
 '(jira-url "http://jira.tapjoy.net/rpc/xmlrpc")
 '(large-file-warning-threshold 100000000)
 '(ldap-host-parameters-alist
   (quote
    (("ldap.cisco.com" base "o=cisco.com" timelimit 10 sizelimit 100))))
 '(mail-default-directory "~/.emacs.local/")
 '(mail-source-directory "~/.emacs.local/Mail/")
 '(major-mode (quote org-mode))
 '(message-auto-save-directory "~/.emacs.local/Mail/drafts/")
 '(message-directory "~/.emacs.local/Mail/")
 '(mode-line-format
   (quote
    ("%e " mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     mode-line-misc-info mode-line-modes mode-line-end-spaces)))
 '(mouse-scroll-min-lines 2)
 '(mouse-wheel-progressive-speed t)
 '(nnmail-message-id-cache-file "~/.emacs.local/.nnmail-cache")
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(nxml-slash-auto-complete-flag t)
 '(org-default-notes-file "~/Dropbox/RIGHT_NOW.txt")
 '(org-reverse-note-order t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(projectile-cache-file "~/.emacs.local/projectile.cache")
 '(projectile-enable-caching nil)
 '(projectile-find-file-hook (quote (projectile-cache-projects-find-file-hook)))
 '(projectile-globally-ignored-files (quote ("*.elc" "TAGS")))
 '(projectile-project-root-files
   (quote
    ("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "Rakefile")))
 '(projectile-remember-window-configs t)
 '(projectile-sort-order (quote recently-active))
 '(projectile-switch-project-action (quote projectile-commander))
 '(ps-line-number t)
 '(ps-line-number-color 50)
 '(python-indent-offset 4)
 '(rmail-secondary-file-directory "~/.emacs.local/")
 '(rng-nxml-auto-validate-flag nil)
 '(rspec-command-options "--format documentation --drb --backtrace")
 '(rspec-use-rake-when-possible nil)
 '(rspec-use-rvm t)
 '(rvm-configuration-file-name ".ruby-version")
 '(save-abbrevs (quote silently))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.local/emacs-places")
 '(semantic-inhibit-functions (quote (my-semantic-dont-parse)))
 '(semanticdb-default-save-directory "~/.emacs.local/semantic.cache")
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(size-indication-mode t)
 '(smart-tab-disabled-major-modes (quote (org-mode term-mode eshell-mode Custom-mode)))
 '(smart-tab-using-hippie-expand t)
 '(speedbar-default-position (quote left-right))
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 30)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t))))
 '(speedbar-show-unknown-files t)
 '(speedbar-supported-extension-expressions
   (quote
    (".org" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" ".rb")))
 '(speedbar-use-images t)
 '(stack-trace-on-error nil t)
 '(tags-revert-without-query t)
 '(tail-hide-delay 0)
 '(tail-max-size 0)
 '(tail-volatile nil)
 '(todo-file-do "~/.emacs.local/todo-do")
 '(todo-file-done "~/.emacs.local/todo-done")
 '(todo-file-top "~/.emacs.local/todo-top")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-min-dir-content 1)
 '(uniquify-trailing-separator-p t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(vc-cvs-diff-switches "-up")
 '(vc-diff-switches "-pu")
 '(which-function-mode t)
 '(whitespace-check-indent-whitespace nil)
 '(whitespace-global-mode nil nil (whitespace))
 '(whitespace-line-column 150)
 '(whitespace-space-regexp "\\(^ +\\| +$\\)")
 '(whitespace-style
   (quote
    (face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab tab-mark)))
 '(window-numbering-auto-assign-0-to-minibuffer t)
 '(writeroom-disable-fringe t)
 '(writeroom-fullscreen-effect (quote fullboth))
 '(writeroom-restore-window-config t)
 '(writeroom-width 150))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(col-highlight ((t (:background "gray15"))))
 '(nxml-attribute-colon-face ((t (:inherit nxml-name-face :foreground "green"))))
 '(nxml-attribute-local-name-face ((t (:inherit nxml-name-face :foreground "limegreen"))))
 '(nxml-attribute-prefix-face ((t (:inherit nxml-name-face :foreground "green"))))
 '(nxml-attribute-value-delimiter-face ((t (:inherit nxml-delimiter-face :foreground "burlywood"))))
 '(nxml-cdata-section-CDATA-face ((t (:inherit nxml-name-face :foreground "limegreen"))))
 '(nxml-comment-content-face ((t (:foreground "cadetblue" :slant italic))))
 '(nxml-comment-delimiter-face ((t (:inherit nxml-delimiter-face :foreground "cadetblue"))))
 '(nxml-delimited-data-face ((nil (:foreground "burlywood"))))
 '(nxml-delimiter-face ((t (:foreground "grey" :weight bold))))
 '(nxml-element-prefix-face ((t (:inherit nxml-name-face :foreground "limegreen"))))
 '(nxml-name-face ((nil (:foreground "goldenrod" :weight bold))))
 '(nxml-namespace-attribute-colon-face ((t (:inherit nxml-name-face :foreground "green"))))
 '(nxml-namespace-attribute-prefix-face ((t (:inherit nxml-name-face :foreground "limegreen"))))
 '(nxml-namespace-attribute-value-delimiter-face ((t (:inherit nxml-attribute-value-delimiter-face :foreground "gold"))))
 '(nxml-namespace-attribute-value-face ((t (:inherit nxml-attribute-value-face :foreground "gold"))))
 '(nxml-namespace-attribute-xmlns-face ((t (:inherit nxml-name-face :foreground "green"))))
 '(nxml-tag-slash-face ((t (:inherit nxml-name-face))))
 '(semantic-decoration-on-private-members-face ((((class color) (background light)) nil)))
 '(underline ((t (:underline nil))))
 '(which-func ((t (:foreground "gray92")))))

;; Load third party lisp tools if available
;; If packages are in elpa they aren't available until after init.el loads so put this
;; in the after-init-hook
;; Put this at the very end of .emacs so that it is put at the beginning of the after-init-hook.
;; We want this to run before the desktop restores previously opened buffers so
;; modes and extras are available to be loaded with the buffers that use them
(defun my-after-init-hook()
  (message "running after init hook")
  ;; If the EMACS_MINIMAL environment variable is set, then we want
  ;; the trimmed down emacs for fast open for a quick edit.  Don't
  ;; load all the crap in these files.
  (cond ((not (getenv "EMACS_MINIMAL"))
         ;; settings from the environment. Because OSX doesn't load the shell environment for applications
         ;; we have to set these here in the after-hook in order to give
         ;; exec-path-from-shell-variables a chance to be loaded and run.
         (setq user-full-name (getenv "MY_NAME")
               user-mail-address (getenv "MY_EMAIL_PERSONAL"))

         (setq my-eng (getenv "MY_ENG")
               my-src (concat my-eng "/src")
               my-env  (getenv "MY_ENV"))
         )
        ((getenv "EMACS_MINIMAL")
         ;; (message "EMACS_MINIMAL set in environment.  Loading minimal tools")
         ;; pager isn't loaded but I hate having C-z background the process. Use C-x z instead.
         (global-unset-key [(control z)])
         (setq
          desktop-save nil  ; don't load desktop in minimal mode
          desktop-save-mode nil
          confirm-kill-emacs 'nil) ; don't ask to exit
         )))
(add-hook 'after-init-hook 'my-after-init-hook)
