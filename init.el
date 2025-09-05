;;; package -- Summary my init file  -*- lexical-binding: t; -*-
;;; 
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Personal variables paths and stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; also https://github.com/neppramod/java_emacs/blob/master/init.el
;; Avoid garbage collection at statup
;;; Code:
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(let ((elapsed (float-time (time-subtract (current-time) my-emacs-start-time))))
  (message "init.el loading started: %.1f seconds since startup" elapsed))

;; reset GC after everything is loaded
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 1000000000 ; 1GB
		  gc-cons-percentage 0.1)
	    ;; Display startup time
	    ;; my-emacs-start-time set in early-init.el
            (let ((elapsed (float-time (time-subtract (current-time) my-emacs-start-time))))
              (message "Emacs startup completed in %.1f seconds" elapsed))))

;; set root of personal emacs repository and common directories used
;; in functions and other variables below
(defconst user-emacs-directory (expand-file-name ".emacs.d" "~"))
(defconst my-emacs-init-dir (expand-file-name "init" user-emacs-directory))
(defconst my-site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))

(defconst my-emacs-local-store (expand-file-name ".emacs.local" "~") "Place where local stuff is kept e.g. desktop definitions.")
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory) "Junk place for auto-save files.")

;; Tell customize to use a separate file
(setq custom-file (expand-file-name "init/custom.el" user-emacs-directory))
;; Load the customizations now. The makes it so we have the list of installed packages available before we load packages
(load custom-file)
(let ((elapsed (float-time (time-subtract (current-time) my-emacs-start-time))))
  (message "Custom.el loaded: %.1f seconds since startup" elapsed))

;; External programs
;; My own terminal script wrapper for xplatform terminal fun
(defconst my-terminal-program "my-terminal" "What command to execute to get a terminal.")
(defconst my-terminal-program-args nil)

;; My own terminal script wrapper for xplatform file browsing fun
(defconst my-filemanager-progam "jjr-file-browser" "My own terminal script wrapper for xplatform file browsing fun.")

;; PACKAGES!!

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Tell Emacs to always prefer the newest installed package version
;; instead of the one that ships with emacs.
(setq package-load-list '((all t)))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)



;; Just in case these directories aren't there yet create them.
;; If localstore doesn't exsist emacs won't exit correctly, which is really annoying
(mapc (lambda (dir) (make-directory dir t))
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
;; Are we on UNIX?
(defvar my-unixp (or (eq system-type 'gnu/linux)
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
(electric-indent-mode 1) ; global minor mode for auto indent on newline

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
 select-enable-clipboard t ; use native clipboard for yanking
 ;; font-use-system-font t ; deprecated in 23
 ring-bell-function 'ignore     ; Blessed silence
 query-replace-highlight t
 completion-ignore-case t               ;ignore case on completions
 read-file-name-completion-ignore-case t ;ignore case on find-file
 inhibit-startup-message t        ; don't show splash screen
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil  ; don't create backup~ files
 ;; death to all temp files https://www.emacswiki.org/emacs/AutoSave
 backup-directory-alist `((".*" . ,emacs-tmp-dir))
 auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
 auto-save-list-file-prefix emacs-tmp-dir
 ;; old variable. Still needed 2019-01-04 
 ;; auto-save-directory (concat my-emacs-local-store "/auto-save-list")
 ;; auto-save-list-file-prefix ".saves-"

 max-lisp-eval-depth 999               ; fixes ecb in deep directories
 line-number-display-limit-width 500 ; still show line numbers with long lines
 require-final-newline nil ; adds new lines to the end of buffers
 next-line-add-newlines nil ; adds new lines to the end of buffers
 mouse-yank-at-point t          ; yank at point, not at click, but who uses the mouse anyway?
 parens-require-spaces nil ; no whitespace padding around parens
                                        ; confirm-kill-emacs 'y-or-n-p ; prompt to exit
 )
(fset 'yes-or-no-p 'y-or-n-p) ; Make all "yes or no" prompts "y or n" instead
;; this fixes the meta key with ubuntu on the macbook
;;(setq x-super-keysym 'meta)

;; keep a list of recently opened files.
(recentf-mode t)
(define-key global-map [(control x) (control meta f)] 'recentf-open-files)

;; turn on default disabled functionality
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom keybindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Additional keybindings under hydra prefix set in hydra-mine function
;; replaced by hydra
;; (defvar my-prefix-map (make-sparse-keymap))
;; (define-key global-map [(control x) (d)] my-prefix-map)


(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-S-k")
                (lambda ()
		  (interactive)
		  (kill-this-buffer nil)))
(global-set-key (kbd "C-c q") 'auto-fill-mode) ; toggle auto-fill-mode
(global-set-key (kbd "M-`") 'other-frame) ; mac-like frame switching behavior
(global-set-key (kbd "M-1") 'other-window) ; alt-tab for buffer windows. 
(global-set-key (kbd "M-0")   #'(lambda () (interactive)
				  (select-window (active-minibuffer-window))))
;; vscode/sublime keybinding and behavior for join lines
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line t)))
(global-set-key [3 9] (lambda () (interactive) (insert-tab))) ; Control-C TAB as a vector

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
(add-to-list 'auto-mode-alist '("\\.log" . text-mode)) ;; 

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
       (mapc (lambda (load-path-dir)
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
;;; Navigation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; If we're on a Mac and the file "~/bin/get_dark.osascript" exists
;; and it outputs "false", activate light mode. Otherwise activate
;; dark mode.
(defun set-dark-light-on-mac ()
  "Set Emacs theme to match the theme on OSX."
  (interactive)
  (cond ((and (file-exists-p "~/.bin/is_dark_mode.osascript")
	      (string> (shell-command-to-string "command -v osascript") "")
	      (equal "false\n"
		     (shell-command-to-string "osascript ~/.bin/is_dark_mode.osascript")))
         (enable-theme 'doom-one-light))
        (t (enable-theme 'doom-spacegrey))))


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
  "Home - begin of text, once more - begin of line, once more - screen,once more - buffer."
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
(global-set-key [?\C-a] 'my-home)
(define-key global-map [?\C-e] 'my-end)

(defun my-last-edit ()
  "Go back to last add/delete edit."
  (interactive)
  (let* ((ubuf (cadr buffer-undo-list))
         (beg (car ubuf))
         (end (cdr ubuf)))
    (cond
     ((integerp beg) (goto-char beg)))))
(global-set-key [(control shift backspace)] 'my-last-edit)

(defun match-paren (arg)
  "Go to the matching parenthesis if ARG is a parenthesis.
Otherwise insert whatever key to which this function is bound.  Usually '%'."
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
  "Make open line indent the line you were on before."
  (interactive )
  (beginning-of-line-text)
  (setq cc (current-column))
  (save-excursion
    (beginning-of-line)
    (open-line 1))
  ;; condition is for correctly handling blank lines
  (if (not (eq cc 0)) (forward-line -1))  
  (dotimes (number cc)
    (insert " "))
  )

(define-key global-map [(control o)] 'my-open-line)

;; insert date in various formats
(defun insert-full-date-and-time ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y %k:%M:%S")))

(defun insert-long-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))

(defun insert-short-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-time ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%k:%M:%S")))


(defun insert-filename (&optional arg)
  "Insert ARG the current filename at point."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (cond (filename
           (if arg
	       (insert filename)
             (insert (file-name-nondirectory filename)))))))

(defun toggle-comment-on-line-or-region (&optional arg)
  "Comment or uncomment current line or selected region ARG, and go to the next line.
https://github.com/kaushalmodi/.emacs.d/blob/13bc1313e786ce1f1ab41d5aaff3dc39dfc57852/setup-files/setup-editing.el#L110-117
Found in comment of http://endlessparentheses.com/implementing-comment-line.html?source=rss"
  (interactive)
  (if (region-active-p)
      (comment-dwim arg)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (forward-line))
(global-set-key (kbd "M-;") #'toggle-comment-on-line-or-region)

(defun my-comment-block ()
  "Write a big comment block to the screen.
Use newcomment package"
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
         (forward-line -1)))
  (forward-line -1)
  (end-of-line)
  (insert " "))

(defun my-tab-line (&optional arg)
  "Insert a tab at the beginning of the current line.
Optional argument ARG removes a tab from the beginning of the line."
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


(defun fill-line ()
  "Fills the current line."
  (interactive)
  (point-to-register 'iwdw-register)
  (beginning-of-line)
  (let ((beg (point))) (forward-line 1) (fill-region beg (point)))
  (register-to-point 'iwdw-register))

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

(defun my-tabify-buffer ()
  "Run Untabify against the current buffer."
  (interactive)
  (tabify (point-min) (point-max))
  (message "Tabify Complete"))

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

(defun clean-buffers (arg)
  "Clean temporary and old buffers as defined by the regexs in 'clean-buffer-list.
If called with an ARG, then cleans up buffers specified by the regexs in clean-buffers-extended-list."
  (interactive "P")
  (mapcar (lambda (buffer)
            (let ((buffer (buffer-name buffer)))
	      (mapcar (lambda (re)
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

;; Reopen the last killed buffer
;; Source: https://stackoverflow.com/questions/10394213/emacs-reopen-previous-killed-buffer
(defun undo-kill-buffer ()
  (interactive)
  (let ((active-files (cl-loop for buf in (buffer-list)
                            when (buffer-file-name buf) collect it)))
    (cl-loop for file in recentf-list
          unless (member 'file active-files) return (find-file 'file))))

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



(defun my-init-flyspell ()
  (interactive)
  (flyspell-mode)
  (if flyspell-mode
      (flyspell-buffer))
  )


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
  (read-only-mode -1)
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
  (read-only-mode 1)
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

(defun my-open-filemanager-here ()
  "Opens the graphical file manager defined by the variable
  my-filemanager-progam at the current working directory."
  (interactive)
  (if (not (boundp 'my-filemanager-progam)) ;use default if not set
      (setq my-filemanager-progam "nautilus"))
  (call-process my-filemanager-progam nil 0 nil "."))

(defun my-delete-file-of-buffer ()
  "Delete the file that the current buffer is visiting and kill the buffer"
  (interactive)
  (cond ((y-or-n-p (format "Delete file: %s? " (buffer-file-name)))
         (delete-file (buffer-file-name))
         (kill-buffer (current-buffer)))))

;; this doesn't appear to work on the mac. Maybe on linux? Found it here
;; http://www.emacswiki.org/emacs/EdiffMode#toc1
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))
(add-to-list 'command-switch-alist '("diff" . command-line-diff))


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

(defun paste-stack-in-project ()
  "Paste the clipboard into a new compilation buffer with the default directory set to to the root of the
current projectile project.
This allows you to call this when you just copied a stack trace so you can use the compilation buffer's
cool file navigation and next/prev-error stuff on the stack trace you pasted."
  (interactive)
  (when (get-buffer "*stack-trace*")
    (kill-buffer "*stack-trace*")
    )

  (let* ((default-directory (projectile-project-root))
         (output-buffer (get-buffer-create "*stack-trace*")))
    (with-current-buffer output-buffer
      (insert (concat "Stack trace navigation in directory " (abbreviate-file-name default-directory) "\n\n\n"))
      (yank)
      (compilation-mode)
      )
    (pop-to-buffer output-buffer)
    )
  )


(defun my-semantic-dont-parse ()
  "Function called by semantic with no args.  If it returns nil semantic
won't parse the buffer."
  (if (string= (file-name-extension (buffer-name)) "rul")
      t
    nil)
  )


(defun my-configure-exec-path-and-cache ()
  "Initialize variable `exec-path' from shell and cache the results for future startups."
  (let ((cache-file (expand-file-name "etc/exec-path-cache.el" user-emacs-directory)))
    (if (file-exists-p cache-file)
        ;; If cache exists, load it. Super fast.
        (progn
          (load-file cache-file)
          (message "exec-path-from-shell: Loaded variables from cache."))
      ;; If not, do the slow initialization once.
      (progn
        (exec-path-from-shell-initialize)
        ;; Now, create the cache file for next time.
        (unless (file-directory-p (file-name-directory cache-file))
          (make-directory (file-name-directory cache-file) t))
        (with-temp-buffer
          (insert ";;; This file is automatically generated by my-configure-exec-path-and-cache\n\n")
          ;; Save the essential variables that were set.
          (dolist (var exec-path-from-shell-variables)
            (when-let ((value (getenv var)))
              (insert (format "(setenv %S %S)\n" var value))))
          (insert (format "\n(setq exec-path (quote %S))" exec-path))
          (write-file cache-file))
        (message (concat "exec-path-from-shell: Created cache file for future startups - " cache-file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End of custom function definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Load third party lisp tools if available
;; If packages are in elpa they aren't available until after init.el loads so put this
;; in the after-init-hook
;; Put this at the very end of .emacs so that it is put at the beginning of the after-init-hook.
;; We want this to run before the desktop restores previously opened buffers so
;; modes and extras are available to be loaded with the buffers that use them
(defun my-after-init-hook()
  "To be run after setup to ensure custom variables are set."
  (let ((elapsed (float-time (time-subtract (current-time) my-emacs-start-time))))
    (message "Running after init hook: %.1f seconds since startup" elapsed))
  
  ;; If the EMACS_MINIMAL environment variable is set, then we want
  ;; the trimmed down emacs for fast open for a quick edit.  Don't
  ;; load all the crap in these files.
  (cond ((not (getenv "EMACS_MINIMAL"))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;;
	 ;; OS/Platform specific settings
	 ;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 (cond ((eq system-type 'darwin)
                ;; settings from the environment. Because OSX doesn't load the shell environment for applications
		;; we have to set these here in the after-hook in order to give
		;; exec-path-from-shell-variables a chance to be loaded and run.
                (use-package exec-path-from-shell
		  :ensure t
		  :config
		  (when (memq window-system '(mac ns))
		    (my-configure-exec-path-and-cache)))

		(let ((elapsed (float-time (time-subtract (current-time) my-emacs-start-time))))
		  (message "Loaded exec-path variables: %.1f seconds since startup" elapsed))

		(setq user-full-name (if (getenv "MY_NAME") (getenv "MY_NAME") user-full-name)
		      user-mail-address (if (getenv "MY_EMAIL_PERSONAL") (getenv "MY_EMAIL_PERSONAL") user-mail-address))

       	        ;; lets emacs find stuff on the path.  This is only necessary on OSX because on
                ;; linux emacs inherits the shell variables we set in our .bashrc config file. On OSX
                ;; applications do not inherit the shell environment. WTF Apple? Seriously.
                ;; needs to be after package-install-selected-packages so that exec-path package is installed
	        ;;(exec-path-from-shell-initialize)
	        (setenv "PATH" (mapconcat 'identity exec-path ":"))
	        (setenv "LANG" "en_US.UTF-8")
	        (setenv "LC_CTYPE" "en_US.UTF-8")
                
		;; actually makes the command key behave on the mac. Its awesome
		;; most of this good stuff is found at
		;; http://lojic.com/blog/2010/03/17/switching-from-carbonemacs-to-emacs-app/
		(setq mac-command-modifier 'meta)
		(setq mac-option-modifier nil)

		;; use spotlight instead of locate command to find stuff
		(setq locate-command "mdfind")

		;; this puts windows in the background on the mac
		;; who needs mark paragraph anyway?
		(global-set-key (kbd "M-h") 'ns-do-hide-emacs)

		;; I want this back to switch between emacs windows
		;; it was mapped to tmm-menubar.  Whatever
		(global-set-key [?\M-`] 'other-frame)

		;; if emacs is launched from spotlight or quicksilver it gets the
		;; default-directory / which is useless. Set it to home.
		(unless (string-match (getenv "HOME") default-directory)
		  (cd (getenv "HOME"))
		  

		  )))
	 (cond (my-unixp
		;; Don't run the emacs server if we are running minimal emacs config
		
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
			 ;; A server is already running, so do nothing.
			 ;; The old message is kept here for debugging.
                         (message "Not starting server, one instance already running.")
		       (progn
			 ;; No server is running, so start one.
                         (message "Starting emacs server...")
			 (server-start))))
		   )))
	       )
	 ;; loading and customization for third party packages, elpa and otherwise
	 (require 'my-tools)

	 ;; Load programming language modes and heavier dev tools
	 (require 'my-development)

	 (let ((elapsed (float-time (time-subtract (current-time) my-emacs-start-time))))
	   (message "After init hook completed: %.1f seconds since startup" elapsed))
	 )
	((getenv "EMACS_MINIMAL")
	 ;; (message "EMACS_MINIMAL set in environment.  Loading minimal tools")
	 ;; pager isn't loaded but I hate having C-z background the process. Use C-x z instead.
	 (global-unset-key [(control z)])
	 (setq
	  desktop-save nil  ; don't load desktop in minimal mode
	  desktop-save-mode nil
	  confirm-kill-emacs 'nil) ; don't ask to exit
	 ))

  ;; run this after darwin cond so env vars are set properly
  
  ) 
(add-hook 'after-init-hook 'my-after-init-hook)


;; https://emacs.stackexchange.com/questions/19506/suppress-warning-assignment-to-free-variable-and-others
;; Suppress assignment to free variable warnings in init file. Most of the time we're modifying other packages
;; variables.

;; Local Variables:
;; byte-compile-warnings: (not free-vars obsolete)
;; End:
(provide 'init)
;;; init.el ends here
