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
;; The mighty TAB key and all of its magic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So the tab key works like this.
;; Smart-tab owns the tab key. smart-tab is enabled globally in customization.
;; It is configured to use hippie-expand to provide completions
;; or auto-complete if auto-complete-mode is enabled

;; snippets for most modes. Custom snippets go in ~/.emacs.d/snippets
;; yasnippet autoloads ~/.emacs.d/snippets
(yas-global-mode 1)
;; yas-expand is explicitly unbound from TAB as it is at the front of the
;; list of hippie-expand functions to try which smart-tab will use
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; make sure we can get to hippie-expand at the normal spot for dabbrev
;; in case auto-complete is using tab key
(global-set-key (kbd "M-/") 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; autocomplete popups everywhere. Sometimes its even smart
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; Don't put hippie expand here because it is at the front of smart-tab list
(set-default 'ac-sources
             '(
               ac-source-yasnippet
               ac-source-imenu
               ;;ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-filename
               ac-source-abbrev
               ;;ac-source-semantic
               ))

(custom-set-variables
 '(ac-etags-requires 1))

(eval-after-load "etags"
  '(progn
     (ac-etags-setup)))
;; added to ruby mode hook in my-development.el

(global-auto-complete-mode t)
(set-default 'ac-fuzzy-enable t)
(setq ac-fuzzy-enable t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ido - makes switching between anything in emacs amazing with type-ahead search, fuzzy matching etc.
;; Can be made to work with selecting anything, commands, files, whatever
;; Imporoved buffer selection with C-x b if available
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;; enable ido style completion in M-x menu
(global-set-key (kbd "M-x") 'smex)


(ido-mode 1)    ; actually I kind of like it with file
(ido-everywhere 1)
;; use ido absolutely everywhere where completing-read would be
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1) ; default is horizontal. Not sure which I like better
;; ido-vertical mode needs more space. With horizontal ido 3 is good
(setq ido-max-window-height 30)

					;;(require 'flx-ido)
(flx-ido-mode 1) ; fuzzy matching for ido https://github.com/lewang/flx
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(define-key global-map [(control x)(control b)] 'ibuffer)
(global-set-key [(control x) (b)] 'ido-switch-buffer)

;; Fix ido-ubiquitous for newer packages
;; http://whattheemacsd.com/setup-ido.el-01.html
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

(defun ido-speedbar()
  (interactive)
  (message "update set default directory to %s and launching speedbar" default-directory)
  (speedbar 1)
  (setq default-directory ido-current-directory)
  (speedbar-update-contents)
  (setq ido-require-match nil)
  (ido-exit-minibuffer)
  )
(defun ido-my-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map "\C-t" 'ido-speedbar)
  )

(add-hook 'ido-setup-hook 'ido-my-keys)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile and project functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(projectile-global-mode t)
;; superceded by projectile
;; ffip - look back up directory tree until you find a file in ffip-project-file list
;; seaches files from there and provides list of files. Its awesome for find in projects
(setq ffip-project-file '(".git" ".svn" ".prj" "README.md")
      ffip-limit 10000 ;; MOAR FILES!!
      ffip-patterns ;; only finds files that match these extensions
      ;; '("*.html" "*.org" "*.txt" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl"
      ;;   "*.sh" "*.erl" "*.hs" "*.ml" "*.json" "*.erb" "*.coffee" "*.ejs" "*.yml"
      ;;   "*css" )
      '("*")
      ffip-find-options "-print"
      ffip-prune-patterns '("vendor" "log" "logs" ".git" "checksums" "tmp" "images"))

(defun ffip-super-project-root ()
  "Return the root of the project. It checks for the root of the project as usual by
searching up for a project root file. However, then it checks that parent directory
for a .prj file. If it exists, that parent directory is the project root. This allows
us to put mutiple 'projects' directories into a single parent directory and search
them all at once."
  ;; This is the impl from the ffip-project-root function in the main file
  (let ((project-root (or ffip-project-root
                          (if (listp ffip-project-file)
                              (some (apply-partially 'locate-dominating-file
                                                     default-directory)
                                    ffip-project-file)
                            (locate-dominating-file default-directory
                                                    ffip-project-file)))))

    (if project-root
        (let ((super-project-root (expand-file-name ".prj" (expand-file-name ".." project-root))))
          (if (file-exists-p super-project-root )
              (setq project-root (file-name-directory super-project-root)))))
    (or project-root
        (progn (message "No project was defined for the current file.")
               nil))))
(setq ffip-project-root-function 'ffip-super-project-root)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tag handling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update tags file whenever I switch files and do a tags thing.
;; sets advice on tags functions to update the file
(require 'etags-table)


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
            (find-tag tag-name)
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



;; projectile tag doesn't seem to udpate the tags in ido
(global-set-key (kbd "M-.") 'projectile-find-tag)
(global-set-key (kbd "M-p") 'projectile-find-file)
(global-set-key (kbd "M-t") 'imenu-anywhere)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ack - using ack-and-a-half  https://github.com/jhelwig/ack-and-a-half
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create shorter aliases for autoloaded ack-and-a-half functions
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(setq ack-and-a-half-prompt-for-directory 't)

(defun ack-gem (pattern &optional regexp gem)
  "ack for pattern in the gem specified.  The gem is found by using 'bundle show'
so you have to be in a ruby project that is using bundler or it will bail."
  (interactive
   (let ((regexp t))
     (list (ack-and-a-half-read regexp)
           regexp
           (read-string "name of gem to search: "))))

  (setq directory (substring (shell-command-to-string (concat "ls -d " (getenv "GEM_HOME") "/gems/" gem "* | head -1" )) 0 -1 ))
  (cond ((not (file-exists-p directory))
         (message (concat "Could not search for gem " gem " in directory " directory)))
        ((file-exists-p directory) (ack-and-a-half pattern t directory))))
(define-key my-prefix-map "a" 'ack)
(define-key my-prefix-map "A" 'ack-gem)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Other packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;brings some sanity to the page up page down commands as well as one line scolling
(require 'pager)
(global-set-key [(control v)] 'pager-page-down)
(global-set-key [next]  'pager-page-down)
(global-set-key [(meta v)] 'pager-page-up)
(global-set-key [prior] 'pager-page-up)
(global-set-key [(control q)] 'pager-row-up)
(global-set-key [(control z)] 'pager-row-down)
(global-set-key [(control Q)] 'quoted-insert)


;; multiple cursor mode
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)

;; expand region on each successive use of this.
(global-set-key (kbd "C-.") 'er/expand-region)


;; incremental search through the kill ring. Awesome.
(global-set-key "\M-\C-y" 'kill-ring-search)

;; better duplicate buffer name handling
(require 'uniquify)

(require 'newcomment) ;; loads stuff for my cool comment block function my-comment-block

(window-numbering-mode t) ; meta NUMBER to switch to a given buffer. Meta 0 is always minibuffer

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
(global-set-key (kbd "C-M-j") 'writeroom-mode)

;; perspective
;; if perspective mode is enabled either by customize or in any other file it causes weird errors with
;; ido-buffer-switch. If we load it after all lisp is loaded and files are reopened it works. Wonky.
(add-hook 'desktop-after-read-hook (lambda () (persp-mode t)))

;; duplicate lines
(define-key my-prefix-map "u" 'duplicate-thing)

;; match parens and quotes etc. Better than electric-pair-mode becuase
;; it auto-deletes adjacent matching pairs
;; works with cua-/delete-selection-mode
(autopair-global-mode)

;; Takes over these keys to let you type them fast in succesion to perform commands.
;; Using common keys can result in a slight delay on the lead key but mostly I've never noticed this
(key-chord-mode 1)
(key-chord-define-global ",." "<>\C-b")
(key-chord-define-global "xx" 'smex) ; M-x replacement
(key-chord-define-global "hh" 'switch-to-previous-buffer)
;; What other ones would be useful?

;; incremental occur awesomeness
(global-set-key (kbd "M-s i") 'ioccur)

;; Oh speedbar. How I wish you were useful
(define-key global-map [(f6)] 'speedbar)

;; visible bookmarks
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)
(global-set-key (kbd "<C-f2>") 'bm-toggle) ; new bookmark
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; buffer-move - Move buffers to other open windows by direction
;; On the mac, C-up/down/left/right switches desktops
;; However, if you press C-option-direction emacs sees it as just C-direction
;; and OSX will let it through so these keybindings work.
(global-set-key (kbd "<C-M-up>")     'buf-move-up)
(global-set-key (kbd "<C-M-down>")   'buf-move-down)
(global-set-key (kbd "<C-M-left>")   'buf-move-left)
(global-set-key (kbd "<C-M-right>")  'buf-move-right)




;; Acejump mode - marking words for direct jumping
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))

;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ))

;; enable using 'a' in dired buffers to replace the buffer instead of spawning a new one
(put 'dired-find-alternate-file 'disabled nil)

(require 'dired-single)

;;redefine dired to use this to make one dired buffer which gets reused
(defun dired (&optional path)
  (interactive)
  (dired-single-magic-buffer path))

(defun my-dired-single-hook ()
  "Bunch of stuff to run for dired, either immediately or when it's
         loaded."
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map [?^]
    (function (lambda nil (interactive)(dired-single-buffer ".."))))
  )

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-single-hook)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-single-hook))

;;Remap dired keybindings to dired-single replacement functions
(global-set-key [(f5)] 'dired-single-magic-buffer)
(global-set-key [(control f5)] (function
                                (lambda nil (interactive)
                                  (dired-single-magic-buffer
                                   default-directory))))
(global-set-key [(shift f5)] 'dired-single-toggle-buffer-name)


(provide 'my-tools)
(message "Done loading my-tools.el")