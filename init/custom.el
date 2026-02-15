;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- lexical-binding: t; -*-
;;
;; emacs customizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(CUA-mode-inhibit-delay 30000)
 '(CUA-mode-remap-cx-shift-only t)
 '(abbrev-mode t t)
 '(ag-arguments
   '("--context" "--ignore-dir=log" "--ignore-dir=vendor" "--all-text" "--smart-case" "--nogroup"
     "--column" "--"))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(auto-indent-blank-lines-on-move nil)
 '(auto-indent-disabled-modes-list
   '(compilation-mode conf-windows-mode diff-mode inferior-ess-mode dired-mode eshell-mode
		      fundamental-mode log-edit-mode makefile-gmake-mode org-mode snippet-mode
		      texinfo-mode text-mode wl-summary-mode coffee-mode yaml-mode nil))
 '(auto-indent-on-yank-or-paste nil)
 '(auto-indent-untabify-on-visit-file t)
 '(auto-save-timeout 30)
 '(auto-save-visited-file-name nil)
 '(beacon-color "#d54e53")
 '(bookmark-automatically-show-annotations nil)
 '(bookmark-default-file "~/.emacs.local/emacs.bmk")
 '(bookmark-save-flag 0)
 '(bookmark-use-annotations nil)
 '(bs-max-window-height 30)
 '(coffee-tab-width 2)
 '(color-theme-legal-frame-parameters "\\(color\\|mode\\)$")
 '(column-highlight-mode nil)
 '(comment-auto-fill-only-comments t)
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/"
     ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem"
     ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl"
     ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys"
     ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".copyarea.db"))
 '(css-indent-offset 2)
 '(cua-auto-mark-last-change t)
 '(cua-enable-cua-keys nil)
 '(cua-enable-cursor-indications t)
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "green")
 '(cua-prefix-override-inhibit-delay 1)
 '(cua-read-only-cursor-color "red")
 '(custom-safe-themes
   '("0f1341c0096825b1e5d8f2ed90996025a0d013a0978677956a9e61408fcd2c77"
     "d97ac0baa0b67be4f7523795621ea5096939a47e8b46378f79e78846e0e4ad3d"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0"
     "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8"
     "80365dd15f97396bdc38490390c23337063c8965c4556b8f50937e63b5e9a65c"
     "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983"
     "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc"
     "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad"
     "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116"
     "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da"
     "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96"
     "256bd513a9875cd855077162cdfee8d75b0ad7e18fe8b8cbc10412561fbef892"
     "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e"
     "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d"
     "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e"
     "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
     "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016"
     "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58"
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))
 '(debug-on-error nil)
 '(desktop-path '("~/.emacs.local/"))
 '(desktop-save t)
 '(desktop-save-mode nil)
 '(diary-file (expand-file-name (concat my-emacs-init-dir "/diary")))
 '(directory-abbrev-alist nil)
 '(dired-find-subdir t)
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*")
 '(dired-recursive-deletes 'top)
 '(dired-view-command-alist
   '(("[.]ps\\'" . "gv -spartan -color") ("[.]pdf\\'" . "xpdf")
     ("[.]dvi\\'" . "xdvi -sidemargin 0.5 -topmargin 1") ("[.]doc\\'" . "winword")
     ("[.]ppt\\'" . "powerpnt")))
 '(dired-x-hands-off-my-keys nil)
 '(echo-keystrokes 0.1)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eshell-directory-name "~/.emacs.local/eshell/")
 '(eshell-prefer-to-shell t nil (eshell))
 '(etags-table-search-up-depth 100)
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables
   '("PATH" "MANPATH" "LANG" "LC_CTYPE" "MY_ENG" "MY_ENV" "MY_EMAIL_PERSONAL" "MY_NAME" "GOPATH"))
 '(fci-rule-color "#073642")
 '(file-coding-system-alist
   '(("\\.elc\\'" emacs-mule . emacs-mule) ("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix)
     ("\\.reg\\'" utf-16-le . utf-16-le) ("\\.tar\\'" no-conversion . no-conversion) ("" undecided)))
 '(fill-column 100)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(github-browse-file-show-line-at-point t)
 '(glasses-face 'bold)
 '(glasses-original-separator "")
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(global-auto-complete-mode nil)
 '(global-auto-revert-mode t)
 '(global-hungry-delete-mode nil)
 '(global-smart-tab-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(global-visual-line-mode t)
 '(grep-command "grep -2EInir ")
 '(grep-highlight-matches 'auto-detect)
 '(grep-use-null-device 'auto-detect)
 '(help-window-select t)
 '(hippie-expand-try-functions-list
   '(yas-hippie-try-expand try-complete-file-name-partially try-complete-file-name
			   try-expand-all-abbrevs try-expand-list try-expand-dabbrev
			   try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
			   try-complete-lisp-symbol-partially try-complete-lisp-symbol))
 '(hs-isearch-open t)
 '(icicle-mode t)
 '(icicle-show-Completions-initially-flag t)
 '(indent-tabs-mode nil)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(large-file-warning-threshold 100000000)
 '(mail-default-directory "~/.emacs.local/")
 '(mail-source-directory "~/.emacs.local/Mail/")
 '(major-mode 'org-mode)
 '(make-backup-files t)
 '(menu-bar-mode nil)
 '(message-auto-save-directory "~/.emacs.local/Mail/drafts/")
 '(message-directory "~/.emacs.local/Mail/")
 '(minibuffer-auto-raise nil)
 '(mode-line-format
   '("%e " mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified
     mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   "
     mode-line-position (vc-mode vc-mode) mode-line-misc-info mode-line-modes mode-line-end-spaces))
 '(mouse-scroll-min-lines 2)
 '(mouse-wheel-progressive-speed t)
 '(neo-autorefresh t)
 '(neo-smart-open t)
 '(neo-theme 'nerd)
 '(neo-window-width 40)
 '(nnmail-message-id-cache-file "~/.emacs.local/.nnmail-cache")
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(nxml-slash-auto-complete-flag t)
 '(objed-cursor-color "#dc322f")
 '(org-default-notes-file "~/.jjr-drive/RIGHT_NOW.txt")
 '(org-reverse-note-order t)
 '(org-startup-indented t)
 '(package-selected-packages
   '(ac-etags ace-jump-mode ack add-node-modules-path ag aggressive-indent ai-code alchemist
	      all-the-icons all-the-icons-completion all-the-icons-dired all-the-icons-ibuffer
	      auto-indent-mode bm browse-kill-ring buffer-move bundler centered-window coffee-mode
	      color-identifiers-mode color-theme-sanityinc-tomorrow company-go company-shell
	      crontab-mode csv-mode ctags-update dash-functional doom-modeline doom-themes
	      dracula-theme duplicate-thing ecb edit-server egg embrace emmet-mode enh-ruby-mode epc
	      exec-path-from-shell fireplace flx-ido flycheck-pyflakes fold-dwim fold-this
	      format-sql fuzzy ggtags gist git-timemachine github-browse-file gmail-message-mode
	      go-autocomplete go-direx go-projectile go-scratch go-stacktracer groovy-mode haml-mode
	      helm helm-lsp hungry-delete hydra ibuffer-vc ido-vertical-mode idomenu iedit
	      imenu-anywhere ioccur jira js2-highlight-vars js2-refactor json-mode key-chord
	      log4j-mode lsp-intellij lsp-java lsp-mode lsp-ui lua-mode magit markdown-mode mmm-mode
	      multiple-cursors neotree nerd-icons-completion nerd-icons-dired nerd-icons-grep
	      nerd-icons-ibuffer osx-plist pager persp-projectile pig-mode pig-snippets pos-tip
	      prettier-js projectile-rails protobuf-mode python-mode rainbow-delimiters rbenv
	      real-auto-save rspec-mode ruby-end ruby-interpolation ruby-tools rvm scss-mode
	      smart-tab smartparens smex sql-indent tern toggle-quotes treemacs treemacs-projectile
	      undo-tree uniquify use-package vertica vterm wgrep window-numbering writeroom-mode
	      xml-rpc xref-js2 yafolding yaml-mode yasnippet-snippets))
 '(ps-line-number t)
 '(ps-line-number-color 50)
 '(python-indent-offset 4)
 '(rmail-secondary-file-directory "~/.emacs.local/")
 '(rng-nxml-auto-validate-flag nil)
 '(rvm-configuration-file-name ".ruby-version")
 '(save-abbrevs 'silently)
 '(save-place-file "~/.emacs.local/emacs-places")
 '(save-place-mode t nil (saveplace))
 '(semantic-inhibit-functions '(my-semantic-dont-parse))
 '(semanticdb-default-save-directory "~/.emacs.local/semantic.cache")
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis)
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(size-indication-mode t)
 '(smart-tab-disabled-major-modes '(org-mode term-mode eshell-mode Custom-mode))
 '(smart-tab-using-hippie-expand t)
 '(solaire-global-mode nil)
 '(sql-connection-alist
   '(("Automation QA" (sql-product 'mysql) (sql-user "jorussell") (sql-server "vitess.hubteamqa.com")
      (sql-database "Automation"))
     ("Automation Platform Prod" (sql-product 'mysql) (sql-user "jorussell")
      (sql-server "vitess.hubteam.com") (sql-database "AutomationPlatform"))
     ("Automation Platform QA" (sql-product 'mysql) (sql-user "jorussell")
      (sql-server "vitess.hubteamqa.com") (sql-database "AutomationPlatform"))))
 '(stack-trace-on-error nil t)
 '(tags-revert-without-query t)
 '(tail-hide-delay 0)
 '(tail-max-size 0)
 '(tail-volatile nil)
 '(todo-file-do "~/.emacs.local/todo-do")
 '(todo-file-done "~/.emacs.local/todo-done")
 '(todo-file-top "~/.emacs.local/todo-top")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f") (40 . "#cb4b16") (60 . "#b58900") (80 . "#859900") (100 . "#2aa198")
     (120 . "#268bd2") (140 . "#d33682") (160 . "#6c71c4") (180 . "#dc322f") (200 . "#cb4b16")
     (220 . "#b58900") (240 . "#859900") (260 . "#2aa198") (280 . "#268bd2") (300 . "#d33682")
     (320 . "#6c71c4") (340 . "#dc322f") (360 . "#cb4b16")))
 '(vc-annotate-very-old-color nil)
 '(vc-cvs-diff-switches "-up")
 '(vc-diff-switches "-pu")
 '(which-function-mode t)
 '(whitespace-check-indent-whitespace nil)
 '(whitespace-global-mode nil nil (whitespace))
 '(whitespace-line-column 150)
 '(whitespace-space-regexp "\\(^ +\\| +$\\)")
 '(whitespace-style
   '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab
	  tab-mark))
 '(window-numbering-auto-assign-0-to-minibuffer t)
 '(window-numbering-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(col-highlight ((t (:background "gray15"))))
 '(js2-highlight-vars-face ((t (:underline t))))
 '(js2-highlight-vars-second-face ((t (:underline (:color foreground-color :style wave)))))
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


;; Local Variables:
;; byte-compile-warnings: (not free-vars obsolete)
;; End:
