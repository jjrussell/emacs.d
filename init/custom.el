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
 '(abbrev-mode t)
 '(ag-arguments
   '("--context" "--ignore-dir=log" "--ignore-dir=vendor" "--all-text" "--smart-case" "--nogroup"
     "--column" "--"))
 '(ai-code-onboarding-seen t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auto-save-timeout 30)
 '(auto-save-visited-file-name nil)
 '(bookmark-automatically-show-annotations nil)
 '(bookmark-default-file "~/.emacs.local/emacs.bmk")
 '(bookmark-save-flag 0)
 '(bookmark-use-annotations nil)
 '(bs-max-window-height 30)
 '(comment-auto-fill-only-comments t)
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/"
     ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem"
     ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl"
     ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys"
     ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".copyarea.db"))
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("5c8a1b64431e03387348270f50470f64e28dfae0084d33108c33a81c1e126ad6"
     "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875"
     "6fc9e40b4375d9d8d0d9521505849ab4d04220ed470db0b78b700230da0a86c1"
     "7771c8496c10162220af0ca7b7e61459cb42d18c35ce272a63461c0fc1336015"
     "0f1341c0096825b1e5d8f2ed90996025a0d013a0978677956a9e61408fcd2c77"
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
 '(diary-file (expand-file-name (concat my-emacs-init-dir "/diary")))
 '(directory-abbrev-alist nil)
 '(dired-find-subdir t)
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*")
 '(dired-recursive-deletes 'top)
 '(dired-x-hands-off-my-keys nil)
 '(echo-keystrokes 0.1)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eshell-directory-name "~/.emacs.local/eshell/")
 '(eshell-prefer-to-shell t nil (eshell))
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables
   '("PATH" "MANPATH" "LANG" "LC_CTYPE" "MY_ENG" "MY_ENV" "MY_EMAIL_PERSONAL" "MY_NAME" "GOPATH"))
 '(fill-column 100)
 '(github-browse-file-show-line-at-point t)
 '(glasses-face 'bold)
 '(glasses-original-separator "")
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(global-auto-revert-mode t)
 '(global-smart-tab-mode t)
 '(global-subword-mode t)
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
 '(indent-tabs-mode nil)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(large-file-warning-threshold 100000000)
 '(mail-default-directory "~/.emacs.local/")
 '(mail-source-directory "~/.emacs.local/Mail/")
 '(major-mode 'text-mode)
 '(make-backup-files t)
 '(menu-bar-mode nil)
 '(message-auto-save-directory "~/.emacs.local/Mail/drafts/")
 '(message-directory "~/.emacs.local/Mail/")
 '(minibuffer-auto-raise nil)
 '(mouse-scroll-min-lines 2)
 '(mouse-wheel-progressive-speed t)
 '(nnmail-message-id-cache-file "~/.emacs.local/.nnmail-cache")
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   '(ack add-node-modules-path ag agent-shell agent-shell-macext agent-shell-manager aggressive-indent
         ai-code bm buffer-move cape closql cond-let consult consult-eglot corfu
         crontab-mode csv-mode diff-hl diminish dired-subtree doom-modeline doom-themes
         duplicate-thing editorconfig eglot-java emacsql emmet-mode exec-path-from-shell expreg
         flymake forge format-sql ghub gist git-timemachine github-browse-file go-mode gptel
         gptel-agent grip-mode groovy-mode hydra ibuffer-vc iedit imenu-anywhere inf-ruby jira
         key-chord log4j-mode lua-mode magit marginalia markdown-mode markdown-toc mixed-pitch
         multiple-cursors nerd-icons-completion nerd-icons-dired nerd-icons-grep nerd-icons-ibuffer
         olivetti orderless org org-modern pager peg projectile projectile-rails protobuf-mode
         rainbow-delimiters reformatter rg rspec-mode ruby-end ruby-interpolation rvm smart-tab
         sql-indent string-inflection toggle-quotes transpose-frame treemacs treemacs-magit
         treemacs-nerd-icons treemacs-projectile valign verilog-mode vertico
         vterm vundo web-mode wgrep winum writeroom-mode ws-butler yaml-mode yasnippet
         yasnippet-snippets))
 '(package-vc-selected-packages
   '((agent-shell-macext :url "https://github.com/cxa/agent-shell-macext")
     (agent-shell-manager :url "https://github.com/jethrokuan/agent-shell-manager")))
 '(ps-line-number t)
 '(ps-line-number-color 50)
 '(python-indent-offset 4)
 '(rmail-secondary-file-directory "~/.emacs.local/")
 '(rng-nxml-auto-validate-flag nil)
 '(rvm-configuration-file-name ".ruby-version")
 '(save-abbrevs 'silently)
 '(save-place-file "~/.emacs.local/emacs-places")
 '(save-place-mode t nil (saveplace))
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis)
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(size-indication-mode t)
 '(smart-tab-disabled-major-modes '(org-mode term-mode eshell-mode Custom-mode))
 '(smart-tab-using-hippie-expand t)
 '(sql-connection-alist
   '(("Automation QA" (sql-product 'mysql) (sql-user "jorussell") (sql-server "vitess.hubteamqa.com")
      (sql-database "Automation"))
     ("Automation Platform Prod" (sql-product 'mysql) (sql-user "jorussell")
      (sql-server "vitess.hubteam.com") (sql-database "AutomationPlatform"))
     ("Automation Platform QA" (sql-product 'mysql) (sql-user "jorussell")
      (sql-server "vitess.hubteamqa.com") (sql-database "AutomationPlatform"))))
 '(tags-revert-without-query t)
 '(vc-cvs-diff-switches "-up")
 '(vc-diff-switches "-pu")
 '(which-function-mode t)
 '(whitespace-line-column 150)
 '(whitespace-space-regexp "\\(^ +\\| +$\\)")
 '(whitespace-style
   '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab
          tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(underline ((t (:underline nil)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars obsolete)
;; End:
