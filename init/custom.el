;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 '(ac-auto-show-menu t)
 '(ac-auto-start 2)
 '(ac-delay 0.1)
 '(ac-etags-requires 1)
 '(ac-use-fuzzy t)
 '(ack-and-a-half-arguments
   '("--ignore-dir=vendor" "--ignore-dir=log" "--column" "--context"))
 '(ack-and-a-half-executable "/usr/local/bin/ag")
 '(ack-and-a-half-prompt-for-directory t t)
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
 '(auto-indent-global-mode t nil (auto-indent-mode))
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
 '(canlock-password "e0f10ec3fe2e8bcd3d1b789cfcad28b660ba3672")
 '(clearcase-checkout-arguments '("-unr"))
 '(clearcase-checkout-switches "-unr")
 '(clearcase-diff-on-checkin t)
 '(clearcase-use-normal-diff t)
 '(coffee-tab-width 2)
 '(color-theme-legal-frame-parameters "\\(color\\|mode\\)$")
 '(column-highlight-mode nil)
 '(comment-auto-fill-only-comments t)
 '(company-backends
   '(company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode
                  company-cmake company-capf
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-files company-dabbrev))
 '(company-idle-delay 0.5)
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/"
     ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem"
     ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl"
     ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys"
     ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".copyarea.db"))
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
 '(custom-enabled-themes '(doom-spacegrey))
 '(custom-safe-themes
   '("77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
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
 '(ecb-create-layout-file "~/.emacs.d/init/ecb-user-layouts.el")
 '(ecb-directories-menu-user-extension-function 'ignore)
 '(ecb-download-url "http://prdownloads.sourceforge.net/ecb/")
 '(ecb-excluded-directories-regexp "^\\(CVS\\|.\\|..\\)$")
 '(ecb-excluded-directories-regexps '("^\\(\\.git\\|\\.sass-cache\\|\\.\\|\\.\\.\\)$"))
 '(ecb-fix-window-size 'width)
 '(ecb-history-menu-user-extension-function 'ignore)
 '(ecb-layout-name "left15")
 '(ecb-layout-window-sizes
   '(("my-ecb-layout-leftright" (ecb-directories-buffer-name 0.23267326732673269 . 0.9821428571428571)
      (ecb-methods-buffer-name 0.23267326732673269 . 0.625)
      (ecb-history-buffer-name 0.23267326732673269 . 0.35714285714285715))
     ("left15" (ecb-directories-buffer-name 0.3016759776536313 . 0.5760869565217391)
      (ecb-methods-buffer-name 0.3016759776536313 . 0.41304347826086957))
     ("leftright1" (ecb-directories-buffer-name 0.212707182320442 . 0.5869565217391305)
      (ecb-sources-buffer-name 0.212707182320442 . 0.043478260869565216)
      (ecb-history-buffer-name 0.212707182320442 . 0.358695652173913)
      (ecb-methods-buffer-name 0.20165745856353592 . 0.9891304347826086))
     ("leftright2" (ecb-directories-buffer-name 0.143646408839779 . 0.6521739130434783)
      (ecb-sources-buffer-name 0.143646408839779 . 0.33695652173913043)
      (ecb-methods-buffer-name 0.16298342541436464 . 0.6521739130434783)
      (ecb-history-buffer-name 0.16298342541436464 . 0.33695652173913043))
     ("left3" (ecb-directories-buffer-name 0.3016759776536313 . 0.29347826086956524)
      (ecb-sources-buffer-name 0.3016759776536313 . 0.34782608695652173)
      (ecb-methods-buffer-name 0.3016759776536313 . 0.34782608695652173))))
 '(ecb-major-modes-show-or-hide '(nil))
 '(ecb-methods-menu-user-extension-function 'ignore)
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
 '(ecb-show-sources-in-directories-buffer 'always)
 '(ecb-sources-menu-user-extension-function 'ignore)
 '(ecb-tip-of-the-day nil)
 '(ecb-tip-of-the-day-file "~/.emacs.local/ecb-tip-of-day.el")
 '(ecb-toggle-layout-sequence '("left15" "left7" "leftright1"))
 '(ecb-tree-incremental-search 'substring)
 '(ecb-tree-indent 2)
 '(ecb-tree-truncate-lines '(ecb-directories-buffer-name ecb-methods-buffer-name))
 '(ecb-use-speedbar-instead-native-tree-buffer 'nil)
 '(ecb-wget-setup 'cons)
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
 '(flx-ido-mode t)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flymake-no-changes-timeout 10)
 '(flymake-start-syntax-check-on-newline nil)
 '(frame-background-mode 'dark)
 '(github-browse-file-show-line-at-point t)
 '(glasses-face 'bold)
 '(glasses-original-separator "")
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(global-auto-complete-mode nil)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-hungry-delete-mode nil)
 '(global-smart-tab-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(global-visual-line-mode t)
 '(global-writeroom-mode nil nil (writeroom-mode))
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
 '(ido-create-new-buffer 'prompt)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-max-file-prompt-width 0)
 '(ido-max-prospects 0)
 '(ido-max-window-height 20)
 '(ido-use-faces t)
 '(ido-use-filename-at-point 'guess)
 '(ido-use-url-at-point t)
 '(ido-vertical-disable-if-short nil)
 '(ido-vertical-mode t)
 '(indent-tabs-mode nil)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message nil)
 '(jde-build-function '(jde-ant-build))
 '(jde-check-version-flag nil)
 '(jde-compile-option-classpath '("."))
 '(jde-compile-option-debug '("all" (t nil nil)))
 '(jde-compile-option-sourcepath '("."))
 '(jde-complete-function 'jde-complete-minibuf)
 '(jde-complete-signature-display '("Eldoc"))
 '(jde-debugger '("JDEbug"))
 '(jde-enable-abbrev-mode t)
 '(jde-gen-conditional-padding-2 "")
 '(jde-gen-k&r t)
 '(jde-gen-method-signature-padding-2 "")
 '(jde-gen-method-signature-padding-3 " ")
 '(jde-global-classpath '("."))
 '(jde-jdk-registry '(("1.4.2" . "$JAVA_HOME")))
 '(jde-sourcepath '("."))
 '(jdee-db-active-breakpoint-face-colors (cons "#FFFBF0" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#FFFBF0" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#FFFBF0" "#E1DBCD"))
 '(jdee-server-dir "~/eng/tools/jdee-server/target")
 '(jira-url "http://jira.tapjoy.net/rpc/xmlrpc")
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(large-file-warning-threshold 100000000)
 '(ldap-host-parameters-alist
   '(("ldap.cisco.com" base "o=cisco.com" timelimit 10 sizelimit 100)))
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
   '(ac-etags ace-jump-mode ack add-node-modules-path ag aggressive-indent alchemist auto-indent-mode
              bm browse-kill-ring buffer-move bundler centered-window coffee-mode
              color-identifiers-mode color-theme-sanityinc-tomorrow company-go company-shell
              crontab-mode csv-mode ctags-update dash-functional doom-modeline doom-themes
              dracula-theme duplicate-thing ecb edit-server egg embrace emmet-mode enh-ruby-mode epc
              exec-path-from-shell fireplace flx-ido flycheck-pyflakes flymake-coffee flymake-go
              flymake-jslint flymake-json flymake-python-pyflakes flymake-ruby flymake-sass
              flymake-shell fold-dwim fold-this format-sql fuzzy ggtags gist git-timemachine
              github-browse-file gmail-message-mode go-autocomplete go-direx go-projectile
              go-scratch go-stacktracer groovy-mode haml-mode helm helm-lsp hungry-delete hydra
              ibuffer-vc ido-vertical-mode idomenu iedit imenu-anywhere ioccur jira
              js2-highlight-vars js2-refactor json-mode key-chord log4j-mode lsp-intellij lsp-java
              lsp-mode lsp-ui lua-mode magit markdown-mode mmm-mode multiple-cursors neotree
              osx-plist pager persp-projectile pig-mode pig-snippets pos-tip projectile-rails
              protobuf-mode python-mode rainbow-delimiters rbenv real-auto-save rspec-mode ruby-end
              ruby-interpolation ruby-tools rvm scss-mode smart-tab smartparens smex sql-indent tern
              toggle-quotes treemacs treemacs-projectile undo-tree use-package vertica wgrep
              window-numbering writeroom-mode xml-rpc xref-js2 yafolding yaml-mode
              yasnippet-snippets))
 '(prettier-js-command "prettier")
 '(projectile-cache-file "~/.emacs.local/projectile.cache")
 '(projectile-enable-caching nil)
 '(projectile-globally-ignored-files '("*.elc" "TAGS"))
 '(projectile-project-root-files
   '("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt"
     "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs"
     "Rakefile"))
 '(projectile-remember-window-configs t)
 '(projectile-sort-order 'recently-active)
 '(projectile-switch-project-action 'projectile-commander)
 '(projectile-tags-backend 'find-tag)
 '(ps-line-number t)
 '(ps-line-number-color 50)
 '(python-indent-offset 4 t)
 '(rmail-secondary-file-directory "~/.emacs.local/")
 '(rng-nxml-auto-validate-flag nil)
 '(rspec-command-options "--format documentation --drb")
 '(rspec-use-bundler-when-possible nil)
 '(rspec-use-rake-when-possible nil)
 '(rspec-use-rvm t)
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
 '(speedbar-before-visiting-file-hook '(push-mark))
 '(speedbar-default-position 'left-right)
 '(speedbar-frame-parameters
   '((minibuffer) (width . 50) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0)
     (unsplittable . t)))
 '(speedbar-indentation-width 2)
 '(speedbar-show-unknown-files t)
 '(speedbar-supported-extension-expressions
   '(".org" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp"
     ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py"
     ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" ".rb"))
 '(speedbar-use-images t)
 '(speedbar-visiting-file-hook nil)
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
 '(treemacs-collapse-dirs 3)
 '(treemacs-width 45)
 '(treemacs-wrap-around nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-min-dir-content 1)
 '(uniquify-trailing-separator-p t)
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
 '(window-numbering-mode nil)
 '(writeroom-border-width 0)
 '(writeroom-disable-fringe t)
 '(writeroom-fullscreen-effect 'maximized)
 '(writeroom-global-effects
   '(writeroom-set-fullscreen writeroom-set-alpha writeroom-set-menu-bar-lines
                              writeroom-set-tool-bar-lines writeroom-set-vertical-scroll-bars
                              writeroom-set-internal-border-width))
 '(writeroom-maximize-window nil)
 '(writeroom-mode-line t)
 '(writeroom-restore-window-config t)
 '(writeroom-width 150))
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
