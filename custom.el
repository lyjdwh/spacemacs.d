;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil t)
 '(command-log-mode-window-size 50)
 '(company-dabbrev-minimum-length 3)
 '(company-dabbrev-other-buffers nil)
 '(company-show-numbers t)
 '(company-statistics-auto-restore nil)
 '(ctags-update-delay-seconds 1024)
 '(erc-nick "liuyan")
 '(erc-port 6666)
 '(evil-want-C-i-jump t)
 '(evil-want-Y-yank-to-eol nil)
 '(exec-path-from-shell-arguments '("-l"))
 '(expand-region-contract-fast-key "V")
 '(expand-region-exclude-text-mode-expansions '(html-mode nxml-mode web-mode))
 '(expand-region-reset-fast-key "r")
 '(flycheck-python-flake8-executable "/home/liuyan/.conda/envs/torch/bin/python")
 '(global-command-log-mode nil)
 '(helm-buffer-max-length 56)
 '(helm-move-to-line-cycle-in-source t)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(ivy-height 18)
 '(lsp-flycheck-live-reporting nil)
 '(lsp-ui-flycheck-live-reporting nil)
 '(lua-documentation-url "http://www.lua.org/manual/5.3/manual.html")
 '(magit-use-overlays nil)
 '(only-global-abbrevs t)
 '(org-agenda-custom-commands nil)
 '(org-agenda-ndays 1)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-text-search-extra-files '(agenda-archives))
 '(org-deadline-warning-days 14)
 '(org-fast-tag-selection-single-key 'expert)
 '(org-log-into-drawer t)
 '(org-pomodoro-play-sounds nil)
 '(org-reverse-note-order t)
 '(package-selected-packages
   '(speed-type ein polymode anaphora websocket highlight-indent-guides super-save company-english-helper pandoc-mode ox-pandoc wakatime-mode xkcd helpful elisp-refs loop zetteldeft flyspell-popup git-gutter-fringe git-gutter benchmark-init pdf-tools grip-mode vmd-mode org-preview-html company-tabnine utop tuareg caml seeing-is-believing ruby-refactor ruby-hash-syntax rubocopfmt ocp-indent ob-elixir mvn meghanada maven-test-mode lsp-java groovy-mode groovy-imports gradle-mode flycheck-ocaml merlin flycheck-mix flycheck-credo dune alchemist elixir-mode zeal-at-point insert-shebang flycheck-bashate fish-mode fasd counsel-dash dash-docs company-shell treemacs-persp magit-section pyim-basedict xr terminal-here flycheck-elsa leetcode aio ycmd request-deferred treemacs-magit vterm attrap hybrid-mode lispyville anki-editor pyim pangu-spacing find-by-pinyin-dired chinese-wbim chinese-conv ace-pinyin pinyinlib python unicode-escape plantuml-mode dap-mode bui tree-mode lsp-ui lsp-treemacs lsp-python-ms helm-lsp cquery company-lsp ccls forge closql emacsql-sqlite emacsql cpp-auto-include company-reftex parseedn lv parseclj a ox-hugo emojify emoji-cheat-sheet-plus company-emoji ob-typescript deadgrep org-cliplink devdocs ssh-agency org-projectile org-category-capture treemacs-projectile treemacs-evil treemacs pfuture transient osx-clipboard lsp-haskell lsp-mode flycheck-package package-lint evil-textobj-line blacken dante lcr company-ghc ghc helm-hoogle intero hlint-refactor hindent haskell-snippets flycheck-haskell company-ghci haskell-mode company-cabal cmm-mode imenu-list writeroom-mode visual-fill-column symbol-overlay treepy graphql sound-wav caps-lock doom-modeline eldoc-eval shrink-path ivy-rich prettier-js ivy-yasnippet gitignore-templates evil-goggles sesman dotenv-mode rjsx-mode magit-svn json-navigator hierarchy yasnippet-snippets spaceline-all-the-icons all-the-icons memoize pippel pipenv overseer org-mime nameless ivy-xref ivy-rtags importmagic epc concurrent google-c-style flycheck-rtags evil-cleverparens counsel-gtags counsel-css company-rtags rtags clojure-cheatsheet centered-cursor-mode font-lock+ ghub let-alist seq restclient-helm org-brain sayid evil-lion auctex-latexmk auctex password-generator realgud test-simple loc-changes load-relative company-lua blog-admin string-inflection opencl-mode cuda-mode symon rspec-mode fuzzy browse-at-remote winum helm-swoop unfill highlight-global marshal ht ob-restclient company-restclient know-your-http-well counsel-projectile lispy counsel swiper ivy-purpose hide-comnt helm-purpose window-purpose zoutline minitest glsl-mode pug-mode magithub editorconfig dockerfile-mode docker tablist docker-tramp helm-projectile xterm-color shell-pop eshell-z eshell-prompt-extras esh-help graphviz-dot-mode py-isort dumb-jump restclient racket-mode faceup projectile-rails ob-http helm-gtags feature-mode company-auctex rvm ruby-tools ruby-test-mode rubocop robe rbenv rake enh-ruby-mode chruby bundler inf-ruby yapfify sicp helm-mode-manager org origami tiny evil-unimpaired helm-pydoc unicode-whitespace github-search flycheck-clojure evil-escape mwim helm-github-stars fcitx solarized-theme tide typescript-mode spaceline powerline org-plus-contrib ivy-hydra helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-ag helm helm-core flyspell-correct-ivy color-identifiers-mode ag bracketed-paste paradox inflections cider names yaml-mode which-key wgrep uuidgen toc-org smex smeargle smartparens reveal-in-osx-finder restart-emacs ranger pytest py-yapf prodigy persp-mode pcre2el osx-trash org-pomodoro mmm-mode markdown-mode lua-mode live-py-mode link-hint launchctl js2-mode jade-mode info+ ibuffer-projectile projectile hy-mode htmlize hl-todo help-fns+ haml-mode gnuplot gitignore-mode github-clone popup git-gutter-fringe+ git-gutter+ flyspell-correct flycheck evil-visual-mark-mode evil-magit magit-popup git-commit with-editor evil-indent-plus iedit evil-ediff evil undo-tree diminish diff-hl ivy tern company column-enforce-mode cmake-mode clojure-snippets eval-sexp-fu pkg-info clojure-mode bind-map bind-key yasnippet auto-compile packed anaconda-mode pythonic ace-window ace-link avy quelpa package-build wrap-region visual-regexp-steroids visual-regexp peep-dired osx-dictionary nodejs-repl litable keyfreq gulpjs find-file-in-project etags-select ctags-update beacon 4clojure moe-theme edn paredit queue peg json-rpc dash-functional web-completion-data makey anzu highlight goto-chg flx gh logito pcache pos-tip guide-key request parent-mode simple-httpd json-snatcher json-reformat multiple-cursors moz ctable orglue epic alert log4e gntp spinner epl hydra async deferred f s chinese-word-at-point dash youdao-dictionary ws-butler web-mode web-beautify volatile-highlights vi-tilde-fringe use-package tagedit smooth-scrolling slim-mode scss-mode sass-mode rfringe reveal-in-finder rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pyenv-mode popwin pip-requirements persp-projectile pbcopy page-break-lines ox-reveal org-repo-todo org-present org-octopress org-mac-link org-download org-bullets open-junk-file neotree multi-term moz-controller move-text monokai-theme markdown-toc magit macrostep linum-relative leuven-theme less-css-mode json-mode js2-refactor js-doc indent-guide impatient-mode ido-vertical-mode hungry-delete hl-anything highlight-parentheses highlight-numbers highlight-indentation guide-key-tip google-translate golden-ratio github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh-md ggtags geiser fringe-helper flycheck-ycmd flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-tutor evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-args evil-anzu engine-mode emmet-mode elisp-slime-nav elfeed discover-my-major deft dash-at-point cython-mode company-ycmd company-web company-tern company-statistics company-quickhelp company-c-headers company-anaconda command-log-mode coffee-mode cmake-font-lock clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu chinese-fonts-setup buffer-move auto-yasnippet auto-highlight-symbol auto-dictionary align-cljlet aggressive-indent adaptive-wrap ace-jump-mode ac-ispell 2048-game))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((git-commit-major-mode . git-commit-elisp-text-mode)
     (typescript-backend . tide)
     (javascript-backend . tern)
     (eval progn
           (pp-buffer)
           (indent-buffer))
     (typescript-backend . lsp)
     (javascript-backend . lsp)
     (eval setenv "PYTHONPATH" "/home/liuyan/.conda/envs/torch/bin")))
 '(sp-show-pair-from-inside t)
 '(tags-revert-without-query t)
 '(vc-follow-symlinks t)
 '(web-mode-markup-indent-offset 2)
 '(ycmd-extra-conf-handler 'load)
 '(ycmd-extra-conf-whitelist '("~/cocos2d-x/*")))
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(snails-content-buffer-face ((t (:background "#111" :height 120))))
 '(snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.2))))
 '(snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 120)))))
