;;; packages.el --- zilongshanren Layer packages File for Spacemacs;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <lyjdwh@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq zilongshanren-programming-packages
      '(
        css-mode
        paredit
        lispy
        cmake-mode
        flycheck
        (nodejs-repl-eval :location local)
        (compile-dwim :location local)
        js2-mode
        js2-refactor
        json-mode
        racket-mode
        yasnippet
        web-mode
        js-doc
        lua-mode
        (cc-mode :location built-in)
        ;; flycheck-clojure
        ;; etags-select
        (python :location built-in)
        (emacs-lisp :location built-in)
        ;; clojure-mode
        company
        (eldoc :location built-in)
        dumb-jump
        graphviz-dot-mode
        cider
        ;; editorconfig
        robe
        lsp-mode
        typescript-mode
        company-tabnine
        lsp-python-ms
        lsp-pyright
        auctex
        highlight-indent-guides
        (color-rg :location (recipe :fetcher github :repo "manateelazycat/color-rg"))
        hl-todo
        counsel-dash
        (webkit-katex-render :location (recipe :fetcher github :repo "fuxialexander/emacs-webkit-katex-render"))
        (lpy :location (recipe :fetcher github :repo "abo-abo/lpy"))
        taskrunner
        (ivy-taskrunner :location (recipe :fetcher github :repo "emacs-taskrunner/ivy-taskrunner"))
        counsel-etags
        magit-todos
        ))

(defun zilongshanren-programming/init-magit-todos ()
  (use-package magit-todos
    :after magit
    :config
    (magit-todos-mode 1)
    ))

(defun zilongshanren-programming/init-counsel-etags ()
  (use-package counsel-etags
    :ensure t
    :commands counsel-etags-find-tag counsel-etags-find-tag-at-point counsel-etags-list-tag
    :config
    (setq counsel-etags-update-interval 60)
    (push "build" counsel-etags-ignore-directories)
    ;; Don't ask before rereading the TAGS files if they have changed
    (setq tags-revert-without-query t)
    ;; Don't warn when TAGS files are large
    (setq large-file-warning-threshold nil)
    (setq counsel-etags-ctags-program "/usr/local/bin/ctags")
    (add-hook 'prog-mode-hook
              (lambda ()
                (add-hook 'after-save-hook
                          'counsel-etags-virtual-update-tags 'append 'local)))
    ))

(defun zilongshanren-programming/init-taskrunner ()
  (use-package taskrunner
    :after ivy-taskrunner
    ))

(defun zilongshanren-programming/init-ivy-taskrunner ()
  (use-package ivy-taskrunner
    :commands ivy-taskrunner ivy-taskrunner-rerun-last-command
    ))

(defun zilongshanren-programming/init-lpy ()
  (use-package lpy
    :defer t
    :config
    ;; fix key conflict between lpy and lsp
    (define-key lpy-mode-map "(" nil)
    ))

(defun zilongshanren-programming/init-webkit-katex-render ()
  (use-package webkit-katex-render
    :commands webkit-katex-render-mode
    ))

(defun zilongshanren-programming/post-init-counsel-dash ()
  (progn
    (add-hook 'sh-mode-hook (lambda () (setq-local counsel-dash-docsets '("Bash"))))
    (add-hook 'LaTeX-mode-hook (lambda () (setq-local counsel-dash-docsets '("LaTeX"))))
    (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
    (add-hook 'c++-mode-hook (lambda () (setq-local counsel-dash-docsets '("C++"))))
    (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python 3" "PyTorch" "NumPy" "Matplotlib" "Pandas" "scikit-learn" "torchvision" "torchtext"))))
    (setq counsel-dash-enable-debugging nil)
    ))

(defun zilongshanren-programming/post-init-hl-todo ()
  ;; Highlight TODO and similar keywords in comments and strings
  ;; PROG OKAY DONE THEM NOTE KLUDGE TEMP TRICK HACK TODO NEXT FIXME XXX
  ;; ISSUE BUG  DONT FAIL
  (progn
    (dolist (keyword '("BUG" "ISSUE"))
      (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
    (dolist (keyword '("HACK" "TRICK"))
      (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces))))

(defun zilongshanren-programming/init-color-rg ()
  (use-package color-rg
    :commands (color-rg-search-input color-rg-search-symbol color-rg-search-input-in-project
               color-rg-search-symbol-in-project color-rg-search-input-in-current-file
               color-rg-search-symbol-in-current-file  color-rg-search-symbol-with-type
               color-rg-search-project-with-type)
    ))

(defun zilongshanren-programming/init-highlight-indent-guides ()
  (use-package highlight-indent-guides
    :diminish
    :hook ((prog-mode web-mode nxml-mode) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-method 'character)
    (highlight-indent-guides-responsive 'top)
    (highlight-indent-guides-delay 0)
    (highlight-indent-guides-auto-character-face-perc 7)
    ))

(defun zilongshanren-programming/post-init-auctex ()
  (add-hook 'LaTeX-mode-hook
        (lambda ()
          (setq TeX-auto-untabify t     ; remove all tabs before saving
                TeX-engine 'xetex     ; use xelatex default
                ;; TeX-view-program-selection '((output-pdf "Okular"))
                ;; TeX-view-program-selection '((output-pdf "Emacs"))
                ;; TeX-view-program-list '(("Emacs" "emacsclient -n %o"))
                TeX-view-program-selection '((output-pdf "PDF Tools"))
                TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                TeX-source-correlate-mode t
                TeX-source-correlate-start-server t
                TeX-source-correlate-method 'synctex
                TeX-PDF-mode t
                reftex-plug-into-AUCTeX t
                Tex-save-query nil
                ;; TeX-view-program-list
                ;; '(("Okular" "okular --unique %o#src:%n`pwd`/./%b")
                ;;   ("Skim" "displayline -b -g %n %o %b")
                ;;   ("Zathura"
                ;;    ("zathura %o"
                ;;     (mode-io-correlate
                ;;      " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\""))))
                )
          (imenu-add-menubar-index)
          (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
          (turn-on-reftex)
          )))
(defun zilongshanren-programming/post-init-lsp-python-ms ()
  (progn
    ;; for executable of language server, if it's not symlinked on your PATH
    ;; (setq lsp-python-ms-executable
    ;;     (string-trim (shell-command-to-string
    ;;      "fd -a ^Microsoft.Python.LanguageServer$ $HOME/.vscode/extensions | tail -1")))
    ;; ;; for dev build of language server
    ;; (setq lsp-python-ms-dir
    ;;     (file-name-directory lsp-python-ms-executable))
    (setq lsp-python-ms-python-executable-cmd "/home/liuyan/.conda/envs/torch/bin/python")
    ))

(defun zilongshanren-programming/init-lsp-pyright ()
  (use-package lsp-pyright
    :ensure t
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp)))      ; or lsp-deferred
    :config
    (setq lsp-pyright-diagnostic-mode "workspace")
    (setq lsp-pyright-venv-path "/home/liuyan/.conda/envs/torch")
    ))

(defun zilongshanren-programming/init-company-tabnine ()
  (use-package company-tabnine
    :defer 1
    :custom
    (company-tabnine-max-num-results 9)
    ;; :bind
    ;; (("M-q" . company-other-backend)
    ;;  ("C-z t" . company-tabnine))
    :hook
    (lsp-after-open . (lambda ()
                        (setq company-tabnine-max-num-results 4)
                        (add-to-list 'company-transformers 'company//sort-by-tabnine t)
                        (add-to-list 'company-backends '(company-capf :with company-yasnippet :with company-tabnine :separate))
                        ))
    (kill-emacs . company-tabnine-kill-process)
    :config
    ;; Enable TabNine on default
    ;; (add-to-list 'company-backends #'company-tabnine)

    ;; Integrate company-tabnine with lsp-mode
    (defun company//sort-by-tabnine (candidates)
      (if (or (functionp company-backend)
              (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
          candidates
        (let ((candidates-table (make-hash-table :test #'equal))
              candidates-lsp
              candidates-tabnine)
          (dolist (candidate candidates)
            (if (eq (get-text-property 0 'company-backend candidate)
                    'company-tabnine)
                (unless (gethash candidate candidates-table)
                  (push candidate candidates-tabnine))
              (push candidate candidates-lsp)
              (puthash candidate t candidates-table)))
          (setq candidates-lsp (nreverse candidates-lsp))
          (setq candidates-tabnine (nreverse candidates-tabnine))
          (nconc (seq-take candidates-tabnine 4)
                 (seq-take candidates-lsp 20)))))))

(defun zilongshanren-programming/post-init-typescript-mode ()
  (add-hook 'typescript-mode-hook 'my-ts-mode-hook))

(defun zilongshanren-programming/post-init-lsp-mode ()
  (progn
    (setq lsp-file-watch-threshold 2000)
    (setq read-process-output-max (* 1024 1024 8)) ;; 8mb

    (setq lsp-ui-doc-enable nil)
    ;; (setq lsp-ui-doc-use-webkit t)
    ;; (setq lsp-eldoc-render-all t)

    (setq lsp-ui-imenu-enable nil)

    (setq lsp-ui-sideline-show-code-actions nil)
    (setq lsp-ui-sideline-show-hover t)

    (setq lsp-enable-folding nil)
    ;;handle yasnippet by myself
    (setq lsp-enable-snippet nil)

    ;; use ffip instead
    (setq lsp-enable-links nil)
    ;; auto restart lsp
    (setq lsp-restart 'auto-restart)
    (setq lsp-log-io nil)
    (setq lsp-enable-semantic-highlighting 1)
    (lsp-treemacs-sync-mode 1)

    (require 'dap-python)
    (setq dap-auto-show-output nil)

    (setq lsp-diagnostics-provider :flycheck)
    (setq lsp-modeline-diagnostics-enable nil)

    ;; support lsp-mode in org babel
    ;; :file "test.py"
    (defvar org-babel-lang-list
      '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "C++" "rust" "java" "sh"))
    (dolist (lang org-babel-lang-list)
      (eval `(lsp-org-babel-enable ,lang)))
    ))

(defun zilongshanren-programming/init-compile-dwim ()
  (use-package compile-dwim
    :commands (compile-dwim-run compile-dwim-compile)
    :init))


(defun zilongshanren-programming/post-init-robe ()
  (progn
    (add-hook 'inf-ruby-mode-hook 'spacemacs/toggle-auto-completion-on)
    (defun zilongshanren/ruby-send-current-line (&optional print)
      "Send the current line to the inferior Ruby process."
      (interactive "P")
      (ruby-send-region
       (line-beginning-position)
       (line-end-position))
      (when print (ruby-print-result)))

    (defun zilongshanren/ruby-send-current-line-and-go ()
      (interactive)
      (zilongshanren/ruby-send-current-line)
      (ruby-switch-to-inf t))

    (defun zilongshanren/start-inf-ruby-and-robe ()
      (interactive)
      (when (not (get-buffer "*ruby*"))
        (inf-ruby))
      (robe-start))

    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "sb" 'ruby-send-block
        "sB" 'ruby-send-buffer
        "sl" 'zilongshanren/ruby-send-current-line
        "sL" 'zilongshanren/ruby-send-current-line-and-go
        "sI" 'zilongshanren/start-inf-ruby-and-robe))))

(defun zilongshanren-programming/init-editorconfig ()
  (use-package editorconfig
    :init
    (progn
      (defun conditional-enable-editorconfig ()
        (if (and (zilongshanren/git-project-root)
                 (locate-dominating-file default-directory ".editorconfig"))
            (editorconfig-apply)))
      (add-hook 'prog-mode-hook 'conditional-enable-editorconfig))))

(defun zilongshanren-programming/post-init-cider ()
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

  (defun zilongshanren/cider-figwheel-repl ()
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
      (cider-repl-return)))

  (global-set-key (kbd "C-c C-f") #'zilongshanren/cider-figwheel-repl))

(defun zilongshanren-programming/post-init-graphviz-dot-mode ()
  (with-eval-after-load 'graphviz-dot-mode
      (require 'company-keywords)
      (push '(graphviz-dot-mode  "digraph" "node" "shape" "subgraph" "label" "edge" "bgcolor" "style" "record") company-keywords-alist)))

(defun zilongshanren-programming/post-init-dumb-jump ()
  (setq dumb-jump-selector 'ivy)
  (defun my-dumb-jump ()
    (interactive)
    (evil-set-jump)
    (dumb-jump-go))
  (global-set-key (kbd "C-s-g") 'my-dumb-jump))

(defun zilongshanren-programming/post-init-clojure-mode ()
  )

(defun zilongshanren-programming/post-init-emacs-lisp ()
    (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode))

(defun zilongshanren-programming/post-init-python ()
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; if you use pyton3, then you could comment the following line
  (setq python-shell-interpreter "ipython")
  (add-hook 'python-mode-hook #'lpy-mode)
  )

(defun zilongshanren-programming/post-init-js-doc ()
  (setq js-doc-mail-address "lyjdwh@gmail.com"
        js-doc-author (format "liuyan Qu <%s>" js-doc-mail-address)
        js-doc-url "http://www.zilongshanren.com"
        js-doc-license "MIT")

  )


(defun zilongshanren-programming/init-ctags-update ()
  (use-package ctags-update
    :init
    :defer t
    :config
    (spacemacs|hide-lighter ctags-auto-update-mode)))

;; nodejs-repl is much better now.
;; (defun zilongshanren-programming/init-js-comint ()
;;   (use-package js-comint
;;     :init
;;     (progn
;;       ;; http://stackoverflow.com/questions/13862471/using-node-js-with-js-comint-in-emacs
;;       (setq inferior-js-mode-hook
;;             (lambda ()
;;               ;; We like nice colors
;;               (ansi-color-for-comint-mode-on)
;;               ;; Deal with some prompt nonsense
;;               (add-to-list
;;                'comint-preoutput-filter-functions
;;                (lambda (output)
;;                  (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output)))))
;;       (setq inferior-js-program-command "node"))))

(defun zilongshanren-programming/post-init-web-mode ()
  (with-eval-after-load "web-mode"
    (web-mode-toggle-current-element-highlight)
    (web-mode-dom-errors-show))
  (setq company-backends-web-mode '((company-dabbrev-code
                                     company-keywords
                                     company-etags)
                                    company-files company-dabbrev)))



(defun zilongshanren-programming/post-init-yasnippet ()
  (progn
    (set-face-background 'secondary-selection "gray")

    (with-eval-after-load 'yasnippet
      (progn
        (define-key yas-keymap [(tab)]       (yas-filtered-definition 'yas-next-field))
        (define-key yas-keymap (kbd "TAB")   (yas-filtered-definition 'yas-next-field))))

    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                      org-mode-hook
                                                                      markdown-mode-hook))

    (spacemacs/add-to-hooks 'zilongshanren/load-yasnippet '(prog-mode-hook
                                                            markdown-mode-hook
                                                            org-mode-hook))
    (with-no-warnings
    (with-eval-after-load 'yasnippet
      (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq command 'prefix)
            (when-let ((prefix (funcall fun 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
                prefix))
          (progn
            (when (and (bound-and-true-p lsp-mode)
                       arg (not (get-text-property 0 'yas-annotation-patch arg)))
              (let* ((name (get-text-property 0 'yas-annotation arg))
                     (snip (format "%s (Snippet)" name))
                     (len (length arg)))
                (put-text-property 0 len 'yas-annotation snip arg)
                (put-text-property 0 len 'yas-annotation-patch t arg)))
            (funcall fun command arg))))
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)))
    ))

(defun zilongshanren-programming/post-init-racket-mode ()
  (progn
    (eval-after-load 'racket-repl-mode
      '(progn
         (define-key racket-repl-mode-map (kbd "]") nil)
         (define-key racket-repl-mode-map (kbd "[") nil)))

    (add-hook 'racket-mode-hook #'(lambda () (lispy-mode 1)))
    (add-hook 'racket-repl-mode-hook #'(lambda () (lispy-mode t)))
    ;; (add-hook 'racket-repl-mode-hook #'(lambda () (smartparens-mode t)))
    ))

(defun zilongshanren-programming/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire.meta\\'" . json-mode))
  (spacemacs/set-leader-keys-for-major-mode 'json-mode
    "ti" 'my-toggle-web-indent))



(defun zilongshanren-programming/init-flycheck-package ()
  (use-package flycheck-package))

(defun zilongshanren-programming/init-lispy ()
  (use-package lispy
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'ielm-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'inferior-emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      ;; (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
      )
    :config
    (progn
      (push '(cider-repl-mode . ("[`'~@]+" "#" "#\\?@?")) lispy-parens-preceding-syntax-alist)

      (spacemacs|hide-lighter lispy-mode)
      (define-key lispy-mode-map (kbd "M-s") 'lispy-splice)
      (define-key lispy-mode-map (kbd "s-k") 'paredit-splice-sexp-killing-backward)

      (with-eval-after-load 'cider-repl
        (define-key cider-repl-mode-map (kbd "C-s-j") 'cider-repl-newline-and-indent))

      (add-hook
       'minibuffer-setup-hook
       'conditionally-enable-lispy)
      (define-key lispy-mode-map (kbd "s-m") 'lispy-mark-symbol)
      (define-key lispy-mode-map (kbd "s-u") 'lispy-undo)
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))))

;; (defun zilongshanren-programming/init-google-c-style ()
;;   (use-package google-c-style
;;     :init (add-hook 'c-mode-common-hook 'google-set-c-style)))


(defun zilongshanren-programming/init-cmake-mode ()
  (use-package cmake-mode
      :defer t
      :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
      :config
      (spacemacs/declare-prefix-for-mode 'cmake-mode
          "mh" "docs")
      (spacemacs/set-leader-keys-for-major-mode 'cmake-mode
          "hd" 'cmake-help)
      (add-hook 'cmake-mode-hook (function cmake-rename-buffer))
      ))

(defun zilongshanren-programming/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      (setq flycheck-display-errors-delay 0.9)
      (setq flycheck-idle-change-delay 2.0)

      (flycheck-add-next-checker 'python-flake8 'python-mypy)
      )))

(defun zilongshanren-programming/post-init-eldoc ()
  (setq eldoc-idle-delay 0.4))


(defun zilongshanren-programming/post-init-js2-refactor ()
  (progn

(defun js2r-toggle-object-property-access-style ()
  "Toggle js object property access style."
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (save-excursion
     (let ((node (js2-node-at-point)))
       (if (js2-string-node-p node)
           (let* ((start (js2-node-abs-pos node))
                  (end (+ start (js2-node-len node))))
             (when (memq (char-before start) '(?\[))
               (save-excursion
                 (goto-char (-  end 1)) (delete-char 2)
                 (goto-char (+ start 1)) (delete-char -2) (insert "."))))
         (let* ((start (js2-node-abs-pos node))
                (end (+ start (js2-node-len node))))
           (when (memq (char-before start) '(?.))
             (save-excursion
               (goto-char end) (insert "\']")
               (goto-char start) (delete-char -1) (insert "[\'")))))))))

    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "r>" 'js2r-forward-slurp
      "r<" 'js2r-forward-barf
      "r." 'js2r-toggle-object-property-access-style
      "rep" 'js2r-expand-call-args)))

(defun zilongshanren-programming/post-init-js2-mode ()
  (progn

    (spacemacs|define-jump-handlers js2-mode)
    (add-hook 'spacemacs-jump-handlers-js2-mode 'etags-select-find-tag-at-point)

    (setq company-backends-js2-mode '((company-dabbrev-code :with company-keywords company-etags)
                                      company-files company-dabbrev))

    (setq company-backends-js-mode '((company-dabbrev-code :with company-keywords company-etags)
                                     company-files company-dabbrev))

    (add-hook 'js2-mode-hook 'my-js2-mode-hook)

    ;; add your own keywords highlight here
    (font-lock-add-keywords 'js2-mode
                            '(("\\<\\(cc\\)\\>" 1 font-lock-type-face)))

    (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "repl")

    (with-eval-after-load 'js2-mode
      (progn
        ;; these mode related variables must be in eval-after-load
        ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
        (setq-default js2-allow-rhino-new-expr-initializer nil)
        (setq-default js2-auto-indent-p nil)
        (setq-default js2-enter-indents-newline nil)
        (setq-default js2-global-externs '("module" "ccui" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
        (setq-default js2-idle-timer-delay 0.2)
        (setq-default js2-mirror-mode nil)
        (setq-default js2-strict-inconsistent-return-warning nil)
        (setq-default js2-include-rhino-externs nil)
        (setq-default js2-include-gears-externs nil)
        (setq-default js2-concat-multiline-strings 'eol)
        (setq-default js2-rebind-eol-bol-keys nil)
        (setq-default js2-auto-indent-p t)

        (setq-default js2-bounce-indent nil)
        (setq-default js-indent-level 4)
        (setq-default js2-basic-offset 4)
        (setq-default js-switch-indent-offset 4)
        ;; Let flycheck handle parse errors
        (setq-default js2-mode-show-parse-errors nil)
        (setq-default js2-mode-show-strict-warnings nil)
        (setq-default js2-highlight-external-variables t)
        (setq-default js2-strict-trailing-comma-warning nil)

        (add-hook 'web-mode-hook 'my-web-mode-indent-setup)

        (spacemacs/set-leader-keys-for-major-mode 'js2-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'js-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'web-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'css-mode
          "ti" 'my-toggle-web-indent)

        (spacemacs/declare-prefix-for-mode 'js2-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'js-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'web-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'css-mode "mt" "toggle")


        ))

    (evilified-state-evilify js2-error-buffer-mode js2-error-buffer-mode-map)

    ))

(defun zilongshanren-programming/post-init-css-mode ()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))

(defun zilongshanren-programming/post-init-tagedit ()
  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))

;; For each extension, define a function zilongshanren/init-<extension-name>
;;
(defun zilongshanren-programming/init-doxymacs ()
  "Initialize doxymacs"
  (use-package doxymacs
    :init
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    :config
    (progn
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (spacemacs|hide-lighter doxymacs-mode))))

;; https://atlanis.net/blog/posts/nodejs-repl-eval.html
(defun zilongshanren-programming/init-nodejs-repl-eval ()
  (use-package nodejs-repl-eval
    :commands (nodejs-repl-eval-buffer nodejs-repl-eval-dwim nodejs-repl-eval-function)
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode
                                         "ms" "REPL")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "sb" 'nodejs-repl-eval-buffer
        "sf" 'nodejs-repl-eval-function
        "sd" 'nodejs-repl-eval-dwim))
    :defer t
    ))

(defun zilongshanren-programming/post-init-lua-mode ()
  (progn
    (add-hook 'lua-mode-hook 'evil-matchit-mode)
    ;; (add-hook 'lua-mode-hook 'smartparens-mode)
    (setq lua-indent-level 2)

    ;; add lua language, basic, string and table keywords.
    ;; (with-eval-after-load 'lua-mode
    ;;   (require 'company-keywords)
    ;;   (push '(lua-mode  "setmetatable" "local" "function" "and" "break" "do" "else" "elseif" "self" "resume" "yield"
    ;;                     "end" "false" "for" "function" "goto" "if" "nil" "not" "or" "repeat" "return" "then" "true"
    ;;                     "until" "while" "__index" "dofile" "getmetatable" "ipairs" "pairs" "print" "rawget" "status"
    ;;                     "rawset" "select" "_G" "assert" "collectgarbage" "error" "pcall" "coroutine"
    ;;                     "rawequal" "require" "load" "tostring" "tonumber" "xpcall" "gmatch" "gsub"
    ;;                     "rep" "reverse" "sub" "upper" "concat" "pack" "insert" "remove" "unpack" "sort"
    ;;                     "lower") company-keywords-alist))

    ))

(defun zilongshanren-programming/post-init-cc-mode ()
  (progn
    (setq c-default-style "linux") ;; set style to "linux"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    (add-hook 'c++-mode-hook 'flycheck-mode)
    ))

(defun zilongshanren-programming/init-flycheck-clojure ()
  (use-package flycheck-clojure
    :defer t
    :init
    (eval-after-load 'flycheck '(flycheck-clojure-setup))))

(defun zilongshanren-programming/post-init-ycmd ()
  (progn
    (setq ycmd-tag-files 'auto)
    (setq ycmd-request-message-level -1)
    (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/Github/ycmd/ycmd/__main__.py")))
    (setq company-backends-c-mode-common '((company-c-headers
                                            company-dabbrev-code
                                            company-keywords
                                            company-gtags :with company-yasnippet)
                                           company-files company-dabbrev ))

    (zilongshanren|toggle-company-backends company-ycmd)
    (eval-after-load 'ycmd
      '(spacemacs|hide-lighter ycmd-mode))

    (spacemacs/set-leader-keys-for-major-mode 'c-mode
      "tb" 'zilong/company-toggle-company-ycmd)
    (spacemacs/set-leader-keys-for-major-mode 'c++-mode
      "tb" 'zilong/company-toggle-company-ycmd)))

;; when many project has the need to use tags, I will give etags-table and etags-update a try
(defun zilongshanren-programming/init-etags-select ()
  (use-package etags-select
    :init
    (progn
      (define-key evil-normal-state-map (kbd "gf")
        (lambda () (interactive) (find-tag (find-tag-default-as-regexp))))

      (define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)

      (define-key evil-normal-state-map (kbd "gn")
        (lambda () (interactive) (find-tag last-tag t)))

      (evilified-state-evilify etags-select-mode etags-select-mode-map)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "gd" 'etags-select-find-tag-at-point))))

(defun zilongshanren-programming/init-gulpjs ()
  (use-package gulpjs
    :init
    (progn
      (defun zilong/build-engine ()
        (interactive)
        (gulpjs-start-task-with-file-name "~/Github/fireball/app.js"))

      (spacemacs/set-leader-keys "ags" 'gulpjs-start-task)
      (spacemacs/set-leader-keys "agS" 'zilong/build-engine)
      (spacemacs/set-leader-keys "agr" 'gulpjs-restart-task))))


(defun zilongshanren-programming/init-paredit ()
  (use-package paredit
    :commands (paredit-wrap-round
               paredit-wrap-square
               paredit-wrap-curly
               paredit-splice-sexp-killing-backward)
    :init
    (progn

      (bind-key* "s-(" #'paredit-wrap-round)
      (bind-key* "s-[" #'paredit-wrap-square)
      (bind-key* "s-{" #'paredit-wrap-curly)
      )))

(defun zilongshanren-programming/post-init-company ()
  (progn
    (setq company-dabbrev-code-other-buffers 'all)
    ;; enable dabbrev-expand in company completion https://emacs-china.org/t/topic/6381
    (setq company-dabbrev-char-regexp "[\\.0-9a-z-_'/]")


    (setq company-minimum-prefix-length 1
          company-idle-delay 0.08)

    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-backends :modes shell-script-mode makefile-bsdmake-mode sh-mode lua-mode nxml-mode conf-unix-mode json-mode graphviz-dot-mode js2-mode js-mode)
      (spacemacs|add-company-backends :backends company-cmake :modes cmake-mode)
      )))
