;; -*- coding: utf-8 -*-

(setq zilongshanren-programming-packages
      '(
        flycheck
        (compile-dwim :location local)
        yasnippet
        (cc-mode :location built-in)
        (python :location built-in)
        company
        (eldoc :location built-in)
        lsp-mode
        ;; company-tabnine
        (copilot :location (recipe
                            :fetcher github
                            :repo "zerolfx/copilot.el"
                            :files ("*.el" "dist")))
        lsp-python-ms
        lsp-pyright
        auctex
        highlight-indent-guides
        (color-rg :location (recipe :fetcher github :repo "manateelazycat/color-rg"))
        hl-todo
        counsel-dash
        lpy
        taskrunner
        (ivy-taskrunner :location (recipe :fetcher github :repo "emacs-taskrunner/ivy-taskrunner"))
        counsel-etags
        (company-tip :location local)
        vimrc-mode
        mermaid-mode
        ob-mermaid
        citre
        blacken
        lsp-ltex
        ))

(defun zilongshanren-programming/init-lsp-ltex()
  (use-package lsp-ltex
    :ensure t
    ;; :hook (text-mode . (lambda ()
    ;;                      (require 'lsp-ltex)
    ;;                      (setq-local lsp-diagnostics-provider :flycheck)
    ;;                      (lsp))) ; or lsp-deferred
    :config
    (defun enable-lang-check()
      (interactive)
      (setq-local lsp-diagnostics-provider :flycheck)
      (lsp))
    (spacemacs/set-leader-keys "eo" 'enable-lang-check)
    ))

(defun zilongshanren-programming/post-init-blacken ()
  (setq blacken-skip-string-normalization t))

(defun zilongshanren-programming/init-citre()
  (use-package citre
    :config
    (setq citre-auto-enable-citre-mode-modes nil)
    (require 'citre-config)
  ))

(defun zilongshanren-programming/init-mermaid-mode ()
  (use-package mermaid-mode
    :mode "\\.mmd\\'"
    ))

(defun zilongshanren-programming/init-ob-mermaid ()
  (use-package ob-mermaid
    :after org
    :config
    (setq ob-mermaid-cli-path "/home/liuyan/.yarn/bin/mmdc")
    (add-to-list 'org-babel-load-languages '(mermaid . t))
    ))

(defun zilongshanren-programming/init-vimrc-mode ()
  (use-package vimrc-mode
    :defer t
    :config
    (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
    ))

(defun zilongshanren-programming/init-company-tip ()
  (use-package company-tip
    :if (not (display-graphic-p))
    :after company
    :bind (:map company-active-map
                ("C-h" . company-tip--manual-begin))
    :config
    (company-tip-mode 1)
    (setq company-tip-delay nil)
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
    :commands lpy-mode
    :config
    ;; fix key conflict between lpy and lsp
    (define-key lpy-mode-map "(" nil)
    ))

(defun zilongshanren-programming/post-init-counsel-dash ()
  (progn
    (add-hook 'sh-mode-hook (lambda () (setq-local counsel-dash-docsets '("Bash"))))
    (add-hook 'LaTeX-mode-hook (lambda () (setq-local counsel-dash-docsets '("LaTeX"))))
    (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
    (add-hook 'c++-mode-hook (lambda () (setq-local counsel-dash-docsets '("C++"))))
    (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python3" "PyTorch" "PytorchLightning" "Pytorch-Geometric" "NumPy" "Matplotlib" "Pandas" "scikit-learn" "torchvision" "torchtext"))))
    (setq counsel-dash-enable-debugging nil)
    (setq dash-docs-enable-debugging nil)
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
    :hook (prog-mode . highlight-indent-guides-mode)
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
                TeX-view-program-selection '((output-pdf "Okular"))
                ;; TeX-view-program-selection '((output-pdf "Emacs"))
                ;; TeX-view-program-list '(("Emacs" "emacsclient -n %o"))
                ;; TeX-view-program-selection '((output-pdf "PDF Tools"))
                ;; TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                TeX-source-correlate-mode t
                TeX-source-correlate-start-server t
                TeX-source-correlate-method 'synctex
                TeX-PDF-mode t
                reftex-plug-into-AUCTeX t
                Tex-save-query nil
                TeX-view-program-list
                '(("Okular" "okular --unique %o#src:%n`pwd`/./%b")
                  ("eaf" eaf-pdf-synctex-forward-view)
                  ("Skim" "displayline -b -g %n %o %b")
                  ("Zathura"
                   ("zathura %o"
                    (mode-io-correlate
                     " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\""))))
                )
          (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))

          (imenu-add-menubar-index)
          (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
          (turn-on-reftex)
          (TeX-toggle-debug-warnings)
          )))
(defun zilongshanren-programming/post-init-lsp-python-ms ()
  (with-eval-after-load 'lsp-python-ms
    (setq lsp-python-ms-python-executable-cmd "~/.conda/envs/torch/bin/python")
    ))

(defun zilongshanren-programming/post-init-lsp-pyright ()
  (with-eval-after-load 'lsp-pyright
    ;; (add-hook 'python-mode-hook (lambda ()
    ;;                               (require 'lsp-pyright)
    ;;                               (lsp)))
    ;; (setq lsp-pyright-diagnostic-mode "workspace")
    (setq lsp-pyright-venv-path "/home/liuyan/.conda/envs/torch")
    (setq lsp-pyright-typechecking-mode "basic") ;; off
    ))

(defun zilongshanren-programming/init-company-tabnine ()
  (use-package company-tabnine
    :defer 1
    :bind
    (("M-q" . company-other-backend)
     ("C-c t" . company-tabnine)
     ("C-t" . company-tabnine))
    :init
    (defun company//sort-by-tabnine (candidates)
      "Integrate company-tabnine with lsp-mode"
      (if (or (functionp company-backend)
              (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
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
                 candidates-lsp
                 ))))

    (defun lsp-after-open-tabnine ()
      "Hook to attach to `lsp-after-open'."
      ;; (setq-local company-tabnine-max-num-results 4)
      ;; (add-to-list 'company-transformers 'company//sort-by-tabnine t)
      ;; (add-to-list 'company-backends '(company-capf :with company-tabnine :separate)
      (add-to-list 'company-backends '(company-tabnine :separate company-capf company-yasnippet))
      ;; (add-to-list 'company-backends '(company-capf company-tabnine :with company-yasnippet))
      )

    (defun company-tabnine-toggle (&optional enable)
      "Enable/Disable TabNine. If ENABLE is non-nil, definitely enable it."
      (interactive)
      (if (or enable (not (memq 'company-tabnine company-backends)))
          (progn
            (add-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
            (add-to-list 'company-backends #'company-tabnine)
            (when (bound-and-true-p lsp-mode) (lsp-after-open-tabnine))
            (message "TabNine enabled."))
        (setq company-backends (delete 'company-tabnine company-backends))
        (setq company-backends (delete '(company-capf :with company-tabnine :separate) company-backends))
        (remove-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
        (company-tabnine-kill-process)
        (message "TabNine disabled.")))

    (spacemacs/set-leader-keys "ott" 'company-tabnine-toggle)
    :hook
    (kill-emacs . company-tabnine-kill-process)
    :config
    (setq company-tabnine-max-num-results 4)
    (setq company-tabnine-executable-args (list "--no-lsp=true"))
    ;; (company-tabnine-toggle t)
  ))

(defun zilongshanren-programming/init-copilot()
  (use-package copilot
    :init
    ;; accept completion from copilot and fallback to company
    (defun my-tab ()
      (interactive)
      (or (copilot-accept-completion)
          (company-indent-or-complete-common nil)))

    :hook (prog-mode . copilot-mode)
    :bind (:map evil-insert-state-map
           ("s-<tab>" . 'copilot-accept-completion)
           ("C-l" . 'copilot-accept-completion-by-word)
           ("C-S-l" . 'copilot-accept-completion-by-line)
           ("C-," . 'copilot-next-completion)
           ("C-." . 'copilot-previous-completion)
           :map company-active-map
           ("<tab>" . 'my-tab)
           ("TAB" . 'my-tab)
           ("C-l" . 'copilot-accept-completion-by-word)
           ("C-S-l" . 'copilot-accept-completion-by-line))
    ))

(defun zilongshanren-programming/post-init-lsp-mode ()
  (with-eval-after-load 'lsp-mode
    (setq lsp-enable-file-watchers t)
    (setq lsp-file-watch-threshold 1000)
    (setq read-process-output-max (* 1024 1024)) ;; 1mb

    ;; lsp-ui
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-doc-position 'at-point)
    (setq lsp-ui-doc-delay 0)
    (setq lsp-ui-imenu-enable nil)
    (setq lsp-ui-sideline-show-code-actions nil)
    (setq lsp-ui-sideline-show-hover nil)
    (setq lsp-ui-sideline-update-mode 'line)

    ;; lens
    (setq lsp-lens-enable nil)

    ;; workspace
    (setq lsp-keep-workspace-alive nil)
    (setq lsp-modeline-workspace-status-enable t)

    ;; Disable features that have great potential to be slow.
    (setq lsp-enable-folding nil)
    (setq lsp-enable-text-document-color nil)

    ;; Reduce unexpected modifications to code
    (setq lsp-enable-on-type-formatting nil)
    (setq lsp-enable-indentation nil)

    (setq lsp-headerline-breadcrumb-enable t)

    ;;handle yasnippet by myself
    (setq lsp-enable-snippet nil)

    (setq lsp-enable-links nil)

    ;; display parameters doc in overlay instead of minibuffer
    (if (display-graphic-p)
        (progn
          (setq lsp-signature-function 'lsp-signature-posframe)
          (setq lsp-signature-posframe-params '(:poshandler posframe-poshandler-point-bottom-left-corner-upward
                                                            :width 60 :border-width 1 :min-width 60 :max-height 12))
          ))

    (setq lsp-idle-delay 0.6)
    (setq lsp-completion-provider :capf)

    ;; auto restart lsp
    (setq lsp-restart 'auto-restart)
    (setq lsp-log-io nil)
    (setq lsp-semantic-tokens-enable nil)
    (lsp-treemacs-sync-mode 1)

    (require 'dap-python)
    (setq dap-auto-show-output nil)

    (setq lsp-diagnostics-provider :auto)
    (setq lsp-modeline-diagnostics-enable nil)

    (setq lsp-modeline-diagnostics-scope :workspace)

    (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]data\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]*logs*\\'")

    ;; doc
    ;; lsp-describe-thing-at-point
    (evil-define-key 'normal 'lsp-mode (kbd "gh") 'lsp-ui-doc-glance)

    (advice-add 'lsp-completion--enable :after (lambda (&rest _args) (setq company-backends '((company-capf :with company-yasnippet)))))
    ))

(defun zilongshanren-programming/init-compile-dwim ()
  (use-package compile-dwim
    :commands (compile-dwim-run compile-dwim-compile)
    :init))

(defun zilongshanren-programming/post-init-python ()
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'python-mode-hook #'flycheck-mode)
  (setq flycheck-flake8rc '(".flake8" ".flake8rc" "pyproject.toml" "setup.cfg"))
  (setq python-shell-interpreter "ipython")
  )

(defun zilongshanren-programming/post-init-yasnippet ()
  (with-no-warnings
    (with-eval-after-load 'yasnippet
      (define-key yas-keymap [(tab)]       (yas-filtered-definition 'yas-next-field))
      (define-key yas-keymap (kbd "TAB")   (yas-filtered-definition 'yas-next-field))

      (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it after '.' "
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
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline))
    ))

(defun zilongshanren-programming/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      (setq flycheck-display-errors-delay 0.9)
      (setq flycheck-idle-change-delay 2.0)
      (setq flycheck-flake8-maximum-complexity 18)
      (setq flycheck-flake8-maximum-line-length 120)

      ;; Remove newline checks, since they would trigger an immediate check
      ;; when we want the idle-change-delay to be in effect while editing.
      (setq flycheck-check-syntax-automatically '(idle-change
                                                  mode-enabled))

      (flycheck-add-next-checker 'python-flake8 'python-pyright)
      (flycheck-remove-next-checker 'python-flake8 'python-pylint)
      (flycheck-remove-next-checker 'python-flake8 'python-mypy)
      )))

(defun zilongshanren-programming/post-init-eldoc ()
  (setq eldoc-idle-delay 0.4))

(defun zilongshanren-programming/post-init-cc-mode ()
  (progn
    (setq c-default-style "linux") ;; set style to "linux"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    (add-hook 'c++-mode-hook 'flycheck-mode)
    ))

(defun zilongshanren-programming/post-init-company ()
  (progn
    (setq company-dabbrev-code-other-buffers 'all)
    ;; enable dabbrev-expand in company completion https://emacs-china.org/t/topic/6381
    (setq company-dabbrev-char-regexp "[\\.0-9a-z-_'/]")

    (setq company-minimum-prefix-length 2
          company-idle-delay 0)

    (with-eval-after-load 'company
      (define-key company-active-map (kbd "C-n") '(lambda () (interactive) (company-select-next-or-abort 4)))
      (define-key company-active-map (kbd "C-p") '(lambda () (interactive) (company-select-previous-or-abort 4)))
      )
    ))
