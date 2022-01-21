;; -*- coding: utf-8 -*-

(setq zilongshanren-programming-packages
      '(
        flycheck
        (compile-dwim :location local)
        json-mode
        yasnippet
        (cc-mode :location built-in)
        (python :location built-in)
        (emacs-lisp :location built-in)
        company
        (eldoc :location built-in)
        lsp-mode
        company-tabnine
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
        ;; prettier-js
        ))

(defun zilongshanren-programming/init-citre()
  (use-package citre
    :config
    (setq citre-auto-enable-citre-mode-modes '(prog-mode))
    (require 'citre-config)
  ))

(defun zilongshanren-programming/post-init-prettier-js ()
  (setq prettier-js-args '("--tab-width" "4")))

(defun zilongshanren-programming/init-mermaid-mode ()
  (use-package mermaid-mode
    :config
    (setq mermaid-mmdc-location "/home/liuyan/.yarn/bin/mmdc")
    (add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))
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
    :after company
    :config
    (company-tip-mode 1)
    (setq company-tip-delay 0.2)
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
  (progn
    (setq lsp-python-ms-python-executable-cmd "~/.conda/envs/torch/bin/python")
    ))

(defun zilongshanren-programming/post-init-lsp-pyright ()
  (progn
    ;; (add-hook 'python-mode-hook (lambda ()
    ;;                               (require 'lsp-pyright)
    ;;                               (lsp)))
    ;; (setq lsp-pyright-diagnostic-mode "workspace")
    (setq lsp-pyright-venv-path "/home/liuyan/.conda/envs/torch")
    (setq lsp-pyright-typechecking-mode "off")
    ))

(defun zilongshanren-programming/init-company-tabnine ()
  (use-package company-tabnine
    :defer 1
    :custom
    (company-tabnine-max-num-results 9)
    :bind
    (("M-q" . company-other-backend)
     ("C-c t" . company-tabnine))
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
      (setq-local company-tabnine-max-num-results 4)
      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
      (add-to-list 'company-backends '(company-capf :with company-tabnine :separate)))

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
    :hook
    (kill-emacs . company-tabnine-kill-process)
    :config
    (company-tabnine-toggle t)
  ))

(defun zilongshanren-programming/post-init-lsp-mode ()
  (progn
    (setq lsp-enable-file-watchers t)
    (setq lsp-file-watch-threshold 2000)
    (setq read-process-output-max (* 1024 1024 8)) ;; 8mb

    ;; lsp-ui
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-doc-position 'at-point)
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

    (setq lsp-enable-links t)

    ;; display parameters doc in overlay instead of minibuffer
    (setq lsp-signature-function 'lsp-signature-posframe)
    (setq lsp-signature-posframe-params '(:poshandler posframe-poshandler-point-bottom-left-corner-upward :width 60 :border-width 1 :min-width 60))

    (setq lsp-idle-delay 0.2)

    ;; auto restart lsp
    (setq lsp-restart 'auto-restart)
    (setq lsp-log-io nil)
    (setq lsp-semantic-tokens-enable nil)
    (lsp-treemacs-sync-mode 1)

    (require 'dap-python)
    (setq dap-auto-show-output nil)

    (setq lsp-diagnostics-provider :flycheck)

    (setq lsp-modeline-diagnostics-scope :project)

    (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]data\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]*logs*\\'")
    ))

(defun zilongshanren-programming/init-compile-dwim ()
  (use-package compile-dwim
    :commands (compile-dwim-run compile-dwim-compile)
    :init))

(defun zilongshanren-programming/post-init-emacs-lisp ()
    (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode))

(defun zilongshanren-programming/post-init-python ()
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; if you use pyton3, then you could comment the following line
  (setq python-shell-interpreter "ipython")
  )

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

(defun zilongshanren-programming/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire.meta\\'" . json-mode)))

(defun zilongshanren-programming/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      (setq flycheck-display-errors-delay 0.9)
      (setq flycheck-idle-change-delay 2.0)
      (setq flycheck-flake8-maximum-complexity 10)
      (setq flycheck-flake8-maximum-line-length 120)

      (flycheck-add-next-checker 'python-flake8 'python-pyright)
      (flycheck-remove-next-checker 'python-flake8 'python-pylint)
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

    (setq company-minimum-prefix-length 1
          company-idle-delay 0)
    ))
