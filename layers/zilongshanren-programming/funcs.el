;; -*- coding: utf-8; lexical-binding: t; -*-

(defun zilongshanren/run-current-file ()
  "Compile and/or Execute the current file."
  (interactive)
  ;; (call-interactively #'compile-dwim-compile)
  (call-interactively #'compile-dwim-run))

(defun zilongshanren/load-yasnippet ()
  (interactive)
  (unless yas-global-mode
    (progn
      (yas-global-mode 1)
      (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
      (setq yas-snippet-dirs '(my-snippet-dir))
      (yas-load-directory my-snippet-dir)
      (setq yas-wrap-around-region t)))
  (yas-minor-mode 1))

(defun cmake-rename-buffer ()
  "Renames a CMakeLists.txt buffer to cmake-<directory name>."
  (interactive)
  (when (and (buffer-file-name)
             (string-match "CMakeLists.txt" (buffer-name)))
    (setq parent-dir (file-name-nondirectory
                      (directory-file-name
                       (file-name-directory (buffer-file-name)))))
    (setq new-buffer-name (concat "cmake-" parent-dir))
    (rename-buffer new-buffer-name t)))

(defun python-set-checker (fn)
  (funcall fn)
  (setq flycheck-checker 'python-flake8))

(advice-add 'spacemacs//python-setup-backend :around 'python-set-checker)

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (user-error "LSP:: specify `:file' property to enable"))

           (setq buffer-file-name file-name)
           (and (fboundp 'lsp-deferred) (lsp-deferred))
           ))
       (put ',intern-pre 'function-documentation
            (format "Enable `%s' in the buffer of org source block (%s)."
                    'lsp-mode (upcase ,lang)))

       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))

(defun lsp-workon (name)
  "Change lsp virtual environment and activate it"
  (interactive
   (list
    (completing-read "Work on: " (pyvenv-virtualenv-list)
                     nil t nil 'pyvenv-workon-history nil nil)))
  (unless (member name (list "" nil pyvenv-virtual-env-name))
    (progn
      (setq lsp-python-ms-python-executable-cmd (format "%s/%s/%s" (pyvenv-workon-home) name "bin/python"))
      (call-interactively 'lsp-workspace-restart)

      (pyvenv-activate (format "%s/%s"
                               (pyvenv-workon-home)
                               name)) )))

(defun current-pyvenv-name ()
  "show the current pyvenv name"
  (interactive)
  (message pyvenv-virtual-env-name))
