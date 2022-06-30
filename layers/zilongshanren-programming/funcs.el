;; -*- coding: utf-8 -*-

(defun zilongshanren/run-current-file ()
  "Compile and/or Execute the current file."
  (interactive)
  ;; (call-interactively #'compile-dwim-compile)
  (call-interactively #'compile-dwim-run))

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

(defun lsp-workon (name)
  "Change lsp virtual environment and activate it"
  (interactive
   (list
    (completing-read "Work on: " (pyvenv-virtualenv-list)
                     nil t nil 'pyvenv-workon-history nil nil)))
  (unless (member name (list "" nil pyvenv-virtual-env-name))
    (progn
      (setq lsp-pyright-venv-path (format "%s/%s/%s" (pyvenv-workon-home) name "bin/python"))
      (call-interactively 'lsp-workspace-restart)

      (pyvenv-activate (format "%s/%s"
                               (pyvenv-workon-home)
                               name)) )))

(defun current-pyvenv-name ()
  "show the current pyvenv name"
  (interactive)
  (message pyvenv-virtual-env-name))

(defun company-dabbrev (command &optional arg &rest ignored)
  "dabbrev-like `company-mode' completion backend."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dabbrev))
    (prefix (company-dabbrev--prefix))
    (annotation
     (company-capf--annotation arg))
    (candidates
     (let* ((case-fold-search company-dabbrev-ignore-case)
            (words (company-dabbrev--search (company-dabbrev--make-regexp)
                                            company-dabbrev-time-limit
                                            (pcase company-dabbrev-other-buffers
                                              (`t (list major-mode))
                                              (`all `all))))
            (downcase-p (if (eq company-dabbrev-downcase 'case-replace)
                            case-replace
                          company-dabbrev-downcase)))
       (setq words (company-dabbrev--filter arg words))
       (if downcase-p
           (mapcar 'downcase words)
         words)))
    (kind 'text)
    (ignore-case company-dabbrev-ignore-case)
    (duplicates t)))

(defun my-company-disable-inline (fun command &optional arg &rest _ignore)
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
