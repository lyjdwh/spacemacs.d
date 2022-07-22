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

(defun my-company-disable-inline (fun command &optional arg &rest _ignore)
  "Enable yasnippet but disable it after '.' "
  (if (eq command 'prefix)
      (when-let ((prefix (funcall fun 'prefix)))
        (unless (memq (char-before (- (point) (length prefix)))
                      '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?` ?:))
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

(defun my-company-dabbrev-disable-inline (fun command &optional arg &rest _ignore)
  "Enable yasnippet but disable it after '.' "
  (if (eq command 'prefix)
      (when-let ((prefix (funcall fun 'prefix)))
        (unless (memq (char-before (- (point) (length prefix)))
                      '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?` ?:))
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

(defun company-transformer//capf-dabbrev (candidates)
  "Return different CANDIDATES depending on the point position.
  | point at | company-capf | company-dabbrev |
  |:---------|:------------:|:---------------:|
  | foo.|    |      ✅      |       ❌        |
  | \"foo|\" |      ❌      |       ✅        |
  | foo|     |      ✅      |       ✅        |
Inspired by @yyjjl's [company//sort-by-tabnine](https://emacs-china.org/t/tabnine/9988/40)
Last-Updated 2022-07-04 11:31:28 +0800"
  (if-let* ((trigger-chars (->> (lsp--server-capabilities)
                                (lsp:server-capabilities-completion-provider?)
                                (lsp:completion-options-trigger-characters?))))
      (let ((candidates-table (make-hash-table :test #'equal))
            (trigger-char-p
             (when (not (nth 3 (syntax-ppss)))
               (save-excursion
                 (goto-char (or (car (bounds-of-thing-at-point 'symbol)) (point)))
                 (and (lsp-completion--looking-back-trigger-characterp trigger-chars) t))))
            candidates-1
            candidates-2
            (print-count 0))
        (dolist (candidate candidates)
          (when (< print-count 10)
            (cl-incf print-count 1))
          ;; Candidates from capf
          (if (get-text-property 0 'lsp-completion-item candidate)
              (unless (gethash candidate candidates-table)
                (push (string-trim-left candidate) candidates-1))
            ;; Candidates from dabbrev
            (unless trigger-char-p
              (push candidate candidates-2)
              (puthash candidate t candidates-table))))
        (setq candidates-1 (nreverse candidates-1))
        (setq candidates-2 (nreverse candidates-2))
        (nconc candidates-1 (unless trigger-char-p candidates-2)))
    candidates))
