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
