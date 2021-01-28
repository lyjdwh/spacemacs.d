;; -*- coding: utf-8; lexical-binding: t; -*-
(defconst zilongshanren-ui-packages
  '(
    ivy-posframe
    helm-posframe
    dashboard
    hide-mode-line
    doom-modeline
    ibuffer
    ibuffer-projectile
    snow
    ivy-rich
    all-the-icons-ivy-rich
    ))

(defun zilongshanren-ui/init-ivy-rich()
  (use-package all-the-icons-ivy-rich
    :after ivy-rich
    :config
    (all-the-icons-ivy-rich-mode 1)
    ))

(defun zilongshanren-ui/init-all-the-icons-ivy-rich()
  (use-package ivy-rich
    :after counsel
    :config
    (setq ivy-rich-parse-remote-buffer nil)
    (ivy-rich-mode)
    (setq ivy-virtual-abbreviate
          (or (and ivy-rich-mode 'abbreviate) 'name))
  ))

(defun zilongshanren-ui/init-snow()
  (use-package snow
    :commands snow
    ))

(defun zilongshanren-ui/post-init-ibuffer-projectile()
  (setq ibuffer-projectile-prefix
        (if (featurep 'all-the-icons)
            (concat (all-the-icons-octicon
                     "file-directory"
                     :face ibuffer-filter-group-name-face
                     :v-adjust -0.05)
                    " ")
          "Project: "))
  )

(defun zilongshanren-ui/post-init-ibuffer()
  (with-eval-after-load 'ibuffer
    (setq ibuffer-show-empty-filter-groups nil
          ibuffer-filter-group-name-face '(:inherit (success bold))
          ibuffer-formats
          `((mark modified read-only locked
                  ,@(if (featurep 'all-the-icons)
                        `(;; Here you may adjust by replacing :right with :center
                          ;; or :left According to taste, if you want the icon
                          ;; further from the name
                          " " (icon 2 2 :left :elide)
                          ,(propertize " " 'display `(space :align-to 8)))
                      '(" "))
                  (name 18 18 :left :elide)
                  " " (size 9 -1 :right)
                  " " (mode 16 16 :left :elide)
                  ,@(when (require 'ibuffer-vc nil t)
                      '(" " (vc-status 12 :left)))
                  " " filename-and-process)
            (mark " " (name 16 -1) " " filename)))

    ;; Display buffer icons on GUI
    (define-ibuffer-column icon (:name "  ")
      (let ((icon (if (and (buffer-file-name)
                           (all-the-icons-auto-mode-match?))
                      (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                    (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
          icon)))

    ;; Redefine size column to display human readable size
    (define-ibuffer-column size
      (:name "Size"
             :inline t
             :header-mouse-map ibuffer-size-header-map)
      (file-size-human-readable (buffer-size)))

    ))

(defun zilongshanren-ui/post-init-doom-modeline()
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-env-python-executable "python"
        doom-modeline-unicode-fallback t
        )
  )

(defun zilongshanren-ui/init-hide-mode-line()
  (use-package hide-mode-line
    :hook (((vterm-mode
             flycheck-error-list-mode) . hide-mode-line-mode))
  ))

(defun zilongshanren-ui/init-dashboard ()
  (use-package dashboard
    :ensure t
    :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    :preface
    (defun my/dashboard-init-info ()
      "Set a dashboard banner including information on package initialization
  time and garbage collections."""
    (setq dashboard-init-info
            (format "Emacs ready in %.2f seconds with %d garbage collections."
                    (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (let ((func (local-key-binding "r")))
        (and func (funcall func))))

    (defun dashboard-goto-projects ()
      "Go to projects."
      (interactive)
      (let ((func (local-key-binding "p")))
        (and func (funcall func))))

    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (> (length (window-list-1))
             ;; exclude `treemacs' window
             (if (and (fboundp 'treemacs-current-visibility)
                      (eq (treemacs-current-visibility) 'visible))
                 2
               1))
          (setq dashboard-recover-layout-p t))

      (delete-other-windows)

      ;; Refresh dashboard buffer
      (when (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)

      ;; Jump to the first section
      (dashboard-goto-recent-files)
      (evil-force-evilified-state))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (when (and dashboard-recover-layout-p
                 (bound-and-true-p winner-mode))
        (winner-undo)
        (setq dashboard-recover-layout-p nil)))
    :bind (("<f2>" . open-dashboard)
           :map dashboard-mode-map
           ("q" . quit-dashboard)
           ("o" . link-hint-open-link)
           )
    :init
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    ;; Set the title
    (setq dashboard-banner-logo-title "Welcome to Emacs, Happy Hacking!")
    (setq dashboard-startup-banner "~/.spacemacs.d/resources/amadeus.png")
    ;; Content is not centered by default. To center, set
    (setq dashboard-center-content t)
    ;; To disable shortcut "jump" indicators for each section, set
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-items '((recents  . 7)
                            (projects . 5)))
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-heading-icons
          '((recents   . "file-text")
            (bookmarks . "bookmark")
            (agenda    . "calendar")
            (projects  . "briefcase")
            (registers . "database"))
          )

    (dashboard-setup-startup-hook)
    (add-hook 'after-init-hook 'open-dashboard)
    (add-hook 'after-init-hook 'my/dashboard-init-info)
    ))

(defun zilongshanren-ui/init-ivy-posframe ()
  (use-package ivy-posframe
    :diminish
    :commands ivy-posframe-mode
    :config
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
    ))

(defun zilongshanren-ui/init-helm-posframe ()
  (use-package helm-posframe
    :diminish
    :commands helm-posframe-enable helm-posframe-disable
    :config
    (setq helm-posframe-poshandler 'posframe-poshandler-frame-center)
    ))
