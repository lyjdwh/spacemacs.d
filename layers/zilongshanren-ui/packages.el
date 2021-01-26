;;; packages.el --- zilong-ui layer packages file for Spacemacs.
;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: liuyan <lyjdwh@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst zilongshanren-ui-packages
  '(
    diminish
    popwin
    (whitespace :location built-in)
    ivy-posframe
    helm-posframe
    dashboard
    hide-mode-line
    doom-modeline
    ibuffer
    ibuffer-projectile
    ;; hl-anything performance is very slow...
    ;; hl-anything
    ;; if you wnat to use spaceline, please comment out zilong-mode-line
    ;; spaceline
    ;; beacon
    ;; evil-vimish-fold
    ;; company-box
    )
  )

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

(defun zilongshanren-ui/post-init-diminish ()
  (progn
    (with-eval-after-load 'whitespace
      (diminish 'whitespace-mode))
    (with-eval-after-load 'smartparens
      (diminish 'smartparens-mode))
    (with-eval-after-load 'which-key
      (diminish 'which-key-mode))
    (with-eval-after-load 'hungry-delete
      (diminish 'hungry-delete-mode))))


(defun zilongshanren-ui/post-init-spaceline ()
  (use-package spaceline-config
    :config
    (progn
      (defvar spaceline-org-clock-format-function
        'org-clock-get-clock-string
        "The function called by the `org-clock' segment to determine what to show.")

      (spaceline-define-segment org-clock
                                "Show information about the current org clock task.  Configure
`spaceline-org-clock-format-function' to configure. Requires a currently running
org clock.

This segment overrides the modeline functionality of `org-mode-line-string'."
                                (when (and (fboundp 'org-clocking-p)
                                           (org-clocking-p))
                                  (substring-no-properties (funcall spaceline-org-clock-format-function)))
                                :global-override org-mode-line-string)

      (spaceline-compile
       'zilong
       ;; Left side of the mode line (all the important stuff)
       '(((persp-name
           workspace-number
           window-number
           )
          :separator "|"
          :face highlight-face)
         ((buffer-modified buffer-size input-method))
         anzu
         '(buffer-id remote-host buffer-encoding-abbrev)
         ((point-position line-column buffer-position selection-info)
          :separator " | ")
         major-mode
         process
         (flycheck-error flycheck-warning flycheck-info)
         ;; (python-pyvenv :fallback python-pyenv)
         ((minor-modes :separator spaceline-minor-modes-separator) :when active)
         (org-pomodoro :when active)
         (org-clock :when active)
         nyan-cat)
       ;; Right segment (the unimportant stuff)
       '((version-control :when active)
         battery))

      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-zilong))))
      )))

;; (defun zilongshanren-ui/init-company-box ()
;;   (use-package company-box
;;     :diminish
;;     :config
;;     (progn
;;       ;; 使用company-box来写org的话，补全选项会更好看一些
;;       (add-hook 'org-mode-hook 'company-box-mode)
;;       (setq company-box-enable-icon nil)
;;       (setq company-box-backends-colors nil)
;;       (setq company-box-show-single-candidate t)
;;       (setq company-box-max-candidates 50))))


(defun zilongshanren-ui/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "otb")

      (spacemacs/toggle-beacon-on))
    :config (spacemacs|hide-lighter beacon-mode)))

(defun zilongshanren-ui/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :init
    (vimish-fold-global-mode 1)
    :config
    (progn
      (define-key evil-normal-state-map (kbd "zf") 'vimish-fold)
      (define-key evil-visual-state-map (kbd "zf") 'vimish-fold)
      (define-key evil-normal-state-map (kbd "zd") 'vimish-fold-delete)
      (define-key evil-normal-state-map (kbd "za") 'vimish-fold-toggle))))

(defun zilongshanren-ui/post-init-hl-anything ()
  (progn
    (defun my-inhibit-globalized-hl-highlight-mode ()
      "Counter-act a globalized hl-highlight-mode."
      (set (make-local-variable 'hl-highlight-mode) nil))

    (add-hook 'org-agenda-mode-hook 'my-inhibit-globalized-hl-highlight-mode)
    (hl-highlight-mode -1)
    (spacemacs|add-toggle toggle-hl-anything
      :status hl-highlight-mode
      :on (hl-highlight-mode)
      :off (hl-highlight-mode -1)
      :documentation "Toggle highlight anything mode."
      :evil-leader "ths")))

(defun zilongshanren-ui/post-init-pangu-spacing ()
  (progn
    ;; add toggle options
    (spacemacs|add-toggle toggle-pangu-spaceing
      :status pangu-spacing-mode
      :on (global-pangu-spacing-mode)
      :off (global-pangu-spacing-mode -1)
      :documentation "Toggle pangu spacing mode"
      :evil-leader "ots")
    (add-hook 'markdown-mode-hook
              #'(lambda ()
                 (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))))

(defun zilongshanren-ui/post-init-popwin ()
  (progn
    ;FIXME:
    ;; (push "*zilongshanren/run-current-file output*" popwin:special-display-config)
    (delete "*Async Shell Command*" popwin:special-display-config)))

(defun zilongshanren-ui/post-init-whitespace ()
  (progn
    ;; ;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
    (setq whitespace-line-column fill-column) ;; limit line length
    ;;https://www.reddit.com/r/emacs/comments/2keh6u/show_tabs_and_trailing_whitespaces_only/
    (setq whitespace-display-mappings
          ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
          '(
            (space-mark 32 [183] [46])           ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
            (newline-mark 10 [182 10])           ; 10 LINE FEED
            (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
            ))
    (setq whitespace-style '(face tabs trailing tab-mark ))
    ;; (setq whitespace-style '(face lines-tail))
    ;; show tab;  use untabify to convert tab to whitespace
    (setq spacemacs-show-trailing-whitespace t)

    (setq-default tab-width 4)
    ;; set-buffer-file-coding-system -> utf8 to convert dos to utf8
    ;; (setq inhibit-eol-conversion t)
    ;; (add-hook 'prog-mode-hook 'whitespace-mode)

    ;; (global-whitespace-mode +1)

    (with-eval-after-load 'whitespace
      (progn
        (set-face-attribute 'whitespace-tab nil
                            :background "#Adff2f"
                            :foreground "#00a8a8"
                            :weight 'bold)
        (set-face-attribute 'whitespace-trailing nil
                            :background "#e4eeff"
                            :foreground "#183bc8"
                            :weight 'normal)))

    (diminish 'whitespace-mode)))
