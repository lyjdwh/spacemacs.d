;; -*- coding: utf-8 -*-

(defconst zilongshanren-better-defaults-packages
  '(
    (dired-mode :location built-in)
    (profiler :location built-in)
    (recentf :location built-in)
    (eaf :location local)
    (snails :location local)
    (company-english-helper :location (recipe :fetcher github :repo "manateelazycat/company-english-helper"))
    (insert-translated-name :location (recipe :fetcher github :repo "manateelazycat/insert-translated-name"))
    (rotate-text :location local)
    super-save
    company-box
    (move-to-position-hint :location local)
    ))

(defun zilongshanren-better-defaults/init-move-to-position-hint ()
  (use-package move-to-position-hint))

(defun zilongshanren-better-defaults/post-init-company-box ()
  (setq company-box-doc-delay 0.2)
)

(defun zilongshanren-better-defaults/init-super-save ()
  (use-package super-save
    :diminish
    :custom
    (super-save-auto-save-when-idle t)
    (auto-save-default nil)
    (make-backup-files nil)
    :config
    (super-save-mode 1)
    (setq super-save-idle-duration 60)
    ))

(defun zilongshanren-better-defaults/init-rotate-text ()
  (progn
    (autoload 'rotate-text "rotate-text" nil t)
    (autoload 'rotate-text-backward "rotate-text" nil t)
    ))

(defun zilongshanren-better-defaults/init-company-english-helper ()
  (use-package company-english-helper
    :commands (toggle-company-english-helper)))

(defun zilongshanren-better-defaults/init-insert-translated-name ()
  (use-package insert-translated-name
    :commands insert-translated-name-insert insert-translated-region-replace insert-translated-name-insert-original-translation
    :config
    (defun insert-translated-region-replace ()
      "translate Chinese to English and replace it "
      (interactive)
      (insert-translated-name-replace-symbol "comment"))
    ))

(defun zilongshanren-better-defaults/init-snails ()
  (use-package snails
    :load-path "/home/liuyan/bin/snails"
    :commands (snails snails-search-point)
    :if (display-graphic-p)
    :custom-face
    (snails-content-buffer-face ((t (:background "#111" :height 130))))
    (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 130))))
    (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.2))))
    :config
    (add-to-list 'load-path "/home/liuyan/bin/fuz.el/")
    (setq snails-default-backends '(snails-backend-eaf-browser-search snails-backend-eaf-github-search snails-backend-google-suggestion))
    (setq snails-prefix-backends
          '((":" '(snails-backend-search-pdf))
            (";" '(snails-backend-eaf-pdf-table))
            ))
    (setq snails-show-with-frame nil)
    (define-key snails-mode-map (kbd "C-j") 'snails-select-next-item)
    (define-key snails-mode-map (kbd "C-k") 'snails-select-prev-item)
    (define-key snails-mode-map (kbd "C-h") 'snails-select-prev-backend)
    (define-key snails-mode-map (kbd "C-l") 'snails-select-next-backend)
    (define-key snails-mode-map (kbd "C-o") 'snails-candiate-alternate-do)
    ))

(defun zilongshanren-better-defaults/init-eaf ()
  (use-package eaf
    :load-path "/home/liuyan/bin/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
    :commands (open-file-with-eaf eaf-open-mail-as-html
               eaf-open-ipython eaf-open-camera eaf-open-demo
               eaf-open-browser eaf-open-external eaf-open-terminal eaf-toggle-fullscreen
               eaf-open eaf-open-url eaf-open-office eaf-open-mindmap eaf-open-airshare
               eaf-open-bookmark eaf-open-rss-reader eaf-kill-process eaf-search-it
               eaf-file-browser-qrcode eaf-interleave-sync-current-note eaf-interleave-sync-next-note
               eaf-interleave-sync-previous-note eaf-interleave-add-note eaf-interleave-open-notes-file eaf-interleave-quit)

    :diminish eaf-mode
    :init
    (spacemacs/declare-prefix "ae" "eaf")
    ;; set eaf as default browse
    (setq browse-url-browser-function 'eaf-open-browser)
    (defalias 'browse-web #'eaf-open-browser)
    :custom
    (eaf-python-command "/usr/bin/python3")
    (eaf-evil-leader-keymap  spacemacs-cmds)
    (eaf-evil-leader-key "SPC")
    :config
    (require 'eaf-evil)
    (require 'eaf-org)
    (setq eaf-buffer-title-format "EAF/%s")
    (setq eaf-grip-token "d95425cda9aa8c58779a312be6fe4662b965a441")
    ;; set proxy
    (setq eaf-proxy-type "socks5")
    (setq eaf-proxy-host "127.0.0.1")
    (setq eaf-proxy-port "1080")
    (eaf-setq eaf-browser-aria2-proxy-host "127.0.0.1")
    (eaf-setq eaf-browser-aria2-proxy-port "12333")
    ;; set dark mode
    (eaf-setq eaf-browser-dark-mode "true")
    (eaf-setq eaf-pdf-dark-mode "false")
    (eaf-setq eaf-mindmap-dark-mode "false")
    ;; web
    ;; (eaf-setq eaf-browser-default-zoom "1.25")
    (eaf-setq eaf-browser-enable-adblocker "true")
    ;; camera
    (eaf-setq eaf-camera-save-path "~/Pictures")
    ;; interleave
    (setq eaf-interleave-org-notes-dir-list '("~/org-notes/notes/"))
    (setq eaf-interleave-split-direction 'vertical)
    (setq eaf-interleave-disable-narrowing t)
    (setq eaf-interleave-split-lines 20)
    (add-hook 'eaf-pdf-viewer-hook 'eaf-interleave-app-mode)
    (add-hook 'eaf-browser-hook 'eaf-interleave-app-mode)
    (add-hook 'org-mode-hook 'eaf-interleave-mode)
    ;; key customize
    (add-to-list 'eaf-terminal-keybinding '("C-'" . "eaf-send-ctrl-esc-sequence"))
    (add-to-list 'eaf-terminal-keybinding '("C-j" . "eaf-send-key-sequence"))
    (add-to-list 'eaf-pdf-viewer-keybinding '("d" . "eaf-proxy-scroll_up_page"))
    (add-to-list 'eaf-pdf-viewer-keybinding '("u" . "eaf-proxy-scroll_down_page"))
    (add-to-list 'eaf-pdf-viewer-keybinding '("aa" . "eaf-proxy-add_annot_highlight"))
    (add-to-list 'eaf-pdf-viewer-keybinding '("au" . "eaf-proxy-add_annot_underline"))
    (add-to-list 'eaf-pdf-viewer-keybinding '("as" . "eaf-proxy-add_annot_squiggly"))
    (add-to-list 'eaf-pdf-viewer-keybinding '("ae" . "eaf-proxy-add_annot_text_or_edit_annot"))
    (add-to-list 'eaf-pdf-viewer-keybinding '("ad" . "eaf-proxy-add_annot_strikeout_or_delete_annot"))
    (add-to-list 'eaf-pdf-viewer-keybinding '("C-c" . "copy_select"))
    (add-to-list 'eaf-browser-keybinding '("C-c" . "kill_text"))

    (add-to-list 'eaf-app-display-function-alist '("browser" . eaf--browser-display))

    (defun eaf-open-this (file)
      "Open html/pdf/image/video files whenever possible with EAF.
    Other files will open normally with `dired-find-file' or `dired-find-alternate-file'"
      (cond
       ((member (eaf-get-file-name-extension file) eaf-office-extension-list)
        (eaf-open-office file))
       ((eaf--get-app-for-extension
         (eaf-get-file-name-extension file))
        (eaf-open file))
       (t (dired-find-file))))

    (defun open-file-with-eaf ()
      "Open current file in eaf."
      (interactive)
      (let ((file-path (if (derived-mode-p 'dired-mode)
                           (dired-get-file-for-visit)
                         buffer-file-name)))
        (if file-path
            (eaf-open-this file-path)
          (message "No file associated to this buffer."))))
    ))

(defun zilongshanren-better-defaults/post-init-recentf ()
  (progn
    (setq recentf-exclude
          '("COMMIT_MSG"
            "COMMIT_EDITMSG"
            "github.*txt$"
            "/tmp/"
            "/ssh:"
            "/sudo:"
            "/TAGS$"
            "/GTAGS$"
            "/GRAGS$"
            "/GPATH$"
            "\\.mkv$"
            "\\.mp[34]$"
            "\\.avi$"
            "\\.pdf$"
            "\\.sub$"
            "\\.srt$"
            "\\.ass$"
            ".*png$"))
    (setq recentf-max-saved-items 2048)))

(defun zilongshanren-better-defaults/init-dired-mode ()
  (use-package dired-mode
    :defer t
    :init
    (progn
      (require 'dired-x)
      (require 'dired-aux)
      (add-hook 'dired-mode-hook 'dired-async-mode)
      (setq dired-listing-switches "-alh")
      (setq dired-guess-shell-alist-user
            '(("\\.pdf\\'" "open")
              ("\\.docx\\'" "open")
              ("\\.\\(?:djvu\\|eps\\)\\'" "open")
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
              ("\\.\\(?:xcf\\)\\'" "open")
              ("\\.csv\\'" "open")
              ("\\.tex\\'" "open")
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
               "open")
              ("\\.\\(?:mp3\\|flac\\)\\'" "open")
              ("\\.html?\\'" "open")
              ("\\.md\\'" "open")))

      (setq dired-omit-files
            (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))

      ;; always delete and copy recursively
      (setq dired-recursive-deletes 'always)
      (setq dired-recursive-copies 'always)

      (defun ora-ediff-files ()
        (interactive)
        (let ((files (dired-get-marked-files))
              (wnd (current-window-configuration)))
          (if (<= (length files) 2)
              (let ((file1 (car files))
                    (file2 (if (cdr files)
                               (cadr files)
                             (read-file-name
                              "file: "
                              (dired-dwim-target-directory)))))
                (if (file-newer-than-file-p file1 file2)
                    (ediff-files file2 file1)
                  (ediff-files file1 file2))
                (add-hook 'ediff-after-quit-hook-internal
                          (lambda ()
                            (setq ediff-after-quit-hook-internal nil)
                            (set-window-configuration wnd))))
            (error "no more than 2 files should be marked"))))

      (define-key dired-mode-map "e" 'ora-ediff-files)

      (defvar dired-filelist-cmd
        '(("vlc" "-L")))

      ;; FIXME: evilify dired mode will lead to startup warnings
      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        (kbd "C-k") 'zilongshanren/dired-up-directory
        "<RET>" 'dired-find-alternate-file
        "E" 'dired-toggle-read-only
        "C" 'dired-do-copy
        "<mouse-2>" 'my-dired-find-file
        "`" 'dired-open-term
        "p" 'peep-dired-prev-file
        "n" 'peep-dired-next-file
        "gr" 'revert-buffer
        "z" 'dired-get-size
        "c" 'dired-copy-file-here
        "J" 'counsel-find-file
        "f" 'zilongshanren/open-file-with-projectile-or-counsel-git
        ")" 'dired-omit-mode)
      )))


(defun zilongshanren-better-defaults/init-profiler ()
  (use-package profiler
    :init
    (evilified-state-evilify profiler-report-mode profiler-report-mode-map)))

;;; packages.el ends here
