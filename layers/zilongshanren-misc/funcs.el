;;; funcs.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren
;;
;; Author: zilongshanren <lyjdwh@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar inline-or-region-replace-last-input "")
(defvar inline-or-region-replace-history nil)
(defvar inline-or-region-replace-count 1)
(defvar inline-or-region-replace-original-buffer nil)
(defvar inline-or-region-replace-overlay nil)
(defvar inline-or-region-replace-beg nil)
(defvar inline-or-region-replace-end nil)

(defvar inline-or-region-replace-minibuffer-map (let ((map minibuffer-local-map))
                                        (define-key map (kbd "C-p") #'inline-or-region-replace-previous)
                                        (define-key map (kbd "C-n") #'inline-or-region-replace-next)
                                        map))

(defun zilongshanren/highlight-dwim ()
  (interactive)
  (if (use-region-p)
      (progn
        (highlight-frame-toggle)
        (deactivate-mark))
    (spacemacs/symbol-overlay)))

(defun zilongshanren/clearn-highlight ()
    (interactive)
  (clear-highlight-frame)
  (symbol-overlay-remove-all))

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(defmacro adjust-major-mode-keymap-with-evil (m &optional r)
  `(eval-after-load (quote ,(if r r m))
     '(progn
        (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
        ;; force update evil keymaps after git-timemachine-mode loaded
        (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))

;; insert ; at the end of current line
(defun zilongshanren/insert-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun zilongshanren/delete-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ";")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun zilongshanren/insert-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ",")))

(defun zilongshanren/delete-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ",")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun zilongshanren/insert-bracket-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ")")))

(defun zilongshanren/delete-bracket-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ")")
        (progn
          (backward-char)
          (delete-char 1)))))


(defun zilongshanren/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "liuyan")))

(defun zilongshanren/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "liuyan")))

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun my-unwind-git-timemachine ()
  (if (not (eq last-command-event 13))
      (git-timemachine-quit)))

;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
(defun my-git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let (collection)
    (setq collection
          (mapcar (lambda (rev)
                    ;; re-shape list for the ivy-read
                    (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :unwind #'my-unwind-git-timemachine
              :action (lambda (rev)
                        (git-timemachine-show-revision (cdr rev))))))

(defun my-git-timemachine ()
  "Open git snapshot with the selected version.  Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'my-git-timemachine-show-selected-revision))


(defun zilongshanren/helm-hotspots ()
  "helm interface to my hotspots, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(zilongshanren//hotspots-sources))))

(defun zilongshanren//hotspots-sources ()
  "Construct the helm sources for my hotspots"
  `((name . "Mail and News")
    (candidates . (("docs" . (lambda () (browse-url "https://devdocs.io/")))
                   ("RSS" . elfeed)
                   ("Github Trending" . (lambda() (browse-url "https://github.com/trending")))
                   ("Reddit" . (lambda() (browse-url "https://www.reddit.com/subreddits/mine/")))
                   ("Hack news" . (lambda() (browse-url "https://news.ycombinator.com/")))
                   ("emacs-china" . (lambda() (browse-url "https://emacs-china.org")))
                   ("Arxiv" . (lambda() (browse-url "http://arxiv.xixiaoyao.cn/")))
                   ("Agenda" . (lambda () (org-agenda "" "a")))
                   ("Random Todo" . org-random-entry)
                   ("Search" . (lambda () (call-interactively #'engine/search-google)))
                   ("pytorch 1.4" . (lambda () (browse-url "https://pytorch.apachecn.org/docs/1.4/")))
                   ("python 标准库" . (lambda () (browse-url "https://docs.python.org/zh-cn/3/library/index.html")))
                   ("Calculator" . (lambda () (helm-calcul-expression)))
                   ("Run current flie" . (lambda () (zilongshanren/run-current-file)))
                   ))
    (candidate-number-limit)
    (action . (("Open" . (lambda (x) (funcall x)))))))

;; insert date and time
(defun zilongshanren/now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun zilongshanren/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

(defun zilongshanren/open-file-with-projectile-or-counsel-git ()
  (interactive)
  (if (zilongshanren/git-project-root)
      (counsel-git)
    (if (projectile-project-p)
        (projectile-find-file)
      (counsel-file-jump))))

(defun zilongshanren/pomodoro-notification ()
  "show notifications when pomodoro end"
  (progn (add-hook 'org-pomodoro-finished-hook '(lambda () (zilongshanren/notify-send "'Pomodoro Finished, Have a break!'")))
           (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (zilongshanren/notify-send "'After a Short Break, Ready to Go?'")))
           (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (zilongshanren/notify-send "'After a Long Break, Ready to Go?'" )))))

(defun zilongshanren/notify-send (message)
  (shell-command (format "notify-send 'Pomodoro' %s -i emacs" message))
  )

(defun zilongshanren/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun zilongshanren/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; remove all the duplicated emplies in current buffer
(defun zilongshanren/single-lines-only ()
  "replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

;;add count for chinese, mainly used for writing chinese blog post
;; http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

(defun zilongshanren/word-count-for-chinese ()
  "「較精確地」統計中/日/英文字數。
- 文章中的註解不算在字數內。
- 平假名與片假名亦包含在「中日文字數」內，每個平/片假名都算單獨一個字（但片假
  名不含連音「ー」）。
- 英文只計算「單字數」，不含標點。
- 韓文不包含在內。

※計算標準太多種了，例如英文標點是否算入、以及可能有不太常用的標點符號沒算入等
。且中日文標點的計算標準要看 Emacs 如何定義特殊標點符號如ヴァランタン・アルカン
中間的點也被 Emacs 算為一個字而不是標點符號。"
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ; 去掉 org 文件的 OPTIONS（以#+開頭）
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
                                        ; 把註解行刪掉（不把註解算進字數）。
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字數（不含標點）：%s
中日文字數（包含標點）：%s
英文字數（不含標點）：%s
=======================
中英文合計（不含標點）：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))

(defun zilongshanren/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

(defun zilongshanren/git-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (locate-dominating-file directory ".git")))

(defadvice persp-switch (after my-quit-helm-perspectives activate)
  (setq hydra-deactivate t))

(defun wrap-sexp-with-new-round-parens ()
  (interactive)
  (insert "()")
  (backward-char)
  (sp-forward-slurp-sexp))

(defun evil-paste-after-from-0 ()
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))

(defun my-erc-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (zilongshanren/growl-notification
     (concat "ERC: : " (buffer-name (current-buffer)))
     message
     t
     )))

(defun my-swiper-search (p)
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'spacemacs/swiper-region-or-symbol
       #'swiper))))

(defun ivy-ff-checksum ()
  (interactive)
  "Calculate the checksum of FILE. The checksum is copied to kill-ring."
  (let ((file (expand-file-name (ivy-state-current ivy-last) ivy--directory))
        (algo (intern (ivy-read
                       "Algorithm: "
                       '(md5 sha1 sha224 sha256 sha384 sha512)))))
    (kill-new (with-temp-buffer
                (insert-file-contents-literally file)
                (secure-hash algo (current-buffer))))
    (message "Checksum copied to kill-ring.")))

(defun ivy-ff-checksum-action (x)
  (ivy-ff-checksum))

(defun my-find-file-in-git-repo (repo)
  (if (file-directory-p repo)
      (let* ((default-directory repo)
             (files (split-string (shell-command-to-string (format "cd %s && git ls-files" repo)) "\n" t)))
        (ivy-read "files:" files
                  :action 'find-file
                  :caller 'my-find-file-in-git-repo))
    (message "%s is not a valid directory." repo)))

(defun my-open-file-in-external-app (file)
  "Open file in external application."
  (interactive)
  (let ((default-directory (zilongshanren/git-project-root))
        (file-path file))
    (if file-path
        (cond
         ((spacemacs/system-is-mswindows) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         ((spacemacs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
         ((spacemacs/system-is-linux) (let ((process-connection-type nil))
                                        (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))

(defun ivy-insert-action (x)
  (with-ivy-window
    (insert x)))

(defun ivy-kill-new-action (x)
  (with-ivy-window
    (kill-new x)))

(defun counsel-goto-recent-directory ()
  "Recent directories"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection
              :action 'dired
              :caller 'counsel-goto-recent-directory)))

(defun counsel-find-file-recent-directory ()
  "Find file in recent git repository."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection
              :action 'my-find-file-in-git-repo
              :caller 'counsel-find-file-recent-directory)))

(defun zilongshanren/markdown-to-html ()
  (interactive)
  (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
  (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

(defun github-browse-file--relative-url ()
  "Return \"username/repo\" for current repository.

Error out if this isn't a GitHub repo."
  (require 'vc-git)
  (let ((url (vc-git--run-command-string nil "config" "remote.origin.url")))
    (unless url (error "Not in a GitHub repo"))
    (when (and url (string-match "github.com:?/?\\(.*\\)" url))
      (replace-regexp-in-string "\\.git$" "" (match-string 1 url)))))

(defun zilong/github-browse-commit ()
  "Show the GitHub page for the current commit."
  (interactive)
  (let* ((commit git-messenger:last-commit-id)
         (url (concat "https://github.com/"
                      (github-browse-file--relative-url)
                      "/commit/"
                      commit)))
    (browse-url url)
    (git-messenger:popup-close)))

(defun zilongshanren/show-current-buffer-major-mode ()
  (interactive)
  (describe-variable 'major-mode))

(defun zilongshanren/counsel-imenu ()
  (interactive)
  (counsel-imenu)
  (evil-set-jump))

;; define function to shutdown emacs server instance
(defun liuyan/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )
;; terminal transparency
(defun liuyan/toggle-terminal-transparency ()
  (interactive)
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(defun screenshot-frame (is-clip)
  "Take screenshot.
Default image ~/Pictures/TIMESTAMP.png
Usage:
M-x screenshot-frame
Enter custom-name or RET to save image with timestamp"
  ;; (interactive)
  (let* ((insert-default-directory t)
         (screenshots-dir "~/Pictures/")
         (sframe-name (concat (format-time-string "%d-%b-%Y-%T") ".png"))
         (sframe-full-path
          (read-file-name "Screenshot name: " screenshots-dir
                          (concat screenshots-dir sframe-name))))

    (if (not (file-accessible-directory-p screenshots-dir))
        (make-directory-internal screenshots-dir))

    (shell-command-to-string
     (concat "import " sframe-full-path))
    (if is-clip
        (call-process-shell-command (concat "xclip -sel clip -t image/png " sframe-full-path)))
    (message "Screenshot saved as %s" sframe-full-path)))

(defun screenshot ()
  (interactive)
  (screenshot-frame nil)
  )

(defun screenshot-clip ()
  (interactive)
  (screenshot-frame t)
  )

;; Network Proxy
(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)               ; defined 25.2+
    (if socks-noproxy
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost")
        socks-server '("Default server" "127.0.0.1" 1080 5))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" http-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,http-proxy)
          ("https" . ,http-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (proxy-http-disable)
    (proxy-http-enable)))

(defun liuyan/change-mode ()
  "change between evil-mode and meow-mode"
  (interactive)
  (if liuyan/mode-now
      (progn
        (setq liuyan/mode-now nil)
        (if (eq evil-mode t)
              (evil-mode -1))
        (if (eq meow-mode nil)
            (meow-global-mode 1)))
    (progn
      (setq liuyan/mode-now t)
      (if (eq evil-mode nil)
            (evil-mode 1))
      (if (eq meow-mode t)
          (meow-global-mode -1)))
    ))

;; winum users can use `winum-select-window-by-number' directly.
(defun my-select-window-by-number (win-id)
  "Use `ace-window' to select the window by using window index.
WIN-ID : Window index."
  (let ((wnd (nth (- win-id 1) (aw-window-list))))
    (if wnd
        (aw-switch-to-window wnd)
      (message "No such window."))))

(defun my-select-window ()
  (interactive)
  (let* ((event last-input-event)
         (key (make-vector 1 event))
         (key-desc (key-description key)))
    (my-select-window-by-number
     (string-to-number (car (nreverse (split-string key-desc "-")))))))

(defun awesome-tab-hide-tab-tab (x)
  "hide some tabs with regular expression"
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*flycheck" name)
     (string-prefix-p "*Messages" name)
     (string-prefix-p "*spacemacs" name)
     (string-prefix-p "*dashboard" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*helpful" name)
     (string-prefix-p "*Warnings" name)
     (string-prefix-p "*scratch" name)
     (string-prefix-p "*Agenda" name)
     (string-prefix-p "*liuyan" name)
     (string-prefix-p "*Ibuffer" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*snow" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

;; copy from https://github.com/bbatsov/crux
(defun crux-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun crux-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun crux-duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (dotimes (_ arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

;; copy from https://pengpengxp.github.io/emacs/counsel-bm.html
(defun bm-counsel-get-list (bookmark-overlays)
  (-map (lambda (bm)
          (with-current-buffer (overlay-buffer bm)
            (let* ((line (replace-regexp-in-string "\n$" "" (buffer-substring (overlay-start bm)
                                                                              (overlay-end bm))))
                   ;; line numbers start on 1
                   (line-num (+ 1 (count-lines (point-min) (overlay-start bm))))
                   (name (format "%s:%d - %s" (buffer-name) line-num line)))

              `(,name . ,bm))))
        bookmark-overlays))

(defun counsel-bm-update-input ()
  "Update fn for counsel-bm."
  (with-ivy-window
    (when (> (length (ivy-state-current ivy-last)) 0)
      (let* ((chosen (ivy-state-current ivy-last))
             (bookmark (gethash chosen bm-hash-table)))
        (if chosen
            (save-restriction
              (with-ivy-window
                (switch-to-buffer (overlay-buffer bookmark))
                (bm-goto bookmark)))
          nil)))))

(defun counsel-bm (&optional initial-input)
  "Use ivy to select bm bookmarks.
It has the ability to preview the bookmarks like `swiper-all'."
  (interactive)
  (let* ((bm-list (bm-counsel-get-list (bm-overlays-lifo-order t)))
         (bm-hash-table (make-hash-table :test 'equal))
         (search-list (-map (lambda (bm) (car bm)) bm-list)))

    (-each bm-list (lambda (bm)
                     (puthash (car bm) (cdr bm) bm-hash-table)))

    (if search-list
        (ivy-read "Find bookmark: "
                  search-list
                  :keymap counsel-describe-map

                  :action (lambda (chosen)
                            (let ((bookmark (gethash chosen bm-hash-table)))
                              (switch-to-buffer (overlay-buffer bookmark))
                              (bm-goto bookmark)))

                  :update-fn #'counsel-bm-update-input

                  :initial-input initial-input
                  :caller 'counsel-bm
                  )
      (message "%s" "No bookmark now."))))

(defun lisp-state-eval-sexp-end-of-line ()
  "Evaluate the last sexp at the end of the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (eval-last-sexp nil)))

(defun my-youdao-search-at-point ()
  "Search word at point and display result with `posframe', `pos-tip', or buffer."
  (interactive)
  (if (display-graphic-p)
      (youdao-dictionary-search-at-point-posframe)
    (youdao-dictionary-search-at-point)))

(defun inline-or-region-replace-previous ()
  "Previous match."
  (interactive)
  (when (> inline-or-region-replace-count 1)
    (decf inline-or-region-replace-count)))

(defun inline-or-region-replace-next ()
  "Next match."
  (interactive)
  (incf inline-or-region-replace-count))

(defun inline-or-region-replace ()
  "Search for the matching REGEXP COUNT times before END.
You can use \\&, \\N to refer matched text."
  (interactive)
  (condition-case nil
      (save-excursion
        (setq inline-or-region-replace-beg (car (crux-get-positions-of-line-or-region)))
        (setq inline-or-region-replace-end (cdr (crux-get-positions-of-line-or-region)))

        (setq inline-or-region-replace-original-buffer (current-buffer))
        (add-hook 'post-command-hook #'inline-or-region-replace-highlight)

        (let* ((minibuffer-local-map inline-or-region-replace-minibuffer-map)
               (input (read-string "regexp/replacement: " nil 'inline-or-region-replace-history))
               (replace (or (nth 1 (split-string input "/")) "")))

          (goto-char inline-or-region-replace-beg)
          (re-search-forward (car (split-string input "/")) inline-or-region-replace-end t inline-or-region-replace-count)

          (unless (equal input inline-or-region-replace-last-input)
            (push input inline-or-region-replace-history)
            (setq inline-or-region-replace-last-input input))
          (remove-hook 'post-command-hook #'inline-or-region-replace-highlight)
          (delete-overlay inline-or-region-replace-overlay)
          (replace-match replace)
          (setq inline-or-region-replace-count 1)))
    ((quit error)
     (delete-overlay inline-or-region-replace-overlay)
     (remove-hook 'post-command-hook #'inline-or-region-replace-highlight)
     (setq inline-or-region-replace-count 1))))

(defun inline-or-region-replace-highlight ()
  "Highlight matched text and replacement."
  (when inline-or-region-replace-overlay
    (delete-overlay inline-or-region-replace-overlay))
  (when (>= (point-max) (length "regexp/replacement: "))
    (let* ((input (buffer-substring-no-properties (1+ (length "regexp/replacement: ")) (point-max)))
           (replace (or (nth 1 (split-string input "/")) "")))
      (with-current-buffer inline-or-region-replace-original-buffer

        (goto-char inline-or-region-replace-beg)
        ;; if no match and count is greater than 1, try to decrease count
        ;; this way if there are only 2 match, you can't increase count to anything greater than 2
        (while (and (not (re-search-forward (car (split-string input "/")) inline-or-region-replace-end t inline-or-region-replace-count))
                    (> inline-or-region-replace-count 1))
          (decf inline-or-region-replace-count))
        (setq inline-or-region-replace-overlay (make-overlay (match-beginning 0) (match-end 0)))
        (overlay-put inline-or-region-replace-overlay 'face '(:strike-through t :background "#75000F" :foreground "red"))
        (overlay-put inline-or-region-replace-overlay 'after-string (propertize replace 'face '(:background "#078A00")))))))

(defun english-teacher-posframe-show-result-function (origin translation)
  (require 'posframe)
  (when (posframe-workable-p)
    (posframe-show
     "*english-teacher*"
     :string (concat (symbol-name english-teacher-backend) "\n" origin "\n" translation)
     :timeout 100
     :poshandler 'posframe-poshandler-point-bottom-left-corner
     :internal-border-width 10)
    (unwind-protect
        (push (read-event) unread-command-events)
      (posframe-delete "*english-teacher*"))))

(defun enable-ivy/helm-posframe ()
  (interactive)
  (ivy-posframe-mode 1)
  (helm-posframe-enable)
  )

(defun disable-ivy/helm-posframe ()
  (interactive)
  (ivy-posframe-mode -1)
  (helm-posframe-disable)
  )
(help-at-pt-kbd-string)

(defun mouse-hover-tooltip (&optional arg)
  "Show mouse hover help info using pos-tip-show."
  (interactive)
  (let ((help (help-at-pt-kbd-string)))
    (if help
        (pos-tip-show help nil nil nil 0)
      (if (not arg) (message "No local help at point"))))
  (unwind-protect
      (push (read-event) unread-command-events)
    (pos-tip-hide)))

(define-advice server-eval-and-print (:around (&rest r) print-buffer)
  "Print the current buffer instead of the result of evaluation."
  (require 'seq)
  (seq-let (_ expr proc) r
    (with-local-quit (eval (car (read-from-string expr))))
    (when proc
      (require 'e2ansi)
      (server-reply-print
       (server-quote-arg
        (let ((e2ansi-background-mode 'dark))
          (e2ansi-string-to-ansi (buffer-string))))
       proc))))

(defun smart-tab-jump-out (fn &optional arg)
  (interactive "P")
  (if (-contains? (list "\"" "'" ")" "}" ";" "|" ">" "]" ) (make-string 1 (char-after)))
      (forward-char 1)
    (funcall-interactively fn arg)))

(advice-add 'indent-for-tab-command :around 'smart-tab-jump-out)

(defun avy-goto-parens ()
  (interactive)
  (let ((avy-command this-command))   ; for look up in avy-orders-alist
    (avy-jump "[({\[]+")))

(defun browse-repo-at-remote ()
  "Browse the current repo with `browse-url'."
  (interactive)
  (let* ((remote-ref (browse-at-remote--remote-ref (buffer-file-name)))
         (remote (car remote-ref))
         (target-repo (browse-at-remote--get-url-from-remote remote))
         (repo-url (cdr target-repo)))
    (browse-url repo-url)
    ))

;; filter of ivy occur
;; keybindings: / filter lines; C-/ undo
(defvar ivy-occur-filter-prefix ">>> ")

(defun ivy-occur/filter-lines ()
  (interactive)
  (unless (string-prefix-p "ivy-occur" (symbol-name major-mode))
    (user-error "Current buffer is not in ivy-occur mode"))

  (let ((inhibit-read-only t)
        (regexp (read-regexp "Regexp(! for flush)"))
        (start (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "[0-9]+ candidates:"))))
    (if (string-prefix-p "!" regexp)
        (flush-lines (substring regexp 1) start (point-max))
      (keep-lines regexp start (point-max)))
    (save-excursion
      (goto-char (point-min))
      (let ((item (propertize (format "[%s]" regexp) 'face 'ivy-current-match)))
        (if (looking-at ivy-occur-filter-prefix)
            (progn
              (goto-char (line-end-position))
              (insert item))
          (insert ivy-occur-filter-prefix item "\n"))))))

(defun ivy-occur/undo ()
  (interactive)
  (let ((inhibit-read-only t))
    (if (save-excursion
          (goto-char (point-min))
          (looking-at ivy-occur-filter-prefix))
        (undo)
      (user-error "Filter stack is empty"))))

(defun ivy|occur-mode-setup ()
  (local-set-key "/" #'ivy-occur/filter-lines)
  (local-set-key (kbd "C-/") #'ivy-occur/undo))

(add-hook 'ivy-occur-mode-hook 'ivy|occur-mode-setup)
(add-hook 'ivy-occur-grep-mode-hook 'ivy|occur-mode-setup)

(defun meow-setup ()
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-delete)
   '("x" . meow-line)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-keyboard-quit)
   '("G" . goto-line)
   '("h" . meow-head)
   '("H" . meow-head-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("m" . meow-join)
   '("M" . delete-indentation)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-replace-save)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("l" . meow-tail)
   '("L" . meow-tail-expand)
   '("u" . undo)
   '("v" . meow-visit)
   '("e" . meow-next-word)
   '("e" . meow-next-symbol)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("<escape>" . meow-last-buffer)))

;; https://emacs-china.org/t/org-roam-v1-2-3-headline/15978/7
(setq-default wr--head-var-a "headline name")

(defun wr/counsel-outline-action (x)
  (setq wr--head-var-a (car x)))

(defun wr/counsel-outline ()
  (interactive)
  (let ((settings (cdr (assq major-mode counsel-outline-settings))))
    (ivy-read "Outline: " (counsel-outline-candidates settings)
              :action (or (plist-get settings :action)
                          #'wr/counsel-outline-action)
              :history (or (plist-get settings :history)
                           'counsel-outline-history)
              :preselect (max (1- counsel-outline--preselect) 0)
              :caller (or (plist-get settings :caller)
                          'wr/counsel-outline))))

(defun wr/insert-a-head-from-a-file (x)
  (interactive)
  (xref-push-marker-stack)
  (save-restriction
    (save-excursion
      (with-ivy-window
        (pcase (cdr x)
          (`(:path ,foo :title ,bar) (progn
                                       (with-temp-buffer
                                         (erase-buffer)
                                         (insert-file-contents foo)
                                         (call-interactively #'wr/counsel-outline))
                                       (insert (format "[[file:%s::*%s][%s]]" foo (car (last (split-string (substring-no-properties wr--head-var-a) " → ")))
                                                       (string-trim-left (car (last (split-string (substring-no-properties wr--head-var-a) " → "))))))))))
      (message "A head has been inserted.")
      (xref-goto-xref))))
