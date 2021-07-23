;; -*- coding: utf-8 -*-

(setq zilongshanren-misc-packages
      '(
        helm-ag
        projectile
        find-file-in-project
        multiple-cursors
        visual-regexp
        visual-regexp-steroids
        evil
        discover-my-major
        tiny
        expand-region
        markdown-mode
        swiper
        magit
        git-messenger
        gist
        hydra
        wrap-region
        ranger
        golden-ratio
        (highlight-global :location local)
        symbol-overlay
        popup
        keyfreq
        terminal-here
        git-gutter
        speed-type
        zone
        youdao-dictionary
        go-translate
        posframe
        rime
        sis
        try
        figlet
        (thing-edit :location (recipe :fetcher github :repo "lyjdwh/thing-edit"))
        (avy-thing-edit :location (recipe :fetcher github :repo "lyjdwh/avy-thing-edit"))
        (one-key :location local)
        (grep-dired :location (recipe :fetcher github :repo "manateelazycat/grep-dired"))
        (delete-block :location (recipe :fetcher github :repo "manateelazycat/delete-block"))
        browse-kill-ring
        bbyac
        meow
        evil-snipe
        powerthesaurus
        mw-thesaurus
        langtool
        (inherit-org :location (recipe :fetcher github :repo "chenyanming/inherit-org"))
        (awesome-tab :location (recipe :fetcher github :repo "manateelazycat/awesome-tab"))
        bm
        counsel
        pdf-tools
        pdfgrep
        separedit
        pyim
        key-chord
        evil-pinyin
        kaomoji
        (english-teacher :location (recipe :fetcher github :repo "loyalpartner/english-teacher.el"))
        pos-tip
        ;; (maple-header :location (recipe :fetcher github :repo "honmaple/maple-emacs" :files ("site-lisp/maple/maple-header.el")))
        ace-pinyin
        tmux-pane
        wttrin
        ;; bongo
        ;; podcaster
        ;; mingus
        helm-chrome
        major-mode-hydra
        elfeed
        (shengci :location (recipe :fetcher github :repo "EvanMeek/shengci.el"))
        evil-text-object-python
        evil-matchit
        avy
        (telega :location (recipe :fetcher github :repo "zevlg/telega.el" :files ("*")))
        vterm
        (casease :location (recipe :fetcher github :repo "DogLooksGood/casease"))
        counsel-projectile
        mu4e
        (org-media-note :location (recipe :fetcher github :repo "yuchen-lea/org-media-note"))
        (netease-cloud-music :location (recipe :fetcher github :repo "SpringHan/netease-cloud-music.el"))
        elisp-demos
        ))

(defun zilongshanren-misc/init-elisp-demos()
  (use-package elisp-demos
    :config
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
    (spacemacs/set-leader-keys-for-major-mode 'helpful-mode
      "d" #'elisp-demos-for-helpful
      "a" #'elisp-demos-add-demo)
    (spacemacs/set-leader-keys "hdd" #'elisp-demos-find-demo)
    ))

(defun zilongshanren-misc/init-netease-cloud-music()
  (use-package netease-cloud-music
    :config
    (setq netease-cloud-music-show-lyric 'all)
    (setq netease-cloud-music-repeat-mode "random")
    (require 'netease-cloud-music-ui)

    (set-face-attribute 'netease-cloud-music-artist-face nil
                        :inherit nil
                        :foreground "MediumSpringGreen"
                        :height 1.0
                        )
    (set-face-attribute 'netease-cloud-music-playlist-face nil
                        :foreground "light sky blue"
                        :weight 'normal
                        )
    (set-face-attribute 'netease-cloud-music-playlists-face nil
                        :foreground "light sky blue"
                        :weight 'bold
                        :height 1.2
                        )

    ;; [remap netease-cloud-music-clear-playlist]
    (define-key netease-cloud-music-mode-map (kbd "k") #'evil-previous-line)
    (define-key netease-cloud-music-mode-map (kbd "j") #'evil-next-line)
    (define-key netease-cloud-music-mode-map (kbd "J") '(lambda () (interactive) (evil-next-line 5)))
    (define-key netease-cloud-music-mode-map (kbd "K") '(lambda () (interactive) (evil-previous-line 5)))
    (define-key netease-cloud-music-mode-map (kbd "D") #'netease-cloud-music-clear-playlist)
    (define-key netease-cloud-music-mode-map (kbd "a") #'netease-cloud-music-storage-current-song)
    (define-key netease-cloud-music-mode-map (kbd "A") #'netease-cloud-music-storage-song)
    (define-key netease-cloud-music-mode-map (kbd "S") #'netease-cloud-music-add-storage-to-current-playlist)
    (define-key netease-cloud-music-mode-map (kbd "O") #'netease-cloud-music-clear-storage)

    (define-key netease-cloud-music-switch-song-mode-map (kbd "k") #'evil-previous-line)
    (define-key netease-cloud-music-switch-song-mode-map (kbd "j") #'evil-next-line)
    (define-key netease-cloud-music-switch-song-mode-map (kbd "J") '(lambda () (interactive) (evil-next-line 5)))
    (define-key netease-cloud-music-switch-song-mode-map (kbd "K") '(lambda () (interactive) (evil-previous-line 5)))

    (define-key netease-cloud-music-switch-playlist-mode-map (kbd "k") #'evil-previous-line)
    (define-key netease-cloud-music-switch-playlist-mode-map (kbd "j") #'evil-next-line)
    (define-key netease-cloud-music-switch-playlist-mode-map (kbd "J") '(lambda () (interactive) (evil-next-line 5)))
    (define-key netease-cloud-music-switch-playlist-mode-map (kbd "K") '(lambda () (interactive) (evil-previous-line 5)))
    ))

(defun zilongshanren-misc/init-org-media-note()
  (use-package org-media-note
    :hook ((org-mode .  org-media-note-mode)
           (org-mode .  org-media-note-setup-org-ref)
           )
    :config
    (require 'org-attach)
    (spacemacs/set-leader-keys "av" 'org-media-note-hydra/body)
    (setq org-media-note-screenshot-image-dir "~/org-notes/notes/imgs")
    ))

(defun zilongshanren-misc/post-init-mu4e()
  (setq mu4e-account-alist
        '(("Gmail"
           ;; Under each account, set the account-specific variables you want.
           (mu4e-sent-messages-behavior sent)
           (mu4e-sent-folder "~/.mail/gmail/[Gmail].已发邮件")
           (mu4e-drafts-folder "~/.mail/gmail/[Gmail].草稿")
           (user-mail-address "lyjdwh@gmail.com")
           (user-full-name "liuyan"))
          ("Foxmail"
           (mu4e-sent-messages-behavior sent)
           (mu4e-sent-folder "/home/liuyan/.mail/qq/Sent Messages")
           (mu4e-drafts-folder "/home/liuyan/.mail/qq/Drafts")
           (user-mail-address "1412511544@qq.com")
           (user-full-name "liuyan"))
          ))

;;; Set up some common mu4e variables
  (setq mu4e-maildir "~/.mail"
        mu4e-trash-folder "/trash"
        mu4e-refile-folder "/archive"
        mu4e-get-mail-command "offlineimap -o"
        mu4e-update-interval 3600
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-enable-mode-line t
        mu4e-enable-notifications t
        message-kill-buffer-on-exit t
        mu4e-compose-signature-auto-include nil
        )

;;; Bookmarks
  (with-eval-after-load 'mu4e
    (setq mu4e-bookmarks
          `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
            ("date:today..now" "Today's messages" ?t)
            ("date:7d..now" "Last 7 days" ?w)
            ("mime:image/*" "Messages with images" ?p)
            (,(mapconcat 'identity
                         (mapcar
                          (lambda (maildir)
                            (concat "maildir:" (car maildir)))
                          mu4e-maildir-shortcuts) " OR ")
             "All inboxes" ?i)
            )))

  (with-eval-after-load 'mu4e-alert
    ;; Enable Desktop notifications
    ;; (mu4e-alert-set-default-style 'notifications)) ; For linux
    (mu4e-alert-set-default-style 'libnotify))  ; Alternative for linux
  ;; (mu4e-alert-set-default-style 'notifier))   ; For Mac OSX (through the
                                        ; terminal notifier app)
  ;; (mu4e-alert-set-default-style 'growl))      ; Alternative for Mac OSX


  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; try to emulate some of the eww key-bindings
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))

  (add-hook 'mu4e-main-mode-hook
            (lambda ()
              (evilified-state-evilify-map mu4e-main-mode-map
                :mode mu4e-main-mode
                :bindings
                (kbd "j") 'next-line
                (kbd "J") 'mu4e~headers-jump-to-maildir)
              ))

  ;; something about ourselves
  (setq user-mail-address "1412511544@qq.com"
        user-full-name "liuyan")

  (setq smtpmail-auth-credentials     (expand-file-name "~/.authinfo")
        smtpmail-stream-type          'tls
        smtpmail-smtp-server          "smtp.qq.com"
        smtpmail-smtp-service         465
        smtpmail-smtp-user "1412511544@qq.com")
  )

(defun zilongshanren-misc/post-init-counsel-projectile()
  (with-eval-after-load 'counsel
    (counsel-projectile-mode 1)
    ))

(defun zilongshanren-misc/init-casease()
  (use-package casease
    :config
    (casease-setup
     :hook python-mode-hook
     :separator ?-
     :entries
     ((pascal "\\(-\\)[a-z]" "[A-Z]")
      (snake "[a-z]")))
    )
  )
(defun zilongshanren-misc/post-init-vterm()
  (setq vterm-max-scrollback 5000)
  (add-hook 'vterm-mode-hook 'evil-emacs-state)
  )

(defun zilongshanren-misc/init-telega()
  (use-package telega
    :commands telega
    :hook
    ('telega-chat-mode . #'company-mode)
    ('telega-chat-mode . #'yas-minor-mode-on)
    ('telega-chat-mode . (lambda ()
                           (make-local-variable 'company-backends)
                           (dolist (it (append '(telega-company-emoji
                                                 telega-company-username
                                                 telega-company-hashtag)
                                               (when (telega-chat-bot-p telega-chatbuf--chat)
                                                 '(telega-company-botcmd))))
                             (push it company-backends))
                           ))
    :config
    (setq telega-proxies '((:server "localhost" :port 1080
                                    :enable t :type (:@type "proxyTypeSocks5")))
          telega-sticker-set-download t)

    (evil-set-initial-state 'telega-root-mode 'emacs)
    (evil-set-initial-state 'telega-chat-mode 'emacs)

    (define-key telega-msg-button-map (kbd "k") 'nil)

    (with-eval-after-load 'all-the-icons
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(telega-root-mode all-the-icons-fileicon "telegram"
                                      :heigt 1.0
                                      :v-adjust -0.2
                                      :face all-the-icons-yellow))
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(telega-chat-mode all-the-icons-fileicon "telegram"
                                      :heigt 1.0
                                      :v-adjust -0.2
                                      :face all-the-icons-blue)))

    ))

(defun zilongshanren-misc/post-init-avy ()
  (progn
    (setq avy-keys (number-sequence ?a ?z))
    (setq avy-orders-alist
          '((avy-goto-char . avy-order-closest)
            (avy-goto-char-2 . avy-order-closest)
            (avy-goto-parens . avy-order-closest)
            (avy-goto-word-or-subword-1 . avy-order-closest)
            (avy-goto-line . avy-order-closest)))

    (evil-define-key '(normal virsual) 'global-map (kbd "gp") 'avy-goto-parens)
    (evil-define-key '(normal virsual) 'global-map (kbd "gl") 'avy-goto-line)
    (evil-define-key 'normal 'global-map (kbd "S") 'evil-avy-goto-char-2)
    (evil-define-key 'normal 'global-map (kbd "s") 'evil-avy-goto-char)
    (evil-define-key 'visual 'global-map (kbd "F") 'evil-avy-goto-char-2)
    (evil-define-key 'visual 'global-map (kbd "f") 'evil-avy-goto-char)
    ))

(defun zilongshanren-misc/post-init-evil-matchit ()
  (progn
    (setq evilmi-shortcut "M")
    (global-evil-matchit-mode 1)
    ))

(defun zilongshanren-misc/init-evil-text-object-python ()
  (use-package evil-text-object-python
    :config
    (setq evil-text-object-python-statement-key "x")
    (add-hook 'python-mode-hook 'evil-text-object-python-add-bindings)
    ))

(defun zilongshanren-misc/init-shengci ()
  (use-package shengci
    :commands shengci-show-recorded-word shengci-capture-word-and-save
    shengci-show-memorized-word shengci-practice-guess-recorded-word
    shengci-practice-guess-memorized-word
    ))

(defun zilongshanren-misc/init-major-mode-hydra ()
  (use-package major-mode-hydra))

(defun zilongshanren-misc/post-init-elfeed ()
  (progn
  (cl-defmacro my/elfeed-search-view-hydra-define (name body views)
    "Define a pretty hydra named NAME with BODY and VIEWS.
  VIEWS is a plist: in it, each property is a string which becomes
  a column header in the hydra, and each value is a list of lists
  in this format: (KEY COMPONENT &optional LABEL).

  The KEY is a key sequence passed to `kbd', like \"s\" or \"S
  TAB\".  The COMPONENT is an Elfeed filter component, which may
  begin with \"+\" or \"=\", and in which spaces are automatically
  escaped as required by Elfeed.  The LABEL, if present, is a
  string displayed next to the KEY; if absent, COMPONENT is
  displayed.

  In the resulting hydra, when KEY is pressed, the COMPONENT is
  toggled in `elfeed-search-filter'.  It is toggled between three
  states: normal, inverse, and absent.  For example, the component
  \"+tag\" cycles between three states in the filter: \"+tag\",
  \"-tag\", and \"\".  The appropriate inverse prefix is used
  according to the component's prefix (i.e. for \"=\", the inverse
  is \"~\", and for \"\" (a plain regexp), \"!\" is used).

  These special components may be used to read choices from the
  Elfeed database with completion and toggle them:

    :complete-age   Completes and sets the age token.
    :complete-feed  Completes and toggles a feed token.
    :complete-tag   Completes and toggles a tag token.
    nil             Sets default filter.

  A complete example:

    (my/elfeed-search-view-hydra-define my/elfeed-search-view-hydra
      (:foreign-keys warn)
      (\"Views\"
       ((\"@\" :complete-age \"Date\")
        (\"d\" nil))
       \"Status\"
       ((\"su\" \"+unread\"))
       \"Feed\"
       ((\"f TAB\" :complete-feed \"Choose\")
        (\"fE\" \"=Planet Emacslife\" \"Planet Emacslife\"))
       \"Tags\"
       ((\"t TAB\" :complete-tag \"Choose\")
        (\"te\" \"+Emacs\"))
       \"\"
       ((\"tn\" \"+news\"))))"
    (declare (indent defun))
    (cl-labels ((escape-spaces (string)
                               ;; Return STRING with spaces escaped with "\s-".  Necessary
                               ;; because Elfeed treats all literal spaces as separating tokens.
                               (replace-regexp-in-string (rx space) "\\s-" string t t)))
      (let* ((completion-fns
              (list (cons :complete-age
                          (lambda ()
                            (interactive)
                            (save-match-data
                              (let* ((date-regexp (rx (group (or bos blank) "@" (1+ digit) (1+ (not blank)))))
                                     (date-tag (when (string-match date-regexp elfeed-search-filter)
                                                 (match-string 1 elfeed-search-filter))))
                                (elfeed-search-set-filter
                                 (replace-regexp-in-string date-regexp (read-string "Date: " date-tag)
                                                           elfeed-search-filter t t))))))
                    (cons :complete-feed
                          '(concat "=" (replace-regexp-in-string
                                        (rx space) "\\s-"
                                        (->> (hash-table-values elfeed-db-feeds)
                                             (--map (elfeed-meta it :title))
                                             (completing-read "Feed: ")
                                             regexp-quote) t t)))
                    (cons :complete-tag
                          '(concat "+" (completing-read "Tag: " (elfeed-db-get-all-tags))))))
             (body (append '(:title elfeed-search-filter :color pink :hint t :quit-key "q")
                           body))
             (heads (cl-loop for (heading views) on views by #'cddr
                             collect heading
                             collect (cl-loop for (key component label) in views
                                              collect
                                              `(,key
                                                ,(cl-typecase component
                                                   ((and function (not null))
                                                    ;; I don't understand why nil matches
                                                    ;; (or lambda function), but it does,
                                                    ;; so we have to account for it.  See
                                                    ;; (info-lookup-symbol 'cl-typep).
                                                    `(funcall ,component))
                                                   (string
                                                    `(elfeed-search-set-filter
                                                      (my/elfeed-search-filter-toggle-component
                                                       elfeed-search-filter ,(escape-spaces component))))
                                                   (otherwise
                                                    `(elfeed-search-set-filter
                                                      ,(when component
                                                         `(my/elfeed-search-filter-toggle-component
                                                           elfeed-search-filter ,component)))))
                                                ,(or label component "Default"))))))
        ;; I am so glad I discovered `cl-sublis'.  I tried several variations of `cl-labels' and
        ;; `cl-macrolet' and `cl-symbol-macrolet', but this is the only way that has worked.
        (setf heads (cl-sublis completion-fns heads))
        `(pretty-hydra-define ,name ,body
           ,heads))))

  (cl-defun my/elfeed-search-filter-toggle-component (string component)
    "Return STRING (which should be `elfeed-search-filter') having toggled COMPONENT.
  Tries to intelligently handle components based on their prefix:
  +tag, =feed, regexp."
    (save-match-data
      (cl-labels ((toggle (component +prefix -prefix string)
                          (let ((+pat (rx-to-string `(seq (or bos blank)
                                                          (group ,+prefix ,component)
                                                          (or eos blank))))
                                (-pat (rx-to-string `(seq (group (or bos (1+ blank)) ,-prefix ,component)
                                                          (or eos blank)))))
                            ;; TODO: In newer Emacs versions, the `rx' pattern `literal'
                            ;; evaluates at runtime in `pcase' expressions.
                            (pcase string
                              ((pred (string-match +pat)) (rm (concat -prefix component) string))
                              ((pred (string-match -pat)) (rm "" string))
                              (_ (concat string " " +prefix component)))))
                  (rm (new string) (replace-match new t t string 1)))
        (pcase component
          ((rx bos "+" (group (1+ anything)))
           (toggle (match-string 1 component) "+" "-" string))
          ((rx bos "=" (group (1+ anything)))
           (toggle (match-string 1 component) "=" "~" string))
          (_ (toggle component "" "!" string))))))

  (my/elfeed-search-view-hydra-define my/elfeed-search-view-hydra
                                              (:foreign-keys warn)
                                              ("Views"
                                               (("@" :complete-age "Date")
                                                ("d" nil))
                                               "Status"
                                               (("wu" "+unread"))
                                               "Feed"
                                               (("f TAB" :complete-feed "Choose"))
                                               "Tags"
                                               (("t TAB" :complete-tag "Choose"))
                                               ))

  (evilified-state-evilify-map elfeed-search-mode-map
    :mode elfeed-search-mode
    :eval-after-load elfeed-search
    :bindings
    "?"  'my/elfeed-search-view-hydra/body)
  ))

(defun zilongshanren-misc/init-helm-chrome ()
  (use-package helm-chrome
    :commands helm-chrome-bookmarks
    ))

;; (defun zilongshanren-misc/init-podcaster ()
;;   (use-package podcaster
;;     :commands podcaster
;;     :config
;;     (setq podcaster-feeds-urls
;; 	      '("http://www.ximalaya.com/album/5574153.xml"      ;;日谈公园
;; 	        "http://www.ximalaya.com/album/8583636.xml"      ;;大内密谈
;; 	        "http://www.ximalaya.com/album/3558668.xml"      ;;黑水公园
;;             "https://www.pythonhunter.org/episodes/feed.xml" ;;捕蛇者说
;;             "https://feeds.pacific-content.com/commandlineheroes" ;;command line heroes
;;             "https://pan.icu/feed"                                ;;内核恐慌
;;             ))
;;     (setq podcaster-mp3-player "ffplay")))

;; (defun zilongshanren-misc/init-bongo ()
;;   (use-package bongo
;;     :commands bongo-playlist
;;     :bind (:map bongo-playlist-mode-map
;;                 ;; `h'         Switch from playlist to library
;; 	            ("<return>" . bongo-dwim)
;; 	            ("<space>" . bongo-pause/resume)
;; 	            ("i" . bongo-insert-file)
;; 	            ("p" . bongo-play-previous)
;; 	            ("n" . bongo-play-next)
;; 	            ("d" . bongo-dired-line)
;; 	            ("e" . bongo-append-enqueue)
;; 	            ("s" . bongo-seek)
;; 	            ("r" . bongo-play-random)
;;                 ("v" . volume))
;;     :custom
;;     (bongo-enabled-backends '(mpv))
;;     (bongo-default-directory "~/Music/")))
;; (defun zilongshanren-misc/init-mingus ()
;;   (use-package mingus
;;     :commands mingus
;;     ))

(defun zilongshanren-misc/init-wttrin ()
  (use-package wttrin
    :ensure t
    :commands wttrin
    :config
    (setq wttrin-default-cities '("~Shanghai+Minhang")
          wttrin-default-accept-language '("Accept-Language" . "zh-CN"))

    (defun wttrin-fetch-raw-string (query)
      "Get the weather information based on your QUERY."
      (let ((url-user-agent "curl"))
        (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
        (with-current-buffer
            (url-retrieve-synchronously
             (concat "https://wttr.in/" query)
             (lambda (status) (switch-to-buffer (current-buffer))))
          (decode-coding-string (buffer-string) 'utf-8))))

    (defun wttrin ()
      "Display weather information for CITY."
      (interactive)
      (wttrin-query (car wttrin-default-cities)))
    ))

(defun zilongshanren-misc/init-tmux-pane ()
  (use-package tmux-pane
    :commands tmux-pane-open-vertical tmux-pane-open-horizontal
    :config
    (tmux-pane-mode)
    (setq tmux-pane--override-map-enable nil)
    (setq tmux-pane-vertical-percent 30)
    ))

(defun zilongshanren-misc/init-ace-pinyin ()
  (use-package ace-pinyin
    :config
    (ace-pinyin-global-mode 1)
    (setq ace-pinyin-enable-punctuation-translation nil)
    ))

(defun zilongshanren-misc/init-maple-header ()
  (use-package maple-header
    :hook (prog-mode . maple-header-mode)
    :config
    (defun maple-header/template(&optional prefix)
      "Template with PREFIX."
      (replace-regexp-in-string
       "^" (or prefix comment-start)
       (concat
        (make-string 70 ?*) "\n"
        "Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
        "File Name: " (file-name-nondirectory buffer-file-name) "\n"
        "Author: " (user-full-name)"\n"
        "Email: " user-mail-address "\n"
        "Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
        "Last Update: \n"
        "         By: \n"
        "Description: \n"
        (make-string 70 ?*))))

    (maple-header modify-by
                         :find ".*\\(By:\\)\\(.*\\)"
                         :replace user-mail-address)

    (setq maple-header/auto-update-alist '(filename email modify modify-by))

    (add-to-list 'maple-header/auto-insert-alist
                 '((sh-mode . "Shell script") nil
                   "#!/usr/bin/env bash\n"
                   (maple-header:template) "\n"))

    (add-to-list 'maple-header/auto-insert-alist
                 '((c++-mode . "C++ program") nil
                   "/*"
                   (string-trim-left
                    (maple-header:template " "))
                   "*/\n"))
    ))

(defun zilongshanren-misc/init-pos-tip ()
  (use-package pos-tip))

(defun zilongshanren-misc/init-english-teacher ()
  (use-package english-teacher
    :commands english-teacher-follow-mode english-teacher-smart-translate
    ;; :hook ((Info-mode
    ;;         elfeed-show-mode
    ;;         eww-mode
    ;;         Man-mode
    ;;         Woman-Mode) . english-teacher-follow-mode)
    :config
    (setq english-teacher-show-result-function 'english-teacher-posframe-show-result-function)
    ))

(defun zilongshanren-misc/init-kaomoji ()
  (use-package kaomoji
    :commands kaomoji insert-kaomoji-into-kill-ring
    :config
    (defun insert-kaomoji-into-kill-ring ()
      "Insert a kaomoji directly into `kill-ring'."
      (interactive)
      (helm :sources (helm-build-sync-source "Please input pattern to search Kaomoji: "
                       :candidates (lambda () (kaomoji-get-candidates helm-pattern))
                       :volatile t
                       :action (lambda (str) (kill-new (kaomoji-process-the-string-to-insert helm-pattern str)))
                       :candidate-number-limit kaomoji-candidates-limit)
            :buffer kaomoji-buffer-name
            :prompt kaomoji-prompt))

    (setq kaomoji-table
          (append '(
                    (("shy") . "⁄(⁄ ⁄•⁄ω⁄•⁄ ⁄)⁄")
                    (("smile") . "ヽ( ^∀^)ﾉ")
                    (("smile") . "( ´∀`)")
                    (("angry") . "(ﾉ｀⊿´)ﾉ")
                    )
                    kaomoji-table))
    ))

(defun zilongshanren-misc/init-evil-pinyin ()
  (use-package evil-pinyin
    :config
    (global-evil-pinyin-mode)
    ))

(defun zilongshanren-misc/init-key-chord ()
  (use-package key-chord
    :config
    (setq key-chord-two-keys-delay 0.2)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "js" 'hydra-insert-symbols/body)
    (key-chord-define evil-insert-state-map "jd" 'hydra-insert-numbers/body)
    (key-chord-mode 1)
    ))

(defun zilongshanren-misc/init-pyim ()
  (use-package pyim
    :after ivy
    :config
    (defun eh-ivy-cregexp (str)
      (let ((x (ivy--regex-ignore-order str))
            (case-fold-search nil))
        (if (listp x)
            (mapcar (lambda (y)
                      (if (cdr y)
                          (list (if (equal (car y) "")
                                    ""
                                  (pyim-cregexp-build (car y)))
                                (cdr y))
                        (list (pyim-cregexp-build (car y)))))
                    x)
          (pyim-cregexp-build x))))

    (setq ivy-re-builders-alist
          '((t . eh-ivy-cregexp)
            (spacemacs/counsel-search . spacemacs/ivy--regex-plus)))
    ))

(defun zilongshanren-misc/init-separedit ()
  "if there are nested string or code block, just continue to enter a new edit buffer"
  (use-package separedit
    :commands separedit
    ;; :config
    ;; (setq separedit-default-mode 'markdown-mode)
    ))

(defun zilongshanren-misc/post-init-pdf-tools ()
  (progn
    (add-hook 'pdf-view-mode-hook
              (lambda ()
                (pdf-annot-minor-mode 1)
                (pdf-history-minor-mode 1)
                (pdf-sync-minor-mode 1)
                (pdf-links-minor-mode)
                ))

    (defun pdf-view-mouse-set-region (event &optional allow-extend-p
                                            rectangle-p)
      "Select a region of text using the mouse with mouse event EVENT.

    Allow for stacking of regions, if ALLOW-EXTEND-P is non-nil.

    Create a rectangular region, if RECTANGLE-P is non-nil.

    Stores the region in `pdf-view-active-region'."
      (interactive "@e")
      (setq pdf-view--have-rectangle-region rectangle-p)
      (unless (and (eventp event)
                   (mouse-event-p event))
        (signal 'wrong-type-argument (list 'mouse-event-p event)))
      (unless (and allow-extend-p
                   (or (null (get this-command 'pdf-view-region-window))
                       (equal (get this-command 'pdf-view-region-window)
                              (selected-window))))
        (pdf-view-deactivate-region))
      (put this-command 'pdf-view-region-window
           (selected-window))
      (let* ((window (selected-window))
             (pos (event-start event))
             (begin-inside-image-p t)
             (begin (if (posn-image pos)
                        (posn-object-x-y pos)
                      (setq begin-inside-image-p nil)
                      (posn-x-y pos)))
             (abs-begin (posn-x-y pos))
             pdf-view-continuous
             region)
        (when (pdf-util-track-mouse-dragging (event 0.001)
                (let* ((pos (event-start event))
                       (end (posn-object-x-y pos))
                       (end-inside-image-p
                        (and (eq window (posn-window pos))
                             (posn-image pos))))
                  (when (or end-inside-image-p
                            begin-inside-image-p)
                    (cond
                     ((and end-inside-image-p
                           (not begin-inside-image-p))
                      ;; Started selection outside the image, setup begin.
                      (let* ((xy (posn-x-y pos))
                             (dxy (cons (- (car xy) (car begin))
                                        (- (cdr xy) (cdr begin))))
                             (size (pdf-view-image-size t)))
                        (setq begin (cons (max 0 (min (car size)
                                                      (- (car end) (car dxy))))
                                          (max 0 (min (cdr size)
                                                      (- (cdr end) (cdr dxy)))))
                              ;; Store absolute position for later.
                              abs-begin (cons (- (car xy)
                                                 (- (car end)
                                                    (car begin)))
                                              (- (cdr xy)
                                                 (- (cdr end)
                                                    (cdr begin))))
                              begin-inside-image-p t)))
                     ((and begin-inside-image-p
                           (not end-inside-image-p))
                      ;; Moved outside the image, setup end.
                      (let* ((xy (posn-x-y pos))
                             (dxy (cons (- (car xy) (car abs-begin))
                                        (- (cdr xy) (cdr abs-begin))))
                             (size (pdf-view-image-size t)))
                        (setq end (cons (max 0 (min (car size)
                                                    (+ (car begin) (car dxy))))
                                        (max 0 (min (cdr size)
                                                    (+ (cdr begin) (cdr dxy)))))))))
                    (let ((iregion (if rectangle-p
                                       (list (min (car begin) (car end))
                                             (min (cdr begin) (cdr end))
                                             (max (car begin) (car end))
                                             (max (cdr begin) (cdr end)))
                                     (list (car begin) (cdr begin)
                                           (car end) (cdr end)))))
                      (setq region
                            (pdf-util-scale-pixel-to-relative iregion))
                      (pdf-view-display-region
                       (cons region pdf-view-active-region)
                       rectangle-p)
                      (pdf-util-scroll-to-edges iregion)))))
          (setq pdf-view-active-region
                (append pdf-view-active-region
                        (list region)))
          (pdf-view--push-mark))))
))

(defun zilongshanren-misc/init-pdfgrep ()
  (use-package pdfgrep
    :after pdf-tools
    :config
    (pdfgrep-mode)
    ))

(defun zilongshanren-misc/post-init-counsel ()
  (let ((command
         (cond
          ((executable-find "rg")
           "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
          ((executable-find "pt")
           "pt -zS --nocolor --nogroup -e %s")
          (t counsel-grep-base-command))))
    (setq counsel-grep-base-command command))
  )

(defun zilongshanren-misc/init-bm ()
  (use-package bm
    :ensure t
    :init
    ;; restore on load (even before you require bm)
    (setq bm-restore-repository-on-load t)
    (setq bm-annotate-on-create t)
    (setq bm-cycle-all-buffers t)
    (setq bm-repository-file (format "%sbm-repository"
                                     spacemacs-cache-directory))
    ;; save bookmarks
    (setq-default bm-buffer-persistence t)
  :config
    ;; Loading the repository from file when on start up.
    (add-hook 'after-init-hook 'bm-repository-load)

    ;; Saving bookmarks
    (add-hook 'kill-buffer-hook #'bm-buffer-save)

    ;; Saving the repository to file when on exit.
    ;; kill-buffer-hook is not called when Emacs is killed, so we
    ;; must save all bookmarks first.
    (add-hook 'kill-emacs-hook #'(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))

    ;; Restoring bookmarks
    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  ))

(defun zilongshanren-misc/init-awesome-tab ()
  (use-package awesome-tab
    :config
    (setq awesome-tab-height 140)
    (setq awesome-tab-hide-tab-function 'awesome-tab-hide-tab-tab)
    (awesome-tab-mode t)
    ))

(defun zilongshanren-misc/init-inherit-org ()
  ;; use imenu or counsel-outline to jump outline
  (use-package inherit-org
    :after org
    :config
    (with-eval-after-load 'info
      (add-hook 'Info-mode-hook 'inherit-org-mode))

    (with-eval-after-load 'helpful
      (add-hook 'helpful-mode-hook 'inherit-org-mode)
      (evil-define-key '(evilified normal) helpful-mode-map
        (kbd "<tab>") 'org-cycle
        (kbd "gj") 'outline-next-visible-heading
        (kbd "gk") 'outline-previous-visible-heading))
    ))

(defun zilongshanren-misc/init-langtool ()
  (use-package langtool
    :config
    (setq langtool-language-tool-server-jar "~/bin/LanguageTool-4.9/languagetool-server.jar")
    (setq langtool-server-user-arguments '("-p" "8082"))
    (setq langtool-default-language "en")
    ;; (add-hook 'text-mode-hook  (lambda ()
    ;;                              (add-hook 'after-save-hook 'langtool-check nil 'make-it-local)))
    ))

(defun zilongshanren-misc/init-mw-thesaurus ()
  (use-package mw-thesaurus
    :commands  mw-thesaurus-lookup-at-point
    :config
    (add-hook 'mw-thesaurus-mode-hook 'variable-pitch-mode)
    (setq mw-thesaurus--api-key "69fe3e22-45d0-4e53-8b6f-a3ace4b2ce3a")
    (define-key mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
    ))

(defun zilongshanren-misc/init-powerthesaurus ()
  (use-package powerthesaurus
    :commands (powerthesaurus-lookup-word powerthesaurus-lookup-word-at-point
                                          powerthesaurus-lookup-word-dwim)
    ))

(defun zilongshanren-misc/init-evil-snipe ()
  (use-package evil-snipe
    :config
    (evil-snipe-mode +1)
    (evil-snipe-override-mode +1)
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
    (setq evil-snipe-smart-case t)
    (setq evil-snipe-scope 'whole-visible)
    (setq evil-snipe-repeat-scope 'whole-visible)
    ))

(defun zilongshanren-misc/init-meow ()
  (use-package meow
    :config
    (meow-setup)

    (defvar liuyan/mode-now t  "when t, evil-mode is on, when nil, meow mode is on")
    (define-key global-map (kbd "<f7>") 'liuyan/change-mode)
    ))

(defun zilongshanren-misc/init-bbyac ()
  (use-package bbyac
    :commands ( bbyac-expand-symbols bbyac-expand-substring  bbyac-expand-lines)
    ))

(defun zilongshanren-misc/init-browse-kill-ring ()
  (use-package browse-kill-ring))

(defun zilongshanren-misc/init-delete-block ()
  (use-package delete-block
    :commands (delete-block-forward delete-block-backward)
    :init
    ;; bind-key* ensure the keybinding not be overrided by other minor modes
    (bind-key* "M-m" 'delete-block-forward)
    (bind-key* "M-n" 'delete-block-backward)
    ))

(defun zilongshanren-misc/init-grep-dired ()
  (use-package grep-dired
    :commands (grep-dired-dwim grep-dired)
    ))

(defun zilongshanren-misc/init-one-key ()
  (use-package one-key
    :config
    (with-eval-after-load 'thing-edit
      (progn
        (setq one-key-menu-thing-edit-alist
              '(
                ;; Copy.
                (("w" . "Copy Word") . thing-copy-word)
                (("s" . "Copy Symbol") . thing-copy-symbol)
                (("m" . "Copy Email") . thing-copy-email)
                (("f" . "Copy Filename") . thing-copy-filename)
                (("u" . "Copy URL") . thing-copy-url)
                (("x" . "Copy Sexp") . thing-copy-sexp)
                (("g" . "Copy Page") . thing-copy-page)
                (("t" . "Copy Sentence") . thing-copy-sentence)
                (("o" . "Copy Whitespace") . thing-copy-whitespace)
                (("i" . "Copy List") . thing-copy-list)
                (("c" . "Copy Comment") . thing-copy-comment)
                (("h" . "Copy Function") . thing-copy-defun)
                (("p" . "Copy Parentheses") . thing-copy-parentheses)
                (("l" . "Copy Line") . thing-copy-line)
                (("a" . "Copy To Line Begin") . thing-copy-to-line-beginning)
                (("e" . "Copy To Line End") . thing-copy-to-line-end)
                (("b" . "Copy Paragraph") . thing-copy-paragraph)
                (("n" . "Copy number") . thing-copy-number)
                ;; Cut.
                (("W" . "Cut Word") . thing-cut-word)
                (("S" . "Cut Symbol") . thing-cut-symbol)
                (("M" . "Cut Email") . thing-cut-email)
                (("F" . "Cut Filename") . thing-cut-filename)
                (("U" . "Cut URL") . thing-cut-url)
                (("X" . "Cut Sexp") . thing-cut-sexp)
                (("G" . "Cut Page") . thing-cut-page)
                (("T" . "Cut Sentence") . thing-cut-sentence)
                (("O" . "Cut Whitespace") . thing-cut-whitespace)
                (("I" . "Cut List") . thing-cut-list)
                (("C" . "Cut Comment") . thing-cut-comment)
                (("H" . "Cut Function") . thing-cut-defun)
                (("P" . "Cut Parentheses") . thing-cut-parentheses)
                (("L" . "Cut Line") . thing-cut-line)
                (("A" . "Cut To Line Begin") . thing-cut-to-line-beginning)
                (("E" . "Cut To Line End") . thing-cut-to-line-end)
                (("B" . "Cut Paragraph") . thing-cut-paragraph)
                (("N" . "Cut number") . thing-cut-number)
                ))

        (setq one-key-menu-thing-edit-replace-alist
              '(
                ;; Copy.
                (("w" . "Replace Word") . thing-replace-word)
                (("s" . "Replace Symbol") . thing-replace-symbol)
                (("m" . "Replace Email") . thing-replace-email)
                (("f" . "Replace Filename") . thing-replace-filename)
                (("u" . "Replace URL") . thing-replace-url)
                (("x" . "Replace Sexp") . thing-replace-sexp)
                (("g" . "Replace Page") . thing-replace-page)
                (("t" . "Replace Sentence") . thing-replace-sentence)
                (("o" . "Replace Whitespace") . thing-replace-whitespace)
                (("i" . "Replace List") . thing-replace-list)
                (("h" . "Replace Function") . thing-replace-defun)
                (("p" . "Replace Parentheses") . thing-replace-parentheses)
                (("l" . "Replace Line") . thing-replace-line)
                (("b" . "Replace Paragraph") . thing-replace-paragraph)
                (("n" . "Replace number") . thing-replace-number)
                (("r" . "Replace region/line") . thing-replace-region-or-line)
                ))

        (setq one-key-menu-avy-thing-edit-alist
              '(
                ;; Copy.
                (("w" . "Copy Word") . avy-thing-copy-word)
                (("s" . "Copy Symbol") . avy-thing-copy-symbol)
                (("m" . "Copy Email") . avy-thing-copy-email)
                (("f" . "Copy Filename") . avy-thing-copy-filename)
                (("u" . "Copy URL") . avy-thing-copy-url)
                (("x" . "Copy Sexp") . avy-thing-copy-sexp)
                (("g" . "Copy Page") . avy-thing-copy-page)
                (("t" . "Copy Sentence") . avy-thing-copy-sentence)
                (("o" . "Copy Whitespace") . avy-thing-copy-whitespace)
                (("i" . "Copy List") . avy-thing-copy-list)
                (("c" . "Copy Comment") . avy-thing-copy-comment)
                (("h" . "Copy Function") . avy-thing-copy-defun)
                (("p" . "Copy Parentheses") . avy-thing-copy-parentheses)
                (("l" . "Copy Line") . avy-thing-copy-line)
                (("a" . "Copy To Line Begin") . avy-thing-copy-to-line-beginning)
                (("e" . "Copy To Line End") . avy-thing-copy-to-line-end)
                (("b" . "Copy Paragraph") . avy-thing-copy-paragraph)
                (("n" . "Copy number") . avy-thing-copy-number)
                ;; Cut.
                (("W" . "Cut Word") . avy-thing-cut-word)
                (("S" . "Cut Symbol") . avy-thing-cut-symbol)
                (("M" . "Cut Email") . avy-thing-cut-email)
                (("F" . "Cut Filename") . avy-thing-cut-filename)
                (("U" . "Cut URL") . avy-thing-cut-url)
                (("X" . "Cut Sexp") . avy-thing-cut-sexp)
                (("G" . "Cut Page") . avy-thing-cut-page)
                (("T" . "Cut Sentence") . avy-thing-cut-sentence)
                (("O" . "Cut Whitespace") . avy-thing-cut-whitespace)
                (("I" . "Cut List") . avy-thing-cut-list)
                (("C" . "Cut Comment") . avy-thing-cut-comment)
                (("H" . "Cut Function") . avy-thing-cut-defun)
                (("P" . "Cut Parentheses") . avy-thing-cut-parentheses)
                (("L" . "Cut Line") . avy-thing-cut-line)
                (("A" . "Cut To Line Begin") . avy-thing-cut-to-line-beginning)
                (("E" . "Cut To Line End") . avy-thing-cut-to-line-end)
                (("B" . "Cut Paragraph") . avy-thing-cut-paragraph)
                (("N" . "Cut number") . avy-thing-cut-number)
                ))

        (setq one-key-menu-avy-thing-edit-replace-alist
              '(
                ;; Copy.
                (("w" . "Replace Word") . avy-thing-replace-word)
                (("s" . "Replace Symbol") . avy-thing-replace-symbol)
                (("m" . "Replace Email") . avy-thing-replace-email)
                (("f" . "Replace Filename") . avy-thing-replace-filename)
                (("u" . "Replace URL") . avy-thing-replace-url)
                (("x" . "Replace Sexp") . avy-thing-replace-sexp)
                (("g" . "Replace Page") . avy-thing-replace-page)
                (("t" . "Replace Sentence") . avy-thing-replace-sentence)
                (("o" . "Replace Whitespace") . avy-thing-replace-whitespace)
                (("i" . "Replace List") . avy-thing-replace-list)
                (("h" . "Replace Function") . avy-thing-replace-defun)
                (("p" . "Replace Parentheses") . avy-thing-replace-parentheses)
                (("l" . "Replace Line") . avy-thing-replace-line)
                (("b" . "Replace Paragraph") . avy-thing-replace-paragraph)
                (("n" . "Replace number") . avy-thing-replace-number)
                (("r" . "Replace region/line") . avy-thing-replace-region-or-line)
                ))

        (defun one-key-menu-thing-edit ()
          "The `one-key' menu for THING-EDIT."
          (interactive)
          (one-key-menu "THING-EDIT" one-key-menu-thing-edit-alist t))

        (defun one-key-menu-thing-edit-replace ()
          "The `one-key' menu for THING-EDIT."
          (interactive)
          (one-key-menu "THING-EDIT" one-key-menu-thing-edit-replace-alist t))

        (defun one-key-menu-avy-thing-edit ()
          "The `one-key' menu for THING-EDIT."
          (interactive)
          (one-key-menu "THING-EDIT" one-key-menu-avy-thing-edit-alist t))

        (defun one-key-menu-avy-thing-edit-replace ()
          "The `one-key' menu for THING-EDIT."
          (interactive)
          (one-key-menu "THING-EDIT" one-key-menu-avy-thing-edit-replace-alist t))
        ))))

(defun zilongshanren-misc/init-thing-edit ()
  (use-package thing-edit))

(defun zilongshanren-misc/init-avy-thing-edit ()
  (use-package avy-thing-edit
    :config
    (setq avy-thing-edit-jump-command 'evil-avy-goto-char)
    ))

(defun zilongshanren-misc/init-figlet ()
  (use-package figlet
    :defer t))

(defun zilongshanren-misc/init-try ()
  (use-package try
    :ensure t
    :defer t
    ))

(defun zilongshanren-misc/init-sis ()
  (use-package sis
    ;; :hook
    ;; enable the /follow context/ and /inline region/ mode for specific buffers
    ;; (((text-mode prog-mode) . sis-context-mode)
    ;;  ((text-mode prog-mode) . sis-inline-mode))

    :config
    (sis-ism-lazyman-config "1" "2" 'fcitx5)

    ;; enable the /respect/ mode
    (sis-global-respect-mode t)
    ;; enable the /follow context/ mode for all buffers
    (sis-global-context-mode t)
    ;; enable the /inline english/ mode for all buffers
    (sis-global-inline-mode t)
    (setq sis-inline-with-other	t)
    (setq sis-english-pattern "[a-zA-Z!@#$%^&*()-=_+,./<>?;':\"|\\]")
    (setq sis-inline-single-space-close t)

    (defun netease-detector (&rest _)
      (let ((name (buffer-name)))
        (if (string-prefix-p "*Netease" name)
            'english)
        ))

    (add-to-list 'sis-context-detectors
                 'netease-detector)
    ))

(defun zilongshanren-misc/init-rime ()
  (use-package rime
    :config
    (setq rime-user-data-dir "~/.local/share/fcitx5/rime")

    (setq rime-posframe-properties
          (list :background-color "#333333"
                :foreground-color "#dcdccc"
                :font "WenQuanYi Micro Hei Mono-14"
                :internal-border-width 10))

    (setq default-input-method "rime"
          rime-show-candidate 'posframe)

    (setq rime-disable-predicates
          '(rime-predicate-evil-mode-p
            rime-predicate-after-alphabet-char-p
            rime-predicate-prog-in-code-p
            rime-predicate-punctuation-after-ascii-p
            rime-predicate-punctuation-after-space-cc-p
            rime-predicate-current-input-punctuation-p
            rime-predicate-ace-window-p
            rime-predicate-hydra-p
            rime-predicate-tex-math-or-command-p
            ))

    ;; 使用 return 推出 inline ascii english
    (setq rime-inline-predicates
          '(rime-predicate-space-after-cc-p
            rime-predicate-current-uppercase-letter-p))

    (setq rime-inline-ascii-trigger 'shift-l)

    ;; (setq rime-show-candidate nil)
    ))

(defun zilongshanren-misc/post-init-posframe ()
  (progn
    (setq posframe-arghandler #'my-posframe-arghandler)
    (defun my-posframe-arghandler (buffer-or-name arg-name value)
      (let ((info '(:internal-border-width 2 :background-color "#5e5079" :foreground-color "#b2b2b2")))
        (or (plist-get info arg-name) value)))
    (setq posframe-mouse-banish nil)
    ))

(defun zilongshanren-misc/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    :config
    (progn
      ;; Enable Cache
      (setq url-automatic-caching t
            ;; Set file path for saving search history
            youdao-dictionary-search-history-file
            (concat spacemacs-cache-directory ".youdao")
            ;; Enable Chinese word segmentation support
            youdao-dictionary-use-chinese-word-segmentation t)

      (define-key youdao-dictionary-mode-map
        (kbd "s") #'youdao-dictionary-search-from-input)
      )))

(defun zilongshanren-misc/init-go-translate ()
  (use-package go-translate
    :commands go-translate go-translate-popup go-translate-kill-ring-save
    :config
    (setq go-translate-base-url "https://translate.google.cn")
    (setq go-translate-buffer-follow-p t)
    (setq go-translate-local-language "zh-CN")
    (setq go-translate-target-language "en")
    (setq go-translate-inputs-function #'go-translate-inputs-current-or-prompt)

    (defun go-translate-token--extract-tkk ()
      (cons 430675 2721866130))
    ))

(defun zilongshanren-misc/init-speed-type ()
  (use-package speed-type
    :commands (speed-type-text)))

(defun zilongshanren-misc/post-init-zone ()
  (use-package zone
    :if (display-graphic-p)
    :config
    (zone-when-idle 600)                ; in seconds
    (defun zone-choose (pgm)
      "Choose a PGM to run for `zone'."
      (interactive
       (list
        (completing-read
         "Program: "
         (mapcar 'symbol-name zone-programs))))
      (let ((zone-programs (list (intern pgm))))
        (zone)))))

(defun zilongshanren-misc/post-init-git-gutter ()
  (setq git-gutter:modified-sign "=")
  )

(defun zilongshanren-misc/post-init-terminal-here ()
  (progn
    (setq terminal-here-terminal-command (list "st"))
    ))

(defun zilongshanren-misc/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode 1)
      (keyfreq-autosave-mode 1)
      )))

(defun zilongshanren-misc/post-init-popup ()
  (use-package popup
    :config
    (progn
      (define-key popup-menu-keymap (kbd "C-j") 'popup-next)
      (define-key popup-menu-keymap (kbd "C-k") 'popup-previous)
      )))

(defun zilongshanren-misc/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (when (configuration-layer/package-used-p 'helm-ag)
      (defadvice er/prepare-for-more-expansions-internal
          (around helm-ag/prepare-for-more-expansions-internal activate)
        ad-do-it
        (let ((new-msg (concat (car ad-return-value)
                               ", H to highlight in buffers"
                               ", / to search in project, "
                               "f to search in files, "
                               "b to search in opened buffers"))
              (new-bindings (cdr ad-return-value)))
          (cl-pushnew
           '("H" (lambda ()
                   (call-interactively
                    'zilongshanren/highlight-dwim)))
           new-bindings)
          (cl-pushnew
           '("/" (lambda ()
                   (call-interactively
                    'spacemacs/helm-project-smart-do-search-region-or-symbol)))
           new-bindings)
          (cl-pushnew
           '("f" (lambda ()
                   (call-interactively
                    'spacemacs/helm-files-smart-do-search-region-or-symbol)))
           new-bindings)
          (cl-pushnew
           '("b" (lambda ()
                   (call-interactively
                    'spacemacs/helm-buffers-smart-do-search-region-or-symbol)))
           new-bindings)
          (setq ad-return-value (cons new-msg new-bindings)))))))

(defun zilongshanren-misc/init-highlight-global ()
  (use-package highlight-global
    :init
    (progn
      (setq-default highlight-faces
        '(('hi-red-b . 0)
          ('hi-yellow . 0)
          ('hi-pink . 0)
          ('hi-blue-b . 0))))))

(defun zilongshanren-misc/post-init-symbol-overlay ()
  (with-eval-after-load 'symbol-overlay
    (progn
      (spacemacs/transient-state-register-add-bindings 'symbol-overlay
      '((">" symbol-overlay-jump-last)
        ("s" spacemacs/swiper-region-or-symbol)
        ("<" symbol-overlay-jump-first))))))

(defun zilongshanren-misc/post-init-golden-ratio ()
  (with-eval-after-load 'golden-ratio
    (dolist (mode '("dired-mode" "occur-mode"))
      (add-to-list 'golden-ratio-exclude-modes mode))
    (dolist (n '("COMMIT_EDITMSG"))
      (add-to-list 'golden-ratio-exclude-buffer-names n))))

(defun zilongshanren-misc/post-init-ranger ()
  ;; https://emacs-china.org/t/ranger-golden-ratio/964/2
  (defun my-ranger ()
    (interactive)
    (if golden-ratio-mode
        (progn
          (golden-ratio-mode -1)
          (ranger)
          (setq golden-ratio-previous-enable t))
      (progn
        (ranger)
        (setq golden-ratio-previous-enable nil))))

  (defun my-quit-ranger ()
    (interactive)
    (if golden-ratio-previous-enable
        (progn
          (ranger-close)
          (golden-ratio-mode 1))
      (ranger-close)))

  (with-eval-after-load 'ranger
    (progn
      (setq golden-ratio-previous-enable nil)
      (define-key ranger-normal-mode-map (kbd "q") 'my-quit-ranger)
      (setq ranger-show-literal nil)
      ))

  (spacemacs/set-leader-keys "atrr" 'my-ranger)
  )

;; copy from spacemacs helm layer
(defun zilongshanren-misc/init-helm-ag ()
  (use-package helm-ag
    :defer t
    :init
    (progn
      (defun spacemacs//helm-do-ag-region-or-symbol (func &optional dir)
        "Search with `ag' with a default input."
        (require 'helm-ag)
        (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
                   ;; make thing-at-point choosing the active region first
                   ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
                   ((symbol-function 'thing-at-point)
                    (lambda (thing)
                      (let ((res (if (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))
                                   (this-fn thing))))
                        (when res (rxt-quote-pcre res))))))
          (funcall func dir)))

      (defun spacemacs//helm-do-search-find-tool (base tools default-inputp)
        "Create a cond form given a TOOLS string list and evaluate it."
        (eval
         `(cond
           ,@(mapcar
              (lambda (x)
                `((executable-find ,x)
                  ',(let ((func
                           (intern
                            (format (if default-inputp
                                        "spacemacs/%s-%s-region-or-symbol"
                                      "spacemacs/%s-%s")
                                    base x))))
                      (if (fboundp func)
                          func
                        (intern (format "%s-%s"  base x))))))
              tools)
           (t 'helm-do-grep))))

      ;; Search in current file ----------------------------------------------

      (defun spacemacs/helm-file-do-ag (&optional _)
        "Wrapper to execute `helm-ag-this-file.'"
        (interactive)
        (helm-ag-this-file))

      (defun spacemacs/helm-file-do-ag-region-or-symbol ()
        "Search in current file with `ag' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-file-do-ag))

      (defun spacemacs/helm-file-smart-do-search (&optional default-inputp)
        "Search in current file using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-file-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-file-smart-do-search-region-or-symbol ()
        "Search in current file using `dotspacemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-file-smart-do-search t))

      ;; Search in files -----------------------------------------------------

      (defun spacemacs/helm-files-do-ag (&optional dir)
        "Search in files with `ag' using a default input."
        (interactive)
        (helm-do-ag dir))

      (defun spacemacs/helm-files-do-ag-region-or-symbol ()
        "Search in files with `ag' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ag))

      (defun spacemacs/helm-files-do-ack (&optional dir)
        "Search in files with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun spacemacs/helm-files-do-ack-region-or-symbol ()
        "Search in files with `ack' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ack))

      (defun spacemacs/helm-files-do-pt (&optional dir)
        "Search in files with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun spacemacs/helm-files-do-pt-region-or-symbol ()
        "Search in files with `pt' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-pt))

      (defun spacemacs/helm-files-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-files-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-files-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools'.
with default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-files-smart-do-search t))

      ;; Search in buffers ---------------------------------------------------

      (defun spacemacs/helm-buffers-do-ag (&optional _)
        "Wrapper to execute `helm-ag-buffers.'"
        (interactive)
        (helm-do-ag-buffers))

      (defun spacemacs/helm-buffers-do-ag-region-or-symbol ()
        "Search in opened buffers with `ag' with a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ag))

      (defun spacemacs/helm-buffers-do-ack (&optional _)
        "Search in opened buffers with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun spacemacs/helm-buffers-do-ack-region-or-symbol ()
        "Search in opened buffers with `ack' with a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ack))

      (defun spacemacs/helm-buffers-do-pt (&optional _)
        "Search in opened buffers with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun spacemacs/helm-buffers-do-pt-region-or-symbol ()
        "Search in opened buffers with `pt' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-pt))

      (defun spacemacs/helm-buffers-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-buffers-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-buffers-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools' with
default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-buffers-smart-do-search t))

      ;; Search in project ---------------------------------------------------

      (defun spacemacs/helm-project-do-ag ()
        "Search in current project with `ag'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ag-region-or-symbol ()
        "Search in current project with `ag' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol 'helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ack ()
        "Search in current project with `ack'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs/helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ack-region-or-symbol ()
        "Search in current project with `ack' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol
               'spacemacs/helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-pt ()
        "Search in current project with `pt'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs/helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-pt-region-or-symbol ()
        "Search in current project with `pt' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol
               'spacemacs/helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-smart-do-search (&optional default-inputp)
        "Search in current project using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (let ((projectile-require-project-root nil))
          (call-interactively
           (spacemacs//helm-do-search-find-tool "helm-project-do"
                                                dotspacemacs-search-tools
                                                default-inputp))))

      (defun spacemacs/helm-project-smart-do-search-region-or-symbol ()
        "Search in current project using `dotspacemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-project-smart-do-search t))

      ;; This overrides the default C-s action in helm-projectile-switch-project
      ;; to search using ag/pt/whatever instead of just grep
      (with-eval-after-load 'helm-projectile
        (defun spacemacs/helm-project-smart-do-search-in-dir (dir)
          (interactive)
          (let ((default-directory dir))
            (spacemacs/helm-project-smart-do-search)))
        (define-key helm-projectile-projects-map
          (kbd "C-s")
          (lambda ()
            (interactive)
            (helm-exit-and-execute-action
             'spacemacs/helm-project-smart-do-search-in-dir))))

      ;; evilify the helm-grep buffer
      (evilified-state-evilify helm-grep-mode helm-grep-mode-map
        (kbd "RET") 'helm-grep-mode-jump-other-window
        (kbd "q") 'quit-window)

      (spacemacs/set-leader-keys
        ;; helm-ag marks
        "s`"  'helm-ag-pop-stack
        ;; opened buffers scope
        "sb"  'spacemacs/helm-buffers-smart-do-search
        "sB"  'spacemacs/helm-buffers-smart-do-search-region-or-symbol
        "sab" 'helm-do-ag-buffers
        "saB" 'spacemacs/helm-buffers-do-ag-region-or-symbol
        "skb" 'spacemacs/helm-buffers-do-ack
        "skB" 'spacemacs/helm-buffers-do-ack-region-or-symbol
        "stb" 'spacemacs/helm-buffers-do-pt
        "stB" 'spacemacs/helm-buffers-do-pt-region-or-symbol
        ;; current file scope
        "ss"  'spacemacs/helm-file-smart-do-search
        "sS"  'spacemacs/helm-file-smart-do-search-region-or-symbol
        "saa" 'helm-ag-this-file
        "saA" 'spacemacs/helm-file-do-ag-region-or-symbol
        ;; files scope
        "sf"  'spacemacs/helm-files-smart-do-search
        "sF"  'spacemacs/helm-files-smart-do-search-region-or-symbol
        "saf" 'helm-do-ag
        "saF" 'spacemacs/helm-files-do-ag-region-or-symbol
        "skf" 'spacemacs/helm-files-do-ack
        "skF" 'spacemacs/helm-files-do-ack-region-or-symbol
        "stf" 'spacemacs/helm-files-do-pt
        "stF" 'spacemacs/helm-files-do-pt-region-or-symbol
        ;; current project scope
        "/"   'spacemacs/helm-project-smart-do-search
        "*"   'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sp"  'spacemacs/helm-project-smart-do-search
        "sP"  'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sap" 'spacemacs/helm-project-do-ag
        "saP" 'spacemacs/helm-project-do-ag-region-or-symbol
        "skp" 'spacemacs/helm-project-do-ack
        "skP" 'spacemacs/helm-project-do-ack-region-or-symbol
        "stp" 'spacemacs/helm-project-do-pt
        "stP" 'spacemacs/helm-project-do-pt-region-or-symbol))
    :config
    (progn
      (advice-add 'helm-ag--save-results :after 'spacemacs//gne-init-helm-ag)
      (evil-define-key 'normal helm-ag-map "SPC" spacemacs-default-map)
      (evilified-state-evilify helm-ag-mode helm-ag-mode-map
        (kbd "RET") 'helm-ag-mode-jump-other-window
        (kbd "gr") 'helm-ag--update-save-results
        (kbd "q") 'quit-window))))

(defun zilongshanren-misc/post-init-hydra ()
  (progn
    (defhydra
      hydra-apropos (:color blue)
      "Apropos"
      ("a" apropos "apropos")
      ("c" apropos-command "cmd")
      ("d" apropos-documentation "doc")
      ("e" apropos-value "val")
      ("l" apropos-library "lib")
      ("o" apropos-user-option "option")
      ("u" apropos-user-option "option")
      ("v" apropos-variable "var")
      ("i" info-apropos "info")
      ("t" tags-apropos "tags")
      ("z" hydra-customize-apropos/body "customize"))

    (defhydra
      hydra-customize-apropos (:color blue)
      "Apropos (customize)"
      ("a" customize-apropos "apropos")
      ("f" customize-apropos-faces "faces")
      ("g" customize-apropos-groups "groups")
      ("o" customize-apropos-options "options"))

    (define-key global-map (kbd "<f1>") 'hydra-hotspots/body)
    (spacemacs/set-leader-keys "oo" 'hydra-hotspots/body)
    ;; (bind-key*  "<f4>" 'hydra-apropos/body)
    (spacemacs/set-leader-keys "oH" 'hydra-apropos/body)

    ))

(defun zilongshanren-misc/post-init-gist ()
  (use-package gist
    :defer t
    :init
    (setq gist-list-format
          '((files "File" 30 nil "%s")
            (id "Id" 10 nil identity)
            (created "Created" 20 nil "%D %R")
            (visibility "Visibility" 10 nil
                        (lambda
                          (public)
                          (or
                           (and public "public")
                           "private")))
            (description "Description" 0 nil identity)))
    :config
    (progn
      (spacemacs|define-transient-state gist-list-mode
        :title "Gist-mode Transient State"
        :bindings
        ("k" gist-kill-current "delete gist")
        ("e" gist-edit-current-description "edit gist title")
        ("+" gist-add-buffer "add a file")
        ("-" gist-remove-file "delete a file")
        ("y" gist-print-current-url "print url")
        ("b" gist-browse-current-url "browse gist in browser")
        ("*" gist-star "star gist")
        ("^" gist-unstar "unstar gist")
        ("f" gist-fork "fork gist")
        ("q" nil "quit" :exit t)
        ("<escape>" nil nil :exit t))
      (spacemacs/set-leader-keys-for-major-mode 'gist-list-mode
        "." 'spacemacs/gist-list-mode-transient-state/body))
    ))

(defun zilongshanren-misc/init-tiny ()
  (use-package tiny
    :commands tiny-expand
    ))

(defun zilongshanren-misc/init-litable ()
  (use-package litable
    :init
    :defer t))

(defun zilongshanren-misc/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map)
      )))

(defun zilongshanren-misc/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    ;; disable highlight when use swiper or evil ex search, this option won't effect evil-ex-search-next command
    (setq-default evil-ex-search-persistent-highlight nil)

    (push "TAGS" spacemacs-useless-buffers-regexp)

    (adjust-major-mode-keymap-with-evil "git-timemachine")
    (adjust-major-mode-keymap-with-evil "tabulated-list")

    (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)
    (define-key evil-insert-state-map (kbd "C-r") 'evil-paste-from-register)

    ;; ;; change evil initial mode state
    (loop for (mode . state) in
          '((shell-mode . normal)
            (minibuffer-inactive-mode . emacs))
          do (evil-set-initial-state mode state))

    ;;mimic "nzz" behaviou in vim
    (defadvice evil-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

    (defun my-evil-yank ()
      (interactive)
      (save-excursion
        (call-interactively 'evil-yank))
      (backward-char))

    (define-key evil-visual-state-map (kbd "y") 'my-evil-yank)

    (evil-define-key '(normal visual) 'global-map (kbd "] f") 'end-of-defun)
    (evil-define-key '(normal visual) 'global-map (kbd "[ f") 'beginning-of-defun)
    (evil-define-key '(normal visual) 'global-map (kbd "] s") 'forward-sexp)
    (evil-define-key '(normal visual) 'global-map (kbd "[ s") 'backward-sexp)
    (evil-define-key '(normal visual) 'global-map (kbd "[ u") 'backward-up-list)
    (evil-define-key '(normal visual) 'global-map (kbd "] u") 'backward-up-list)

    (define-key evil-normal-state-map (kbd "M-y") 'counsel-yank-pop)
    (define-key evil-normal-state-map (kbd "g f") 'find-file-in-project-at-point)
    (define-key evil-normal-state-map (kbd "g F") 'find-file-at-point)
    (define-key evil-normal-state-map (kbd "g r") 'xref-find-references)
    (define-key evil-normal-state-map (kbd "g R") 'lsp-ui-peek-find-references)

    (spacemacs/set-leader-keys "bi" 'ibuffer-other-window)
    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)
    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)

    ;; in spacemacs, we always use evilify miscro state
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
    ;; Don't move back the cursor one position when exiting insert mode
    (setq evil-move-cursor-back nil)

    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)

    (evil-define-key 'emacs term-raw-map (kbd "C-w")
      'evil-delete-backward-word)

    (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
          evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
          evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
          evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
          evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
          evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))
    (setq evil-insert-state-cursor '("chartreuse3" box))
    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)

    (evil-define-key 'normal 'global-map (kbd "zy") 'thing-copy-symbol)
    ))

(defun zilongshanren-misc/init-visual-regexp ()
  (use-package visual-regexp
    :commands (vr/replace vr/query-replace)))

(defun zilongshanren-misc/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :commands (vr/select-replace vr/select-query-replace)
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace))))

(defun zilongshanren-misc/init-multiple-cursors ()
  (use-package multiple-cursors
    :config
    (setq mc/always-run-for-all t)
    (with-eval-after-load 'multiple-cursors-core
      (add-to-list 'mc/cmds-to-run-once 'spacemacs/helm-M-x-fuzzy-matching)
      (add-to-list 'mc/cmds-to-run-once 'counsel-M-x)
      (add-to-list 'mc/cmds-to-run-once 'spacemacs/default-pop-shell))

    (evil-define-key '(normal visual) 'global-map (kbd "mn") 'mc/mark-next-like-this)
    (evil-define-key '(normal visual) 'global-map (kbd "mp") 'mc/mark-previous-like-this)
    (evil-define-key '(normal visual) 'global-map (kbd "mN") 'mc/skip-to-next-like-this)
    (evil-define-key '(normal visual) 'global-map (kbd "mP") 'mc/skip-to-previous-like-this)
    (evil-define-key '(normal visual) 'global-map (kbd "mu") 'mc/unmark-next-like-this)
    (evil-define-key '(normal visual) 'global-map (kbd "mU") 'mc/unmark-previous-like-this)
    (evil-define-key '(normal visual) 'global-map (kbd "mm") 'mc/mark-more-like-this-extended)
    (evil-define-key '(normal visual) 'global-map (kbd "me") 'mc/edit-beginnings-of-lines)
    (evil-define-key '(normal visual) 'global-map (kbd "mE") 'mc/edit-ends-of-lines)
    (evil-define-key '(normal visual) 'global-map (kbd "ma") 'mc/mark-all-dwim)
    (evil-define-key '(normal visual) 'global-map (kbd "mA") 'mc/mark-all-like-this)
    (evil-define-key '(normal visual) 'global-map (kbd "mr") 'mc/mark-all-in-region)
    (evil-define-key '(normal visual) 'global-map (kbd "md") 'mc/mark-all-like-this-in-defun)
    (evil-define-key '(normal visual) 'global-map (kbd "mR") 'set-rectangular-region-anchor)
    (evil-define-key '(normal visual) 'global-map (kbd "mt") 'mc/mark-sgml-tag-pair)
    (evil-define-key '(normal visual) 'global-map (kbd "mi") 'mc/insert-numbers)
    (evil-define-key '(normal visual) 'global-map (kbd "mI") 'mc/insert-letters)
    (evil-define-key '(normal visual) 'global-map (kbd "ms") 'mc/sort-regions)
    (evil-define-key '(normal visual) 'global-map (kbd "mV") 'mc/reverse-regions)
    ))


(defun zilongshanren-misc/post-init-evil-escape ()
  (setq evil-escape-delay 0.2))

(defun zilongshanren-misc/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :config
    (progn
      ;; If you use other VCS (subversion, for example), enable the following option
      ;;(setq ffip-project-file ".svn")
      ;; in MacOS X, the search file command is CMD+p
      ;; for this project, I'm only interested certain types of files
      (setq-default ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.cpp" "*.h" "*.c" "*.mm" "*.m" "*.el"))
      ;; if the full path of current file is under SUBPROJECT1 or SUBPROJECT2
      ;; OR if I'm reading my personal issue track document,
      (defadvice find-file-in-project (before my-find-file-in-project activate compile)
        (when (ffip-current-full-filename-match-pattern-p "\\(HLMJ_js\\)")
          ;; set the root directory into "~/projs/PROJECT_DIR"
          (setq-local ffip-project-root "~/Github/HLMJ_js")
          ;; well, I'm not interested in concatenated BIG js file or file in dist/
          (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
          ;; do NOT search files in below directories, the default value is better.
          (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "temp"))))
      (ad-activate 'find-file-in-project))))

(defun zilongshanren-misc/post-init-projectile ()
  (progn
    (with-eval-after-load 'projectile
      (progn
        (setq projectile-completion-system 'ivy)
        (add-to-list 'projectile-other-file-alist '("html" "js"))
        (add-to-list 'projectile-other-file-alist '("js" "html"))))

    (spacemacs/set-leader-keys "pf" 'zilongshanren/open-file-with-projectile-or-counsel-git)
    ))

(defun zilongshanren-misc/init-moz-controller ()
  (use-package moz-controller
    :init
    (progn
      (moz-controller-global-mode t)
      (spacemacs|hide-lighter moz-controller-mode))))


(defun zilongshanren-misc/init-ag ()
  (use-package ag
    :init))

(defun zilongshanren-misc/post-init-erc ()
  (progn
    (add-hook 'erc-text-matched-hook 'my-erc-hook)
    (spaceline-toggle-erc-track-off)))

(defun zilongshanren-misc/init-wrap-region ()
  (use-package wrap-region
    :init
    (progn
      (wrap-region-global-mode t)
      (wrap-region-add-wrappers
       '(("$" "$")
         ("{-" "-}" "#")
         ("/" "/" nil ruby-mode)
         ("/* " " */" "#" (java-mode javascript-mode css-mode js2-mode))
         ("`" "`" nil (markdown-mode ruby-mode))))
      (add-to-list 'wrap-region-except-modes 'dired-mode)
      (add-to-list 'wrap-region-except-modes 'web-mode)
      )
    :defer t
    :config
    (spacemacs|hide-lighter wrap-region-mode)))



(defun zilongshanren-misc/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))

(defun zilongshanren-misc/post-init-swiper ()
  "Initialize my package"
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)

    (evilified-state-evilify ivy-occur-mode ivy-occur-mode-map)

    (use-package ivy
      :defer t
      :config
      (progn
        (spacemacs|hide-lighter ivy-mode)

        (setq ivy-dynamic-exhibit-delay-ms 300)

        (ivy-set-actions
         t
         '(("i" wr/insert-a-head-from-a-file "roam insert head")
           ("f" my-find-file-in-git-repo "find files")
           ("e" my-open-file-in-external-app "Open file in external app")
           ("I" ivy-insert-action "insert")
           ("c" ivy-kill-new-action "copy")
           ("k" ivy--kill-buffer-action "kill")
           ("r" ivy--rename-buffer-action "rename")
           ("s" ivy-ff-checksum-action "Checksum")))

        (spacemacs/set-leader-keys "fad" 'counsel-goto-recent-directory)
        (spacemacs/set-leader-keys "faf" 'counsel-find-file-recent-directory)

        (setq ivy-initial-inputs-alist nil)
        (setq ivy-wrap t)
        (setq confirm-nonexistent-file-or-buffer t)
        (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-previous-line)
        (define-key ivy-switch-buffer-map (kbd "C-d") 'ivy-switch-buffer-kill)

        (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
        (define-key ivy-minibuffer-map (kbd "C-c s") 'ivy-ff-checksum)
        (define-key ivy-minibuffer-map (kbd "s-o") 'ivy-dispatching-done-hydra)
        (define-key ivy-minibuffer-map (kbd "C-c C-e") 'spacemacs//counsel-edit)
        (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-call-and-recenter)
        (define-key ivy-minibuffer-map (kbd "<f3>") 'ivy-occur)
        (define-key ivy-minibuffer-map (kbd "C-c d") 'ivy-immediate-done)
        (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
        (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)))

    (define-key global-map (kbd "C-s") 'my-swiper-search)))


(defun zilongshanren-misc/post-init-magit ()
  (with-eval-after-load 'magit
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defadvice magit-quit-window (after magit-restore-screen activate)
      (jump-to-register :magit-fullscreen))

    ;; speed up magit
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
    ))

(defun zilongshanren-misc/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
      (define-key git-messenger-map (kbd "f") 'zilong/github-browse-commit))))

(defun zilongshanren-misc/post-init-markdown-mode ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

    (with-eval-after-load 'markdown-mode
      (progn
        ;; (when (configuration-layer/package-usedp 'company)
        ;;   (spacemacs|add-company-hook markdown-mode))

        (spacemacs/set-leader-keys-for-major-mode 'gfm-mode-map
          "p" 'zilongshanren/markdown-to-html)
        (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
          "p" 'zilongshanren/markdown-to-html)
        (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
          "it" 'markdown-insert-table)
        (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
          "Tp" 'markdown-live-preview-mode)

        (evil-define-key 'normal markdown-mode-map (kbd "TAB") 'markdown-cycle)

        (setq markdown-asymmetric-header t
              markdown-enable-wiki-links t
              markdown-fontify-code-blocks-natively t
              )
        ))
    ))
