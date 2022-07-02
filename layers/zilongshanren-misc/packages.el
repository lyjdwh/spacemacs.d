;; -*- coding: utf-8 -*-

(setq zilongshanren-misc-packages
      '(
        helm-ag
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
        ranger
        golden-ratio
        (highlight-global :location (recipe :fetcher github :repo "glen-dai/highlight-global"))
        symbol-overlay
        popup
        keyfreq
        terminal-here
        git-gutter
        speed-type
        zone
        youdao-dictionary
        fanyi
        go-translate
        posframe
        ;; rime
        sis
        try
        figlet
        (thing-edit :location (recipe :fetcher github :repo "lyjdwh/thing-edit"))
        (avy-thing-edit :location (recipe :fetcher github :repo "lyjdwh/avy-thing-edit"))
        (one-key :location (recipe :fetcher github :repo "manateelazycat/one-key"))
        (grep-dired :location (recipe :fetcher github :repo "manateelazycat/grep-dired"))
        (delete-block :location (recipe :fetcher github :repo "manateelazycat/delete-block"))
        bbyac
        ;; meow
        powerthesaurus
        mw-thesaurus
        (inherit-org :location (recipe :fetcher github :repo "chenyanming/inherit-org"))
        (awesome-tab :location (recipe :fetcher github :repo "manateelazycat/awesome-tab"))
        bm
        counsel
        pdf-tools
        pdfgrep
        separedit
        pyim
        ;; key-chord
        evil-pinyin
        kaomoji
        (english-teacher :location (recipe :fetcher github :repo "loyalpartner/english-teacher.el"))
        pos-tip
        ;; (maple-header :location (recipe :fetcher github :repo "honmaple/maple-emacs" :files ("site-lisp/maple/maple-header.el")))
        tmux-pane
        wttrin
        ;; bongo
        ;; podcaster
        ;; mingus
        helm-chrome
        elfeed
        (shengci :location (recipe :fetcher github :repo "EvanMeek/shengci.el"))
        (guess-word :location (recipe :fetcher github :repo "Qquanwei/emacs-guess-word-game" :files ("*")))
        evil-matchit
        avy
        telega
        vterm
        (casease :location (recipe :fetcher github :repo "DogLooksGood/casease"))
        mu4e
        org-msg
        (org-media-note :location (recipe :fetcher github :repo "yuchen-lea/org-media-note"))
        netease-cloud-music
        elisp-demos
        forge
        rainbow-delimiters
        xref
        blamer
        auto-package-update
        epkg
        ;; undo-tree
        gcmh
        (screenshot :location (recipe :fetcher github :repo "tecosaur/screenshot"))
        popwin
        ))

(defun zilongshanren-misc/post-init-popwin ()
  (push '("*Flycheck errors*"  :dedicated t :position bottom :stick t :noselect t   :height 0.3) popwin:special-display-config)
  )

(defun zilongshanren-misc/init-screenshot()
  (use-package screenshot
    :init
    (spacemacs/set-leader-keys "atf" 'screenshot)
    :commands screenshot
    ))

(defun zilongshanren-misc/init-gcmh ()
  (use-package gcmh
    :config
    (setq gcmh-idle-delay 'auto  ; default is 15s
          gcmh-auto-idle-delay-factor 10
          gcmh-high-cons-threshold (* 100 1024 1024))
    (gcmh-mode 1)
    ))

(defun zilongshanren-misc/post-init-undo-tree()
  (setq undo-tree-auto-save-history nil)
  )

(defun zilongshanren-misc/init-epkg()
  (use-package epkg
    :defer t
    :commands epkg-describe-package epkg-find-file
    ))

(defun zilongshanren-misc/init-auto-package-update()
  (use-package auto-package-update
    :if (not (daemonp))
    :custom
    (auto-package-update-interval 60) ;; in days
    (auto-package-update-prompt-before-update t)
    (auto-package-update-delete-old-versions t)
    (auto-package-update-hide-results t)
    :config
    (auto-package-update-maybe)))

(defun zilongshanren-misc/init-blamer()
  (use-package blamer
    :commands global-blamer-mode
    :init
    (defun blamer-callback-show-commit-diff (commit-info)
      (interactive)
      (let ((commit-hash (plist-get commit-info :commit-hash)))
        (when commit-hash
          (magit-show-commit commit-hash))))

    (defun blamer-callback-magit-log-file (commit-info)
      (interactive)
      (magit-log-buffer-file)
      (let ((commit-hash (plist-get commit-info :commit-hash)))
        (when commit-hash
          (run-with-idle-timer 1 nil (lambda (commit-hash)
                                       (goto-char (point-min))
                                       (search-forward (substring commit-hash 0 7))
                                       (set-mark (point-at-bol))
                                       (goto-char (point-at-eol)))
                               commit-hash))))

    (spacemacs/set-leader-keys "otg" 'global-blamer-mode)
    (spacemacs/set-leader-keys "gb" 'global-blamer-mode)
    (spacemacs/set-leader-keys "gB" 'spacemacs/git-blame-transient-state)
    :custom
    (blamer-idle-time 0.3)
    (blamer-min-offset 70)

    (setq blamer-bindings '(("<mouse-3>" . blamer-callback-magit-log-file)
                            ("<mouse-1>" . blamer-callback-show-commit-diff)))
    ))

(defun zilongshanren-misc/post-init-xref()
  (setq xref-search-program 'ripgrep)
  )

(defun zilongshanren-misc/init-rainbow-delimiters()
  (use-package rainbow-delimiters
    :config
    (spacemacs/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook))
    ))

(defun zilongshanren-misc/post-init-forge()
  (setq forge-add-default-bindings nil)
  )

(defun zilongshanren-misc/init-elisp-demos()
  (use-package elisp-demos
    :commands elisp-demos-for-helpful elisp-demos-add-demo elisp-demos-find-demo
    :config
    (spacemacs/set-leader-keys-for-major-mode 'helpful-mode
      "d" #'elisp-demos-for-helpful
      "a" #'elisp-demos-add-demo)
    (spacemacs/set-leader-keys "hdd" #'elisp-demos-find-demo)
    ))

(defun zilongshanren-misc/init-netease-cloud-music()
  (use-package netease-cloud-music
    :commands netease-cloud-music
    :config
    (setq netease-cloud-music-show-lyric 'all)
    (setq netease-cloud-music-repeat-mode "random")
    (require 'netease-cloud-music-ui)
    (require 'netease-cloud-music-comment)

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
    (define-key netease-cloud-music-mode-map (kbd "S") #'netease-cloud-music-add-storage-to-selected-playlist)
    (define-key netease-cloud-music-mode-map (kbd "O") #'netease-cloud-music-clear-storage)
    (define-key netease-cloud-music-mode-map (kbd "L") #'netease-cloud-music-love-song)

    (define-key netease-cloud-music-switch-song-mode-map (kbd "k") #'evil-previous-line)
    (define-key netease-cloud-music-switch-song-mode-map (kbd "j") #'evil-next-line)
    (define-key netease-cloud-music-switch-song-mode-map (kbd "J") '(lambda () (interactive) (evil-next-line 5)))
    (define-key netease-cloud-music-switch-song-mode-map (kbd "K") '(lambda () (interactive) (evil-previous-line 5)))

    (define-key netease-cloud-music-switch-playlist-mode-map (kbd "k") #'evil-previous-line)
    (define-key netease-cloud-music-switch-playlist-mode-map (kbd "j") #'evil-next-line)
    (define-key netease-cloud-music-switch-playlist-mode-map (kbd "J") '(lambda () (interactive) (evil-next-line 5)))
    (define-key netease-cloud-music-switch-playlist-mode-map (kbd "K") '(lambda () (interactive) (evil-previous-line 5)))

    (defun liuyan/netease-cloud-music-start-api ()
      (interactive)
      (netease-cloud-music-start-api))
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

(defun zilongshanren-misc/init-org-msg()
  (use-package org-msg
    :after mu4e
    :config
    (setq mail-user-agent 'mu4e-user-agent
          org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
          org-msg-startup "hidestars indent inlineimages"
          org-msg-default-alternatives '((new		. (text html))
				                                 (reply-to-html	. (text html))
				                                 (reply-to-text	. (text)))
	        org-msg-signature "")

    (org-msg-mode)
  ))

(defun zilongshanren-misc/post-init-mu4e()
;;; Set up some common mu4e variables
  (setq mu4e-maildir "~/.mail"
        mu4e-trash-folder "/trash"
        mu4e-refile-folder "/archive"
        mu4e-get-mail-command "proxychains -q mbsync -a"
        mu4e-update-interval 3600
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        message-kill-buffer-on-exit t
        mu4e-compose-signature-auto-include nil
        smtpmail-auth-credentials  (expand-file-name "~/.authinfo")
        smtpmail-stream-type  'starttls
        smtpmail-smtp-service 587
        mu4e-sent-messages-behavior 'sent)


  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ivy-completing-read)

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

  (setq mu4e-contexts
      `( ,(make-mu4e-context
      :name "qq"
      :enter-func (lambda () (mu4e-message "Switch to the QQ mail context"))
      ;; leave-func not defined
      :match-func (lambda (msg)
        (when msg
          (mu4e-message-contact-field-matches msg
            :to "1412511544@qq.com")))
      :vars '((user-mail-address      . "1412511544@qq.com")
              (user-full-name     . "liuyan")
              (smtpmail-smtp-server . "smtp.qq.com")
              (smtpmail-smtp-user . "1412511544@qq.com")
              (mu4e-sent-folder . "/qq/Sent Messages")
              (mu4e-drafts-folder . "/qq/Drafts")
         ))
         ,(make-mu4e-context
      :name "gmail"
      :enter-func (lambda () (mu4e-message "Switch to the gmail context"))
      ;; leave-fun not defined
      :match-func (lambda (msg)
        (when msg
          (mu4e-message-contact-field-matches msg
            :to "lyjdwh@gmail.com")))
      :vars '((user-mail-address      . "lyjdwh@gmail.com")
              (user-full-name     . "yan.liu")
              (smtpmail-smtp-server . "smtp.gmail.com")
              (smtpmail-smtp-user . "lyjdwh@gmail.com")
              (mu4e-sent-folder . "/gmail/Sent")
              (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
         ))))
  )

(defun zilongshanren-misc/init-casease()
  (use-package casease
    :config
    (casease-setup
     :hook python-mode-hook
     :separator ?-
     :entries
     ((pascal "\\(-\\)[a-z]" "[A-Z]")
      (snake "[a-z]")))
    ))

(defun zilongshanren-misc/post-init-vterm()
  (with-eval-after-load 'vterm
    (setq vterm-max-scrollback 5000)
    (add-hook 'vterm-mode-hook 'evil-collection-vterm-toggle-send-escape)
    (evil-define-key 'insert vterm-mode-map (kbd "C-'") 'spacemacs/default-pop-shell)
    (evil-define-key 'insert vterm-mode-map (kbd "C-o") 'vterm--self-insert)
    ))

(defun zilongshanren-misc/init-telega()
  (use-package telega
    :commands telega
    :bind-keymap*
    ("C-c t" . telega-prefix-map)
    :hook
    ('telega-chat-mode . #'company-mode)
    ('telega-chat-mode . (lambda ()
                           (make-local-variable 'company-backends)
                           (dolist (it (append '(telega-company-emoji
                                                 telega-company-username
                                                 telega-company-hashtag)
                                               (when (telega-chat-bot-p telega-chatbuf--chat)
                                                 '(telega-company-botcmd))))
                             (push it company-backends))
                           ))
    :init
    (unless (display-graphic-p) (setq telega-use-images nil))
    :config
    (setq telega-emoji-company-backend 'telega-company-telegram-emoji)
    (setq telega-sticker-set-download t)
    (setf (alist-get 2 telega-avatar-factors-alist ) '(0.5 . 0.1))

    (evil-set-initial-state 'telega-root-mode 'emacs)
    (evil-set-initial-state 'telega-chat-mode 'emacs)

    (define-key telega-msg-button-map (kbd "k") 'nil)
    (define-key telega-msg-button-map (kbd "SPC") 'nil)
    (define-key telega-chat-mode-map (kbd "C-c C-t") #'telega-chatbuf-attach-sticker)

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
    ;; make avy support pinyin
    (defun my-avy--regex-candidates (fun regex &optional beg end pred group)
      (let ((regex (pyim-cregexp-build regex)))
        (funcall fun regex beg end pred group)))
    (advice-add 'avy--regex-candidates :around #'my-avy--regex-candidates)

    (setq avy-keys (number-sequence ?a ?z))
    (setq avy-orders-alist
          '((avy-goto-char . avy-order-closest)
            (avy-goto-char-2 . avy-order-closest)
            (avy-goto-parens . avy-order-closest)
            (avy-goto-word-or-subword-1 . avy-order-closest)
            (avy-goto-line . avy-order-closest)))

    (evil-define-key '(normal visual) 'global-map (kbd "gp") 'avy-goto-parens)
    (evil-define-key '(normal visual) 'global-map (kbd "gl") 'avy-goto-line)

    (evil-define-key 'normal 'evil-snipe-local-mode (kbd "S") 'evil-avy-goto-char-2)
    (evil-define-key 'normal 'evil-snipe-local-mode (kbd "s") 'evil-avy-goto-char)
    (evil-define-key 'visual 'evil-snipe-local-mode (kbd "F") 'evil-avy-goto-char-2)
    (evil-define-key 'visual 'evil-snipe-local-mode (kbd "f") 'evil-avy-goto-char)
    ))

(defun zilongshanren-misc/post-init-evil-matchit ()
  (progn
    (setq evilmi-shortcut "M")
    (global-evil-matchit-mode 1)
    ))

(defun zilongshanren-misc/init-shengci ()
  (use-package shengci
    :commands shengci-show-recorded-word shengci-capture-word-and-save
    shengci-show-memorized-word shengci-practice-guess-recorded-word
    shengci-practice-guess-memorized-word
    ))

(defun zilongshanren-misc/init-guess-word()
  (use-package guess-word
    :commands guess-word))

(defun zilongshanren-misc/post-init-elfeed ()
  (progn

    (defvar elfeed-search-filter)

    (defun my/elfeed-set-date()
      (interactive)
      (save-match-data
        (let* ((date-regexp (rx (group (or bos blank) "@" (1+ digit) (1+ (not blank)))))
               (date-tag (when (string-match date-regexp elfeed-search-filter)
                           (match-string 1 elfeed-search-filter))))
          (elfeed-search-set-filter
           (replace-regexp-in-string date-regexp (read-string "Date: " date-tag)
                                     elfeed-search-filter t t)))))

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
                            '(my/elfeed-set-date))
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
                                               (("wu" "+unread")
                                                ("m" "+later"))
                                               "Feed"
                                               (("f TAB" :complete-feed "Choose"))
                                               "Tags"
                                               (("t TAB" :complete-tag "Choose"))
                                               ))

    (defun my/elfeed-translate (url)
      (interactive (list (or (when-let* ((entry (or elfeed-show-entry
                                                    (car (elfeed-search-selected)))))
                               (elfeed-entry-link entry))
                             (read-from-minibuffer "Feed URL: "))))
      (eaf-open-browser (format "https://translate.google.com/translate?tl=zh-CN&sl=en&u=%s" url)))

    (require 'elfeed-lib)
    (defalias 'elfeed-toggle-later
      (elfeed-expose #'elfeed-search-toggle-all 'later))

    (evilified-state-evilify-map elfeed-search-mode-map
      :mode elfeed-search-mode
      :eval-after-load elfeed-search
      :bindings
      "?"  'my/elfeed-search-view-hydra/body
      "t"  'my/elfeed-translate
      "J" '(lambda () (interactive) (evil-next-line 5))
      "K" '(lambda () (interactive) (evil-previous-line 5))
      "m" 'elfeed-toggle-later
      )

    (evilified-state-evilify-map elfeed-show-mode-map
      :mode elfeed-show-mode
      :eval-after-load elfeed-show
      :bindings
      "t" 'my/elfeed-translate
      "J" '(lambda () (interactive) (evil-next-line 5))
      "K" '(lambda () (interactive) (evil-previous-line 5))
      "w" 'my-forward-word
      "b" 'self-insert-command
      "H" 'evil-first-non-blank
      "L" 'evil-end-of-line
      "v" 'evil-visual-char
      )

    ;; (setq elfeed-curl-extra-arguments '("-x" "socks5h://localhost:1080"))
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
    :init
    (setq-default evil-pinyin-with-search-rule 'always)
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
  ;; make ivy support chinese
  (require 'pyim-cregexp-utils)
  (setq ivy-re-builders-alist
        '((t . pyim-cregexp-ivy)))
  )

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
    :commands bm-toggle bm-next bm-previous counsel-bm
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
    (defun awesome-tab-project-name ()
      (let ((project-name
             (if (version< "29.0" emacs-version)
                 (nth 2 (project-current))
               (cdr (project-current)))))
        (if project-name
            (format "Project: %s" (expand-file-name project-name))
          awesome-tab-common-group-name)))

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
    :commands figlet figlet-comment
    ))

(defun zilongshanren-misc/init-try ()
  (use-package try
    :commands try
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
    :init
    ;; rime，default: ctrl + \
    (spacemacs/set-leader-keys "otr" 'toggle-input-method)
    (define-key rime-mode-map (kbd "M-]") 'rime-force-enable)
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

(defun zilongshanren-misc/init-fanyi()
  (use-package fanyi
    :custom
    (fanyi-providers '(fanyi-haici-provider fanyi-longman-provider fanyi-youdao-thesaurus-provider))
    :commands fanyi-dwim fanyi-dwim2
    :config

    (evilified-state-evilify-map fanyi-mode-map
      :bindings
      "go" 'spacemacs/counsel-jump-in-buffer
      "gj" 'org-forward-element
      "gk" 'org-backward-element
      "s" 'fanyi-dwim
      "J" '(lambda () (interactive) (evil-next-line 5))
      "K" '(lambda () (interactive) (evil-previous-line 5))
      )
    ))

(defun zilongshanren-misc/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :commands my-youdao-search-at-point youdao-dictionary-play-voice-at-point youdao-dictionary-play-voice-from-input
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
    :commands gts-do-translate-popup gts-do-translate gts-do-translate-kill-ring
    :config
    (setq gts-translate-list '(("en" "zh")))
    (setq gts-default-translator
          (gts-translator
           :picker (gts-noprompt-picker)
           :engines (list (gts-google-engine))
           :render  (gts-buffer-render)
           ))

    (setq gts-popup-translator
      (gts-translator
       :picker (gts-noprompt-picker)
       :engines (list (gts-google-engine :parser (gts-google-summary-parser)))
       :render (gts-posframe-pop-render)
       ))

    (setq gts-kill-ring-translator
          (gts-translator
           :picker (gts-noprompt-picker)
           :engines (list (gts-google-engine :parser (gts-google-summary-parser)))
           :render (gts-kill-ring-render)
           ))

    (cl-defmethod gts-translate :before ((o gts-engine) task callback)
      (with-slots (text) task
        ;; 删除多余行的逻辑，有没有更简单的方法?
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (while (re-search-forward "\n" nil t)
            (if (equal (char-after) ?\n)
                (re-search-forward "[^\n]" nil t)
              (kill-backward-chars 1)
              (insert " ")))
          (setf text (buffer-string)))))

    (defun gts-do-translate-popup ()
      (interactive)
      (gts-translate gts-popup-translator))

    (defun gts-do-translate-kill-ring ()
      (interactive)
      (gts-translate gts-kill-ring-translator))
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
    :if (display-graphic-p)
    :config
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)
    ))

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
    :commands highlight-frame-toggle clear-highlight-frame
    :config
    (setq-default highlight-faces
                  '(('hi-red-b . 0)
                    ('hi-yellow . 0)
                    ('hi-pink . 0)
                    ('hi-blue-b . 0)))
    ))

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
  (spacemacs/set-leader-keys "fj" 'deer)
  )

;; copy from spacemacs helm layer
(defun zilongshanren-misc/init-helm-ag ()
  (use-package helm-ag
    :commands (spacemacs/helm-buffers-do-rg spacemacs/helm-buffers-do-rg-region-or-symbol
               spacemacs/helm-files-do-rg spacemacs/helm-files-do-rg-region-or-symbol
               spacemacs/helm-dir-do-rg spacemacs/helm-dir-do-rg-region-or-symbol
               spacemacs/helm-project-do-rg spacemacs/helm-project-do-rg-region-or-symbol)
    :init
    (progn
      (spacemacs/set-leader-keys
        ;; opened buffers scope
        "sb"  'spacemacs/helm-buffers-do-rg
        "sB"  'spacemacs/helm-buffers-do-rg-region-or-symbol
        ;; files scope
        "sf"  'spacemacs/helm-files-do-rg
        "sF"  'spacemacs/helm-files-do-rg-region-or-symbol
        ;; current dir scope
        "sd"  'spacemacs/helm-dir-do-rg
        "sD"  'spacemacs/helm-dir-do-rg-region-or-symbol
        ;; current project scope
        "sp" 'spacemacs/helm-project-do-rg
        "sP" 'spacemacs/helm-project-do-rg-region-or-symbol))
    :config
    (progn
      (advice-add 'helm-ag--save-results :after 'spacemacs//gne-init-helm-ag)
      (evil-define-key 'normal helm-ag-map "SPC" spacemacs-default-map)
      (evilified-state-evilify-map helm-ag-mode-map
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

(defun zilongshanren-misc/init-discover-my-major ()
  (use-package discover-my-major
    :commands discover-my-major
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
      )))

(defun zilongshanren-misc/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    ;; disable highlight when use swiper or evil ex search, this option won't effect evil-ex-search-next command
    (setq-default evil-ex-search-persistent-highlight nil)

    (push "TAGS" spacemacs-useless-buffers-regexp)

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

    ;; (evil-define-key '(normal visual) 'global-map (kbd "]f") 'end-of-defun)
    ;; (evil-define-key '(normal visual) 'global-map (kbd "[f") 'beginning-of-defun)
    (evil-define-key '(normal visual) 'global-map (kbd "]s") 'forward-sexp)
    (evil-define-key '(normal visual) 'global-map (kbd "[s") 'backward-sexp)
    (evil-define-key '(normal visual) 'global-map (kbd "[u") 'backward-up-list)
    (evil-define-key '(normal visual) 'global-map (kbd "]u") 'backward-up-list)
    (evil-define-key '(normal visual) 'global-map (kbd "]p") 'evil-forward-paragraph)
    (evil-define-key '(normal visual) 'global-map (kbd "[p") 'evil-backward-paragraph)

    (define-key evil-normal-state-map (kbd "M-y") 'counsel-yank-pop)
    (define-key evil-normal-state-map (kbd "gf") 'evil-find-file-at-point-with-line)
    (define-key evil-normal-state-map (kbd "gF") 'find-file-at-point)
    (define-key evil-normal-state-map (kbd "gr") 'xref-find-references)
    (define-key evil-normal-state-map (kbd "gR") 'lsp-ui-peek-find-references)
    (define-key evil-normal-state-map (kbd "ze") 'spacemacs/error-transient-state/body)

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

    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)

    ;; add pulse highlight on yank
    (defun evil-yank-advice (orig-fn beg end &rest args)
      (pulse-momentary-highlight-region beg end)
      (apply orig-fn beg end args))
    (advice-add 'evil-yank :around 'evil-yank-advice)

    ;; cursor type
    (put 'cursor 'evil-emacs-color  (face-foreground 'warning))
    (put 'cursor 'evil-normal-color (face-background 'cursor))

    (defun evil-default-cursor-fn ()
      (evil-set-cursor-color (get 'cursor 'evil-normal-color)))

    (defun evil-emacs-cursor-fn ()
      (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

    (setq evil-default-cursor 'evil-default-cursor-fn
          evil-normal-state-cursor 'box
          evil-emacs-state-cursor  '(box evil-emacs-cursor-fn)
          evil-insert-state-cursor 'bar
          evil-visual-state-cursor 'hollow
          )
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

    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mn") 'mc/mark-next-like-this)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mp") 'mc/mark-previous-like-this)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mN") 'mc/skip-to-next-like-this)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mP") 'mc/skip-to-previous-like-this)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mu") 'mc/unmark-next-like-this)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mU") 'mc/unmark-previous-like-this)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mm") 'mc/mark-more-like-this-extended)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "me") 'mc/edit-beginnings-of-lines)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mE") 'mc/edit-ends-of-lines)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "ma") 'mc/mark-all-dwim)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mA") 'mc/mark-all-like-this)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mr") 'mc/mark-all-in-region)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "md") 'mc/mark-all-like-this-in-defun)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mR") 'set-rectangular-region-anchor)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mt") 'mc/mark-sgml-tag-pair)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mi") 'mc/insert-numbers)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mI") 'mc/insert-letters)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "ms") 'mc/sort-regions)
    (evil-define-key '(normal visual) 'evil-snipe-local-mode (kbd "mV") 'mc/reverse-regions)
    ))


(defun zilongshanren-misc/post-init-evil-escape ()
  (setq evil-escape-delay 0.2))

(defun zilongshanren-misc/post-init-projectile ()
  (progn
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy)
      (setq projectile-project-configure-cmd "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -S . -B build")

      (add-to-list 'projectile-other-file-alist '("html" "js"))
      (add-to-list 'projectile-other-file-alist '("js" "html")))

    (spacemacs/set-leader-keys "pf" 'zilongshanren/open-file-with-projectile-or-counsel-git)
    ))

(defun zilongshanren-misc/post-init-swiper ()
  "Initialize my package"
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)

    (use-package ivy
      :defer t
      :config
      (progn
        (spacemacs|hide-lighter ivy-mode)

        (setq ivy-dynamic-exhibit-delay-ms 300)

        (ivy-set-actions
         t
         '(("f" my-find-file-in-git-repo "find files")
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

    ))


(defun zilongshanren-misc/post-init-magit ()
  (with-eval-after-load 'magit

    ;; speed up magit
    (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

    ;; show differences within lines, magit-delta too slow
    (setq magit-diff-refine-hunk 'all)

    ;; (evil-collection-define-key evil-collection-magit-state 'git-rebase-mode-map (kbd "e") 'git-rebase-edit)
    ;; (evil-collection-define-key evil-collection-magit-state 'git-rebase-mode-map (kbd "s") 'git-rebase-squash)
    ))

(defun zilongshanren-misc/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
      (define-key git-messenger-map (kbd "f") 'zilong/github-browse-commit))))

(defun zilongshanren-misc/post-init-markdown-mode ()
  (progn
    (with-eval-after-load 'markdown-mode
      (progn
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
