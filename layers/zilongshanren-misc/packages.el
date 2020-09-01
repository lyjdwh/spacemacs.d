;;; packages.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <lyjdwh@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq zilongshanren-misc-packages
      '(
        helm-github-stars
        helm
        helm-ag
        projectile
        prodigy
        find-file-in-project
        multiple-cursors
        visual-regexp
        visual-regexp-steroids
        command-log
        evil
        fcitx
        discover-my-major
        ace-window
        persp-mode
        tiny
        expand-region
        ;; smartparens
        flyspell-correct
        peep-dired
        markdown-mode
        swiper
        magit
        git-messenger
        gist
        hydra
        wrap-region
        ranger
        golden-ratio
        ;; (highlight-global :location (recipe :fetcher github :repo "gle-dai/highlight-global"))
        (highlight-global :location local)
        symbol-overlay
        chinese-conv
        ;; chinese-wbim
        lispyville
        popup
        keyfreq
        terminal-here
        git-gutter
        speed-type
        zone
        leetcode
        youdao-dictionary
        (posframe :location (recipe :fetcher github :repo "tumashu/posframe") )
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
        (meow :location (recipe :fetcher github :repo "DogLooksGood/meow") )
        evil-snipe
        forge
        (powerthesaurus :location (recipe :fetcher github :repo "SavchenkoValeriy/emacs-powerthesaurus"))
        mw-thesaurus
        langtool
        (inherit-org :location local)
        (awesome-tab :location (recipe :fetcher github :repo "manateelazycat/awesome-tab"))
        bm
        counsel
        pdfgrep
        separedit
        pyim
        key-chord
        (evil-pinyin :location (recipe :fetcher github :repo "laishulu/evil-pinyin"))
        kaomoji
        magit-delta
        (english-teacher :location (recipe :fetcher github :repo "loyalpartner/english-teacher.el"))
        pos-tip
        (maple-header :location (recipe :fetcher github :repo "honmaple/maple-emacs" :files ("site-lisp/maple/maple-header.el")))
        ace-pinyin
        tmux-pane
        ivy-avy
        ))

(defun zilongshanren-misc/init-ivy-avy ()
  (use-package ivy-avy))

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
    (defun maple-header:template(&optional prefix)
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
    (maple-header-define modify-by
      :find ".*\\(By:\\)\\(.*\\)"
      :replace user-mail-address)

    (add-to-list 'maple-header:auto-insert-alist
                 '((sh-mode . "Shell script") nil
                    "#!/usr/bin/env bash\n"
                    (maple-header:template) "\n"))

    (add-to-list 'maple-header:auto-insert-alist
                 '((c++-mode . "C++ program") nil
                   "/*"
                   (string-trim-left
                    (maple-header:template " "))
                   "*/\n"))

    (setq maple-header-filename-p t
          maple-header-email-p nil
          maple-header-modify-p t
          maple-header-modify-by-p t)
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

(defun zilongshanren-misc/init-magit-delta ()
  (use-package magit-delta
    :after magit
    :config
    (magit-delta-mode 1)
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
    :demand t
    :init
    ;; restore on load (even before you require bm)
    (setq bm-restore-repository-on-load t)

    :config
    (setq bm-annotate-on-create t)
    (setq bm-cycle-all-buffers t)
    (setq bm-repository-file "~/.emacs.d/bm-repository")

    ;; save bookmarks
    (setq-default bm-buffer-persistence t)

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
    :load-path "~/bin/inherit-org"
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
    :demand t
    :commands  mw-thesaurus-lookup-at-point
    :config
    (add-hook 'mw-thesaurus-mode-hook 'variable-pitch-mode)
    (setq mw-thesaurus--api-key "69fe3e22-45d0-4e53-8b6f-a3ace4b2ce3a")
    ))

(defun zilongshanren-misc/init-powerthesaurus ()
  (use-package powerthesaurus
    :commands (powerthesaurus-lookup-word powerthesaurus-lookup-word-at-point
                                          powerthesaurus-lookup-word-dwim)
    ))

(defun zilongshanren-misc/init-forge ()
  (use-package forge
    :after magit))

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
    :custom
    ;; layout options: qwerty, dvorak, dvp, colemak
    (meow-layout 'qwerty)
    :config
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
  (use-package avy-thing-edit))

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
    ;; (((text-mode prog-mode) . sis-follow-context-mode)
    ;;  ((text-mode prog-mode) . sis-inline-mode))

    :config
    (sis-ism-lazyman-config "1" "2" 'fcitx5)

    ;; enable the /respect/ mode
    (sis-global-respect-mode t)
    ;; enable the /follow context/ mode for all buffers
    (sis-global-follow-context-mode t)
    ;; enable the /inline english/ mode for all buffers
    (sis-global-inline-mode t)
    (setq sis-inline-with-other	t)
    (setq sis-english-pattern "[a-zA-Z!@#$%^&*()-=_+,./<>?;':\"|\\]")
    ))

(defun zilongshanren-misc/init-rime ()
  (use-package rime
    :config
    (setq rime-user-data-dir "~/.config/fcitx/rime")

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

(defun zilongshanren-misc/init-leetcode ()
  (use-package leetcode
    :defer t
    :config
    (define-key leetcode--problems-mode-map (kbd "TAB") 'leetcode-show-current-problem)
    (define-key leetcode--problems-mode-map (kbd "<return>") 'leetcode-show-current-problem)
    (setq leetcode-prefer-language "python3")
    (setq leetcode--domain "leetcode-cn.com")
    (setq leetcode--base-url "https://leetcode-cn.com")))

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

(defun zilongshanren-misc/init-lispyville ()
  (use-package lispyville
    :init
    (progn
      (add-hook 'lispy-mode-hook #'lispyville-mode)
      )
    ))

(defun zilongshanren-misc/post-init-chinese-conv ()
  (setq chinese-conv-opencc-program "/usr/bin/opencc")
  (setq chinese-conv-opencc-data "/usr/share/opencc/"))

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
    (defhydra hydra-hotspots (:color blue)
      "Hotspots"
      ("b" blog-admin-start "blog")
      ("g" helm-github-stars "helm github stars")
      ("r" zilongshanren/run-current-file "run current file"))

    (defhydra multiple-cursors-hydra (:hint nil)
      "
       ^Up^            ^Down^        ^Other^
             ----------------------------------------------
         [_p_]   Next    [_n_]   Next    [_l_] Edit lines
         [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
         [_M-p_] Unmark  [_M-n_] Unmark [_r_] Mark by regexp
         ^ ^             ^ ^ [_q_] Quit
       "
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this :exit t)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("r" mc/mark-all-in-region-regexp :exit t)
      ("q"
       nil))

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

(defun zilongshanren-misc/init-peep-dired ()
  ;;preview files in dired
  (use-package peep-dired
    :defer t
    :commands (peep-dired-next-file
               peep-dired-prev-file)
    :bind (:map dired-mode-map
                ("P" . peep-dired))))


(defun zilongshanren-misc/post-init-flyspell-correct ()
  (progn
    (with-eval-after-load 'flyspell
      (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous))
    (setq flyspell-correct-interface 'flyspell-correct-ivy)))

(defun zilongshanren-misc/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-(") 'wrap-sexp-with-new-round-parens))
    :config
    (progn
      (setq sp-highlight-pair-overlay nil)

      (evil-define-key 'normal sp-keymap
        (kbd ")>") 'sp-forward-slurp-sexp
        (kbd ")<") 'sp-forward-barf-sexp
        (kbd "(>") 'sp-backward-barf-sexp
        (kbd "(<") 'sp-backward-slurp-sexp))))

(defun zilongshanren-misc/init-tiny ()
  (use-package tiny
    :defer t
    ;; :init
    ;; (spacemacs/set-leader-keys "oe" 'tiny-expand)
    ))

(defun zilongshanren-misc/post-init-helm ()
  (with-eval-after-load 'helm
    (progn
      (setq helm-buffer-max-length 56)

      ;; limit max number of matches displayed for speed
      (setq helm-candidate-number-limit 100)
      ;; ignore boring files like .o and .a
      (setq helm-ff-skip-boring-files t)
      ;; replace locate with spotlight on Mac
      (setq helm-locate-command "mdfind -name %s %s")
      (push "\\.emlx$" helm-boring-file-regexp-list)
      )
    ))

(defun zilongshanren-misc/init-helm-github-stars ()
  (use-package helm-github-stars
    :commands (helm-github-stars)
    :init
    (setq helm-github-stars-username "zilongshanren")))




(defun zilongshanren-misc/post-init-fcitx ()
  (fcitx-aggressive-setup))

(defun zilongshanren-misc/post-init-command-log ()
  (with-eval-after-load 'global-command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))



(defun zilongshanren-misc/init-litable ()
  (use-package litable
    :init
    :defer t))

(defun zilongshanren-misc/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o") #'ace-window))

(defun zilongshanren-misc/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map)
      )))


(defun zilongshanren-misc/post-init-elfeed ()
  (use-package elfeed
    :init
    (global-set-key (kbd "C-x w") 'elfeed)
    :defer t
    :config
    (progn

      (setq elfeed-feeds
            '("http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://irreal.org/blog/?feed=rss2"
              "http://feeds.feedburner.com/LostInTheTriangles"
              "http://tonybai.com/feed/"
              "http://planet.emacsen.org/atom.xml"
              "http://feeds.feedburner.com/emacsblog"
              "http://blog.binchen.org/rss.xml"
              "http://oremacs.com/atom.xml"
              "http://blog.gemserk.com/feed/"
              "http://www.masteringemacs.org/feed/"
              "http://t-machine.org/index.php/feed/"
              "http://gameenginebook.blogspot.com/feeds/posts/default"
              "http://feeds.feedburner.com/ruanyifeng"
              "http://coolshell.cn/feed"
              "http://blog.devtang.com/atom.xml"
              "http://emacsist.com/rss"
              "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
              "http://angelic-sedition.github.io/atom.xml"))

      ;; (evilify elfeed-search-mode elfeed-search-mode-map)
      (evilified-state-evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :bindings
        "G" 'elfeed-update
        "g" 'elfeed-search-update--force)

      (defun zilong/elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'zilong/elfeed-mark-all-as-read)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring"
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank))))

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

    ;; rebind g,k to gj and gk
    ;; (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    ;; (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))

    (define-key evil-normal-state-map (kbd "g[")
      (lambda () (interactive) (beginning-of-defun)))

    (define-key evil-normal-state-map (kbd "g]")
      (lambda () (interactive) (end-of-defun)))

    (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
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
    ;; (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
    ;; (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
    (define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)
    (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
    (define-key evil-visual-state-map (kbd "mp") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    (define-key evil-visual-state-map (kbd "mf") 'mc/mark-all-like-this-in-defun)


    ;; in spacemacs, we always use evilify miscro state
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
    ;; Don't move back the cursor one position when exiting insert mode
    (setq evil-move-cursor-back nil)

    ;; (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
    ;; (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
    ;; (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
    ;; (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)

    ;; for emacs shell mode
    ;; (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
    ;; (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
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
    ;; This will break visual column edit
    ;; enable hybrid editing style
    ;; (defadvice evil-insert-state (around zilongshanren/holy-mode activate)
    ;;   "Preparing the holy water flasks."
    ;;   (evil-emacs-state))
    ;; disable c-[ temporally
    ;; (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
    ;; (bind-keys ("<C-[>" . evil-normal-state))
    ;; (setq evil-emacs-state-cursor '("chartreuse3" (bar . 2)))
    ;; (define-key evil-emacs-state-map [escape] 'evil-normal-state)
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
    :init
    (progn

      (bind-key* "C-s-l" 'mc/edit-lines)
      (bind-key* "C-s-f" 'mc/mark-all-dwim)
      (bind-key* "s-." 'mc/mark-next-like-this)
      (bind-key* "C-s-," 'mc/mark-previous-like-this)
      (bind-key* "s->" 'mc/unmark-next-like-this)
      (bind-key* "s-<" 'mc/unmark-previous-like-this)
      (bind-key* "C-c C-s-." 'mc/mark-all-like-this)

      ;; http://endlessparentheses.com/multiple-cursors-keybinds.html?source=rss
      (define-prefix-command 'endless/mc-map)
      ;; C-x m is usually `compose-mail'. Bind it to something
      ;; else if you use this command.
      (define-key ctl-x-map "m" 'endless/mc-map)
;;; Really really nice!
      (define-key endless/mc-map "i" #'mc/insert-numbers)
      (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
      (define-key endless/mc-map "a" #'mc/mark-all-like-this)

;;; Occasionally useful
      (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
      (define-key endless/mc-map "r" #'mc/reverse-regions)
      (define-key endless/mc-map "s" #'mc/sort-regions)
      (define-key endless/mc-map "l" #'mc/edit-lines)
      (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
      (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines)
      )
    :config
    (setq mc/cmds-to-run-once
          '(
            counsel-M-x
            zilongshanren/my-mc-mark-next-like-this))
    (setq mc/cmds-to-run-for-all
          '(
            electric-newline-and-maybe-indent
            hungry-delete-backward
            spacemacs/backward-kill-word-or-region
            spacemacs/smart-move-beginning-of-line
            evil-substitute
            lispy-move-beginning-of-line
            lispy-move-end-of-line
            lispy-space
            lispy-delete-backward
            evil-exit-visual-state
            evil-backward-char
            evil-delete-char
            evil-escape-emacs-state
            evil-escape-insert-state
            mwim-beginning-of-code-or-line
            mwim-end-of-line-or-code
            evil-exit-emacs-state
            evil-previous-visual-line
            evil-next-visual-line
            evil-forward-char
            evil-insert
            evil-next-line
            evil-normal-state
            evil-previous-line
            evil-append
            evil-append-line
            forward-sentence
            kill-sentence
            org-self-insert-command
            sp-backward-delete-char
            sp-delete-char
            sp-remove-active-pair-overlay
            orgtbl-hijacker-command-109))
    ))

(defun zilongshanren-misc/post-init-persp-mode ()
  (setq persp-kill-foreign-buffer-behaviour 'kill)
  (setq persp-lighter nil)

  (defun zilongshanren-kill-other-persp-buffers (&optional arg)
    "Kill all other buffers in current persp layout"
    (interactive)
    (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                               (buffer-name)))
      (mapc 'persp-kill-buffer (delq (current-buffer) (persp-buffer-list)))
      (persp-add-buffer (current-buffer))
      (message "Buffers deleted!")))

  (when (fboundp 'spacemacs|define-custom-layout)
    (spacemacs|define-custom-layout "@work"
      :binding "w"
      :body
      (find-file "~/Github/HlMJ_js/assets/scripts/Login/LoginScene.ts")))
  (when (fboundp 'spacemacs|define-custom-layout)
    (spacemacs|define-custom-layout "@blog"
      :binding "b"
      :body
      (find-file "~/zilongshanren.com/config.toml")))
  )

;; deprecated
(defun zilongshanren-misc/post-init-chinese-wbim ()
  (progn

    ;; (bind-key* ";" 'chinese-wbim-insert-ascii)
    ;; (setq chinese-wbim-punc-translate-p nil)

    (setq chinese-wbim-tooltip-timeout 5)
    (spacemacs/declare-prefix "ot" "Toggle")
    (spacemacs/set-leader-keys
      "otp" 'chinese-wbim-punc-translate-toggle)
    (setq chinese-wbim-wb-use-gbk t)

    (setq chinese-wbim-use-tooltip t)

    (add-hook 'chinese-wbim-wb-load-hook
              (lambda ()
                (let ((map (chinese-wbim-mode-map)))
                  (define-key map "-" 'chinese-wbim-previous-page)
                  (define-key map "=" 'chinese-wbim-next-page))))
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



(defun zilongshanren-misc/post-init-prodigy ()
  (progn
    (prodigy-define-tag
      :name 'jekyll
      :env '(("LANG" "en_US.UTF-8")
             ("LC_ALL" "en_US.UTF-8")))
    ;; define service


    (prodigy-define-service
      :name "Hugo Server"
      :command "hugo"
      :args '("server" "-D" "--navigateToChanged" "-t" "even")
      :cwd blog-admin-dir
      :tags '(hugo server)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "hugo Deploy"
      :command "bash"
      :args '("./deploy.sh" )
      :cwd blog-admin-dir
      :tags '(hugo deploy)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

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

        (defun ivy-call-and-recenter ()
          "Call action and recenter window according to the selected candidate."
          (interactive)
          (ivy-call)
          (with-ivy-window
            (evil-scroll-line-to-center (line-number-at-pos))))

        ;; .projectile file will specify the search root
        ;; add / to search when use expand region
        ;; (when (configuration-layer/package-used-p 'counsel)
        ;;   (defadvice er/prepare-for-more-expansions-internal
        ;;       (around ivy-rg/prepare-for-more-expansions-internal activate)
        ;;     ad-do-it
        ;;     (let ((new-msg (concat (car ad-return-value)
        ;;                            ", / to search in project, "))
        ;;           (new-bindings (cdr ad-return-value)))
        ;;       (cl-pushnew
        ;;        '("/" (lambda ()
        ;;                (call-interactively
        ;;                 'spacemacs/search-project-auto-region-or-symbol)))
        ;;        new-bindings)
        ;;       (setq ad-return-value (cons new-msg new-bindings)))))


        (ivy-set-actions
         t
         '(("f" my-find-file-in-git-repo "find files")
           ("!" my-open-file-in-external-app "Open file in external app")
           ("I" ivy-insert-action "insert")
           ("C" ivy-kill-new-action "copy")
           ("d" ivy--kill-buffer-action )
           ("k" ivy--kill-buffer-action "kill")
           ("r" ivy--rename-buffer-action "rename")
           ("S" ivy-ff-checksum-action "Checksum")))

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
  (progn
    (with-eval-after-load 'magit
      (progn

        (add-to-list 'magit-no-confirm 'stage-all-changes)
        (define-key magit-log-mode-map (kbd "W") 'magit-copy-section-value)
        (setq magit-completing-read-function 'magit-builtin-completing-read)

        (magit-define-popup-switch 'magit-push-popup ?u
                                   "Set upstream" "--set-upstream")


        (magit-add-section-hook 'magit-status-sections-hook
                                'magit-insert-assume-unchanged-files nil t)

        ;; insert the hidden files section in the magit status buffer.
        (magit-add-section-hook 'magit-status-sections-hook
                                'magit-insert-skip-worktree-files nil t)

        (define-key magit-status-mode-map "ga" 'magit-jump-to-assume-unchanged)

        (define-key magit-status-mode-map "gw" 'magit-jump-to-skip-worktree)
        ))

    ;; prefer two way ediff
    (setq magit-ediff-dwim-show-on-hunks t)

    (setq magit-push-always-verify nil)

    (eval-after-load 'magit
      '(define-key magit-mode-map (kbd "C-c g")
         #'zilongshanren/magit-visit-pull-request))

    (setq magit-process-popup-time 10)))

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
