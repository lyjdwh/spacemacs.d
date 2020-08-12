;;; keybindings.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren
;;
;; Author: zilongshanren <lyjdwh@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(define-key global-map [(shift return)] 'zilongshanren/smart-open-line)

(define-key global-map (kbd "<f9>") 'org-capture)
(define-key global-map (kbd "<f8>") 'zilongshanren/show-current-buffer-major-mode)
(define-key global-map (kbd "<f5>") 'zilongshanren/run-current-file)
(define-key global-map (kbd "<f1>") 'zilongshanren/helm-hotspots)

(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

(bind-key* "M--" 'evil-jump-item)
(bind-key* "C-c k" 'which-key-show-top-level)

;; Utility functions
(defun bb/define-key (keymap &rest bindings)
  (declare (indent 1))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))

(define-key evil-normal-state-map (kbd "J") '(lambda () (interactive) (evil-next-line 5)))
(define-key evil-normal-state-map (kbd "K") '(lambda () (interactive) (evil-previous-line 5)))
(define-key evil-normal-state-map (kbd "-") nil)
(define-key evil-visual-state-map (kbd "J") '(lambda () (interactive) (evil-next-line 5)))
(define-key evil-visual-state-map (kbd "K") '(lambda () (interactive) (evil-previous-line 5)))
(evil-define-key 'normal emacs-lisp-mode-map (kbd "gh") 'helpful-at-point)
(evil-define-key 'normal python-mode-map (kbd "gh") 'lsp-describe-thing-at-point)
(evil-define-key 'normal c++-mode-map (kbd "gh") 'lsp-describe-thing-at-point)
(define-key evil-normal-state-map (kbd "gH") 'mouse-hover-tooltip)

(bb/define-key evil-normal-state-map
  "+" 'evil-numbers/inc-at-pt
  "-" 'evil-numbers/dec-at-pt
  "\\" 'evil-repeat-find-char-reverse)

(bb/define-key ivy-occur-grep-mode-map
  (kbd "C-d") 'evil-scroll-down
  "d" 'ivy-occur-delete-candidate)

(with-eval-after-load 'company
  (bb/define-key company-active-map
      (kbd "C-w") 'evil-delete-backward-word))

(spacemacs/declare-prefix "ot" "Toggle")

(spacemacs/set-leader-keys "oS" 'zilongshanren/helm-hotspots)
(spacemacs/set-leader-keys "oc" 'my-auto-update-tags-when-save)
(spacemacs/set-leader-keys "fR" 'zilongshanren/rename-file-and-buffer)
(spacemacs/set-leader-keys "bD" 'spacemacs/kill-other-buffers)

;; emacs bookmark can work in any buffer, but only one bookmark for one buffer
;; bm only work in file buffer, can set multi bookmarks for one buffer
;; other bookmark
;; 1. m: evil-set-marker, `: evil-goto-mark
;; 2. register: point-to-register, jump to register
(spacemacs/declare-prefix "ob" "Bookmark")
(spacemacs/set-leader-keys "obs" 'bookmark-set)
(spacemacs/set-leader-keys "obr" 'bookmark-rename)
(spacemacs/set-leader-keys "obd" 'bookmark-delete)
(spacemacs/set-leader-keys "obk" 'counsel-bookmark)
(spacemacs/set-leader-keys "obK" 'bookmark-jump-other-window)
(spacemacs/set-leader-keys "obe" 'edit-bookmarks)
(spacemacs/set-leader-keys "obi" 'bookmark-insert)
(spacemacs/set-leader-keys
  "obb" 'bm-toggle
  "obB" 'counsel-bm
  "obj" 'bm-next
  "obJ" 'bm-previous
  )

(spacemacs/set-leader-keys "od" 'occur-dwim)
(spacemacs/set-leader-keys "ok" 'zilongshanren-kill-other-persp-buffers)
(spacemacs/set-leader-keys "ox" 'org-open-at-point-global)

(spacemacs/set-leader-keys "rh" 'helm-resume)

(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  "gr" 'xref-find-references)

;; ivy specific keybindings
(if (configuration-layer/layer-usedp 'ivy)
    (progn
      (spacemacs/set-leader-keys "ff" 'counsel-find-file)
      (spacemacs/set-leader-keys "fL" 'counsel-locate)
      (spacemacs/set-leader-keys "hi" 'counsel-info-lookup-symbol)
      (spacemacs/set-leader-keys "pb" 'projectile-switch-to-buffer)))

(spacemacs/set-leader-keys "en" 'flycheck-next-error)
(spacemacs/set-leader-keys "ep" 'flycheck-previous-error)
(spacemacs/set-leader-keys "o(" 'ielm)

(spacemacs/set-leader-keys "gL" 'magit-log-buffer-file)
(spacemacs/set-leader-keys "gn" 'smerge-next)
(spacemacs/set-leader-keys "gp" 'smerge-prev)
(spacemacs/set-leader-keys "og" 'my-git-timemachine)

(spacemacs/set-leader-keys "sj" 'zilongshanren/counsel-imenu)
;; deal with BOM
(spacemacs/set-leader-keys "fl" 'find-file-literally-at-point)
(spacemacs/set-leader-keys "ri" 'ivy-resume)
(spacemacs/set-leader-keys "fh" 'ffap-hexl-mode)
(spacemacs/set-leader-keys "fd" 'projectile-find-file-dwim-other-window)
(spacemacs/set-leader-keys "nl" 'spacemacs/evil-search-clear-highlight)
(spacemacs/declare-prefix "ol" "layout")
(spacemacs/set-leader-keys "oll" 'zilongshanren/load-my-layout)
(spacemacs/set-leader-keys "ols" 'zilongshanren/save-my-layout)

;; youdao
(spacemacs/set-leader-keys "oy" 'my-youdao-search-at-point)
(spacemacs/set-leader-keys "oY" 'youdao-dictionary-search-from-input)
(spacemacs/set-leader-keys "ow" 'youdao-dictionary-play-voice-at-point)
(spacemacs/set-leader-keys "oW" 'youdao-dictionary-play-voice-from-input)

(spacemacs/set-leader-keys "bM" 'spacemacs/switch-to-messages-buffer)
(spacemacs/set-leader-keys "sS" 'spacemacs/swiper-region-or-symbol)

(spacemacs/set-leader-keys "os" 'spacemacs/helm-project-do-ag-region-or-symbol)

(spacemacs/set-leader-keys "pa" 'projectile-find-other-file)
(spacemacs/set-leader-keys "pA" 'projectile-find-other-file-other-window)
(spacemacs/set-leader-keys ":" 'counsel-M-x)
(spacemacs/set-leader-keys "xe" 'set-buffer-file-coding-system)

;; highlight
(spacemacs/set-leader-keys "hh" 'zilongshanren/highlight-dwim)
(spacemacs/set-leader-keys "hc" 'zilongshanren/clearn-highlight)

;; emacs daemon
(spacemacs/set-leader-keys "qD" 'spacemacs/restart-emacs-debug-init)
(spacemacs/set-leader-keys "qd" 'liuyan/server-shutdown)
(spacemacs/set-leader-keys "Th" 'liuyan/toggle-terminal-transparency)

;;ivy-yasnippet
(spacemacs/set-leader-keys "oi" 'ivy-yasnippet)

;;eaf
(spacemacs/set-leader-keys "oo" 'eaf-open-this-from-dired)
(spacemacs/declare-prefix "ae" "eaf")
(spacemacs/set-leader-keys "aei" 'eaf-open-ipython)
(spacemacs/set-leader-keys "aec" 'eaf-open-camera)
(spacemacs/set-leader-keys "aed" 'eaf-open-demo)
(spacemacs/set-leader-keys "aeb" 'eaf-open-browser)
(spacemacs/set-leader-keys "aee" 'eaf-open-external)
(spacemacs/set-leader-keys "aet" 'eaf-open-terminal)
(spacemacs/set-leader-keys "aeT" 'eaf-toggle-fullscreen)
(spacemacs/set-leader-keys "aeo" 'eaf-open)
(spacemacs/set-leader-keys "aeF" 'eaf-open-office)
(spacemacs/set-leader-keys "aem" 'eaf-open-mindmap)
(spacemacs/set-leader-keys "aea" 'eaf-open-airshare)
(spacemacs/set-leader-keys "aeB" 'eaf-open-bookmark)
(spacemacs/set-leader-keys "aer" 'eaf-open-rss-reader)
(spacemacs/set-leader-keys "aeq" 'eaf-kill-process)
(spacemacs/set-leader-keys "aes" 'eaf-search-it)
(spacemacs/set-leader-keys "aef" 'eaf-file-browser-qrcode)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "m." 'eaf-interleave-sync-current-note
  "mj" 'eaf-interleave-sync-next-note
  "mk" 'eaf-interleave-sync-previous-note)

(spacemacs/set-leader-keys
  "ami" 'eaf-interleave-add-note
  "amo" 'eaf-interleave-open-notes-file
  "amq" 'eaf-interleave-quit)

;; snails
(spacemacs/set-leader-keys "aa" 'snails)
(spacemacs/set-leader-keys "sn" 'snails-search-point)


;; company-english-helper
(spacemacs/set-leader-keys "ote" 'toggle-company-english-helper)
(spacemacs/set-leader-keys "oey" 'english-teacher-smart-translate)
;; insert-translated-name
(spacemacs/set-leader-keys "it" 'insert-translated-name-insert)
(spacemacs/set-leader-keys "iT" 'insert-translated-name-insert-original-translation	)
(spacemacs/set-leader-keys "ir" 'insert-translated-region-replace)

;; rotate-text
(spacemacs/set-leader-keys "or" 'rotate-text)

;; pandoc
(spacemacs/set-leader-keys "op" 'spacemacs/run-pandoc)

;; speed type
(spacemacs/set-leader-keys "att" 'speed-type-text)

;; color rg
(spacemacs/declare-prefix "sr" "color-rg")
(spacemacs/set-leader-keys "srd" 'color-rg-search-input)
(spacemacs/set-leader-keys "srD" 'color-rg-search-symbol)
(spacemacs/set-leader-keys "srp" 'color-rg-search-input-in-project)
(spacemacs/set-leader-keys "srP" 'color-rg-search-symbol-in-project)
(spacemacs/set-leader-keys "srf" 'color-rg-search-input-in-current-file)
(spacemacs/set-leader-keys "srF" 'color-rg-search-symbol-in-current-file)
(spacemacs/set-leader-keys "srt" 'color-rg-search-symbol-with-type)
(spacemacs/set-leader-keys "srT" 'color-rg-search-project-with-type)

;; grep dired
(spacemacs/set-leader-keys "srg" 'grep-dired-dwim)
(spacemacs/set-leader-keys "srG" 'grep-dired)


;; take screenshot
(spacemacs/declare-prefix "as" "shot")
(spacemacs/set-leader-keys "asf" 'screenshot)
(spacemacs/set-leader-keys "asF" 'screenshot-clip)

;; highlight todo and similar keywords
(spacemacs/declare-prefix "oh" "hl-todo/highlight")
(spacemacs/set-leader-keys "ohp" 'hl-todo-previous)
(spacemacs/set-leader-keys "ohn" 'hl-todo-next)
(spacemacs/set-leader-keys "oho" 'hl-todo-occur)
(spacemacs/set-leader-keys "ohi" 'hl-todo-insert)
(spacemacs/set-leader-keys "ohl" 'ivy-magit-todos)
(spacemacs/set-leader-keys "ohh" 'zilongshanren/highlight-dwim)

;; leetcode
(spacemacs/declare-prefix "aL" "Leetcode")
(spacemacs/set-leader-keys
  "aLl" 'leetcode
  "aLd" 'leetcode-show-current-problem
  "aLr" 'leetcode-refresh
  "aLt" 'leetcode-try
  "aLu" 'leetcode-submit
  )

;; proxy
(spacemacs/set-leader-keys
  "otp" 'proxy-socks-toggle
  "otP" 'proxy-http-toggle
  )

;; google
(spacemacs/set-leader-keys "ag" 'engine/search-google)

;; rime，default: ctrl + \
(spacemacs/set-leader-keys "otr" 'toggle-input-method)
(define-key rime-mode-map (kbd "M-]") 'rime-force-enable)

;; lsp
;; (define-key spacemacs-lsp-mode-map (kbd "hg") 'lsp-ui-doc-glance)

;; helm ag to overhide ivy ag
(spacemacs/set-leader-keys "sad" 'spacemacs/helm-dir-do-ag)
(spacemacs/set-leader-keys "saD" 'spacemacs/helm-dir-do-ag-region-or-symbol)

;; org
(spacemacs/set-leader-keys "ao." 'spacemacs/org-agenda-transient-state/org-agenda-set-tags)
(spacemacs/set-leader-keys "aoi" 'org-insert-last-stored-link)

;; ivy-bibex
;; (global-set-key (kbd "C-x p") 'ivy-bibtex-my-publications)
;; press M-o list actions in ivy-bibex
(spacemacs/set-leader-keys "ab" 'ivy-bibtex)

;; org noter
(spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode
  "i" 'org-noter-insert-precise-note
  "c" 'org-noter-create-skeleton
  "g" 'pdfgrep
  )
(spacemacs/declare-prefix-for-mode 'org-mode  "mn" "org-noter")
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "nc" 'org-noter-sync-current-note
  "nj" 'org-noter-sync-next-note
  "nk" 'org-noter-sync-prev-note)

(defalias 'org-insert-src-block 'zilongshanren/org-insert-src-block)
(defalias 'org-archive-done-tasks 'zilongshanren/org-archive-done-tasks)
(defalias 'capture-screenshot 'zilongshanren/capture-screenshot)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "ia" 'org-insert-src-block
  "iS" 'capture-screenshot
  )

(spacemacs/set-leader-keys
  "aon" 'org-noter
  "aod" 'org-archive-done-tasks
  )

;; org ref
;; how to add new bibtex entry
;; 1. drag a pdf into bib file
;; 2. ....
(spacemacs/declare-prefix-for-mode 'bibtex-mode "ml" "add-entry")
(spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
  "lr" 'crossref-add-bibtex-entry)

;; thing edit
(spacemacs/set-leader-keys "k" 'one-key-menu-thing-edit)
(spacemacs/set-leader-keys "rr" 'one-key-menu-thing-edit-replace)

;; re-builder, a nice interactive tool for building regular expressions
(spacemacs/set-leader-keys "oR" 're-builder)

;;bbyac
(spacemacs/declare-prefix "oe" "expand/english/duplicate")
(spacemacs/set-leader-keys "oee" 'bbyac-expand-symbols)
(spacemacs/set-leader-keys "oes" 'bbyac-expand-substring)
(spacemacs/set-leader-keys "oel" 'bbyac-expand-lines)
(spacemacs/set-leader-keys "oeh" 'hippie-expand)
(spacemacs/set-leader-keys "oew" 'powerthesaurus-lookup-word-at-point)
(spacemacs/set-leader-keys "oeW" 'powerthesaurus-lookup-word)
(spacemacs/set-leader-keys "oem" 'mw-thesaurus-lookup-at-point)
(spacemacs/set-leader-keys "oed" 'crux-duplicate-current-line-or-region)
(spacemacs/set-leader-keys "oeD" 'crux-duplicate-and-comment-current-line-or-region)
(define-key mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)

;; evil avy
(spacemacs/set-leader-keys "jj" 'evil-avy-goto-char-2)

;; multi-cursor
;; space v 选中，m/e
(spacemacs/set-leader-keys "om" 'multiple-cursors-hydra/body)

;; langtool
(spacemacs/set-leader-keys
  "oen" 'langtool-goto-next-error
  "oep" 'langtool-goto-previous-error
  "oec" 'langtool-correct-buffer
  "oet" 'langtool-check
  "oeT" 'langtool-check-done
  "oei" 'langtool-show-message-at-point
  )

;; awesome tabs
(defhydra awesome-fast-switch (:hint nil)
  "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
  ("h" awesome-tab-backward-tab)
  ("j" awesome-tab-forward-group)
  ("k" awesome-tab-backward-group)
  ("l" awesome-tab-forward-tab)
  ("0" my-select-window)
  ("1" my-select-window)
  ("2" my-select-window)
  ("3" my-select-window)
  ("4" my-select-window)
  ("5" my-select-window)
  ("6" my-select-window)
  ("7" my-select-window)
  ("8" my-select-window)
  ("9" my-select-window)
  ("C-a" awesome-tab-select-beg-tab)
  ("C-e" awesome-tab-select-end-tab)
  ("C-j" awesome-tab-ace-jump)
  ("C-h" awesome-tab-move-current-tab-to-left)
  ("C-l" awesome-tab-move-current-tab-to-right)
  ("b" ivy-switch-buffer)
  ("g" awesome-tab-counsel-switch-group)
  ("C-k" kill-current-buffer)
  ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
  ("q" nil "quit"))

(spacemacs/set-leader-keys "bj" 'awesome-tab-ace-jump)
(spacemacs/set-leader-keys "bJ" 'awesome-fast-switch/body)

;; counsel
(spacemacs/set-leader-keys "ss" 'counsel-grep-or-swiper)

;; separedit
(spacemacs/set-leader-keys "oE" 'separedit)

;; shiftless
(defun insert-number-0 () (interactive) (insert ")"))
(defun insert-number-1 () (interactive) (insert "!"))
(defun insert-number-2 () (interactive) (insert "@"))
(defun insert-number-3 () (interactive) (insert "#"))
(defun insert-number-4 () (interactive) (insert "$"))
(defun insert-number-5 () (interactive) (insert "%"))
(defun insert-number-6 () (interactive) (insert "^"))
(defun insert-number-7 () (interactive) (insert "&"))
(defun insert-number-8 () (interactive) (insert "*"))
(defun insert-number-9 () (interactive) (insert "("))
(defun insert-number-slash () (interactive) (insert "?"))
(defun insert-number-colon () (interactive) (insert ":"))
(defun insert-number-plus () (interactive) (insert "+"))
(defun insert-number-close-curly-1 () (interactive) (insert "{"))
(defun insert-number-close-curly-2 () (interactive) (insert "}"))

(defhydra hydra-insert-symbols (global-map ";")
  "INSERT symbols"
  ("1" insert-number-1 "!")
  ("2" insert-number-2 "@")
  ("3" insert-number-3 "#")
  ("4" insert-number-4 "$")
  ("5" insert-number-5 "5")
  ("6" insert-number-6 "^")
  ("7" insert-number-7 "&")
  ("8" insert-number-8 "*")
  ("9" insert-number-9 "(")
  ("0" insert-number-0 ")")
  ("-" insert-number-minus "+")
  ("/" insert-number-slash "?")
  (";" insert-number-colon ":")
  ("[" insert-number-close-curly-1 "{")
  ("]" insert-number-close-curly-2 "}")
  ("DEL" hungry-delete-backward "delete")
  ("RET" newline-and-indent "enter" )
  ("q" nil "quit"))

;; insert
(spacemacs/declare-prefix "oI" "insert")
(spacemacs/set-leader-keys
  "oI;" 'zilongshanren/insert-semicolon-at-the-end-of-this-line
  "oI:" 'zilongshanren/delete-semicolon-at-the-end-of-this-line
  "oI," 'zilongshanren/insert-comma-at-the-end-of-this-line
  "oI<" 'zilongshanren/delete-comma-at-the-end-of-this-line
  "oI0" 'zilongshanren/insert-bracket-at-the-end-of-this-line
  "oI)" 'zilongshanren/delete-bracket-at-the-end-of-this-line
  )

;; Frame
(spacemacs/declare-prefix "F" "frame")

;; compile
(spacemacs/set-leader-keys
  "cc" 'ivy-taskrunner
  "cr" 'ivy-taskrunner-rerun-last-command
  "cR" 'recompile
  "ca" 'ivy-taskrunner-config-files
  )

;; dash
(spacemacs/set-leader-keys "oz" 'counsel-dash-at-point)

;; kaomoji
(defalias 'insert-kaomoji 'kaomoji)
(spacemacs/set-leader-keys
  "im" 'insert-kaomoji-into-kill-ring
  "iM" 'insert-kaomoji)

;; inline-replace
(spacemacs/set-leader-keys "srr" 'inline-or-region-replace)

;; english teacher
(spacemacs/set-leader-keys "ott" 'english-teacher-follow-mode)

;; counsel etags
(spacemacs/set-leader-keys
  "sac" 'counsel-etags-find-tag-at-point
  "saC" 'counsel-etags-find-tag
  "sal" 'counsel-etags-list-tag
  )

;; ivy/helm-posframe
(spacemacs/set-leader-keys
  "otf" 'enable-ivy/helm-posframe
  "otF" 'disable-ivy/helm-posframe
  )

;; notdeft
;; Press TAB to enter a search query
;; for more doc: https://tero.hasu.is/notdeft/
(spacemacs/declare-prefix "an" "deft")
(spacemacs/set-leader-keys
  "ann" 'notdeft
  "anh" 'notdeft-mode-hydra/body
  "anf" 'notdeft-open-query
  "ani" 'notdeft-insert-org-link
  "anI" 'notdeft-org-link-new-file)

;; tmux-pane
(spacemacs/set-leader-keys
  "wpv" 'tmux-pane-open-vertical
  "wph" 'tmux-pane-open-horizontal
  "wpl" 'tmux-pane-toggle-vertical
  "wpj" 'tmux-pane-toggle-horizontal
  "wpq" 'tmux-pane-close)
