;; -*- coding: utf-8; lexical-binding: t; -*-

(define-key global-map [(shift return)] 'zilongshanren/smart-open-line)

(define-key global-map (kbd "<f9>") 'org-capture)
(define-key global-map (kbd "<f8>") 'zilongshanren/show-current-buffer-major-mode)
(define-key global-map (kbd "<f5>") 'zilongshanren/run-current-file)
(define-key global-map (kbd "<f1>") 'zilongshanren/helm-hotspots)

;; fill-paragraph
(global-set-key (kbd "M-/") #'endless/fill-or-unfill)

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

;; work in visual, and visual line modes
(evil-define-motion my/evil-next-visual-line (count)
  "Move the cursor COUNT screen lines down, or 5."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count 5))))

(evil-define-motion my/evil-previous-visual-line (count)
  "Move the cursor COUNT screen lines down, or 5."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count -5))))

(define-key evil-visual-state-map (kbd "J") 'my/evil-next-visual-line)
(define-key evil-visual-state-map (kbd "K") 'my/evil-previous-visual-line)

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

(evil-define-key 'motion 'global-map (kbd "H") 'evil-first-non-blank)
(evil-define-key 'motion 'global-map (kbd "L") 'evil-end-of-line)

(with-eval-after-load 'company
  (bb/define-key company-active-map
    (kbd "C-w") 'evil-delete-backward-word))

(spacemacs/declare-prefix "ot" "Toggle")

(spacemacs/set-leader-keys "oS" 'zilongshanren/helm-hotspots)
;; (spacemacs/set-leader-keys "oc" 'my-auto-update-tags-when-save)
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
(spacemacs/set-leader-keys "gT" 'my-git-timemachine)
(spacemacs/set-leader-keys "gO" 'browse-repo-at-remote)

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
(spacemacs/set-leader-keys "fo" 'open-file-with-eaf)
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
(spacemacs/set-leader-keys "aeq" 'eaf-kill-process)
(spacemacs/set-leader-keys "aes" 'eaf-search-it)
(spacemacs/set-leader-keys "aef" 'eaf-file-browser-qrcode)
(spacemacs/set-leader-keys "otb" 'change-browser-function)

(spacemacs/set-leader-keys-for-major-mode 'eaf-edit-mode
  "c" 'eaf-edit-buffer-confirm
  "k" 'eaf-edit-buffer-cancel
  "o" 'eaf-edit-buffer-switch-to-org-mode
  )

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
(spacemacs/set-leader-keys "atf" 'screenshot)
(spacemacs/set-leader-keys "atF" 'screenshot-clip)

;; highlight todo and similar keywords
(spacemacs/declare-prefix "oh" "hl-todo/highlight")
(spacemacs/set-leader-keys "ohp" 'hl-todo-previous)
(spacemacs/set-leader-keys "ohn" 'hl-todo-next)
(spacemacs/set-leader-keys "oho" 'hl-todo-occur)
(spacemacs/set-leader-keys "ohi" 'hl-todo-insert)
(spacemacs/set-leader-keys "ohl" 'ivy-magit-todos)
(spacemacs/set-leader-keys "ohh" 'zilongshanren/highlight-dwim)
(spacemacs/set-leader-keys "ohc" 'zilongshanren/clearn-highlight)

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
  "k" 'org-ctrl-c-ctrl-c
  "iT" 'org-set-tags-command
  "1"  'org-cycle-num-bullet
  "2"  'org-add-checkbox
  "r" 'avy-org-refile-as-child
  "it" 'counsel-org-tag
  "tl" 'org-toggle-link-display
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
(define-key evil-normal-state-map (kbd "zk") 'one-key-menu-avy-thing-edit)
(define-key evil-normal-state-map (kbd "zK") 'one-key-menu-avy-thing-edit-replace)
(define-key evil-normal-state-map (kbd "zs") 'avy-thing-copy-symbol)
(define-key evil-insert-state-map (kbd "C-'") 'avy-thing-copy-symbol)

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
(evil-define-key '(normal visual) 'global-map (kbd "gt") 'awesome-fast-switch/body)

;; counsel
(spacemacs/set-leader-keys "ss" 'counsel-grep-or-swiper)

;; separedit
(spacemacs/set-leader-keys "oE" 'separedit)

;; shiftless
(defmacro shift-insert-number (src dst)
  `(defun ,(intern (format "shift-insert-number-%s" src) ) ()
     ,(format "insert %s" dst)
     (interactive)
     (insert ,dst)))

(shift-insert-number 0 ")")
(shift-insert-number 1 "!")
(shift-insert-number 2 "@")
(shift-insert-number 3 "#")
(shift-insert-number 4 "$")
(shift-insert-number 5 "%")
(shift-insert-number 6 "^")
(shift-insert-number 7 "&")
(shift-insert-number 8 "*")
(shift-insert-number 9 "(")
(shift-insert-number "slash" "?")
(shift-insert-number "colon" ":")
(shift-insert-number "plus" "+")
(shift-insert-number "minus" "_")
(shift-insert-number "close-curly-1" "{")
(shift-insert-number "close-curly-2" "}")

(defhydra hydra-insert-symbols (:hint nil)
  "insert symbols"
  ("a" shift-insert-number-1 "!")
  ("s" shift-insert-number-2 "@")
  ("d" shift-insert-number-3 "#")
  ("f" shift-insert-number-4 "$")
  ("g" shift-insert-number-5 "5")
  ("h" shift-insert-number-6 "^")
  ("j" shift-insert-number-7 "&")
  ("k" shift-insert-number-8 "*")
  ("l" shift-insert-number-9 "(")
  ("0" shift-insert-number-0 ")")
  ("-" shift-insert-number-minus "_")
  ("=" shift-insert-number-plus "+")
  ("/" shift-insert-number-slash "?")
  (";" shift-insert-number-colon ":")
  ("[" shift-insert-number-close-curly-1 "{")
  ("]" shift-insert-number-close-curly-2 "}")
  ("DEL" hungry-delete-backward "delete")
  ("RET" newline-and-indent "enter" )
  ("q" nil "quit"))

(defmacro insert-number (src dst)
  `(defun ,(intern (format "insert-number-%s" src)) ()
    ,(format "insert %s" dst)
    (interactive)
    (insert , dst)))

(insert-number 0 "0")
(insert-number 1 "1")
(insert-number 2 "2")
(insert-number 3 "3")
(insert-number 4 "4")
(insert-number 5 "5")
(insert-number 6 "6")
(insert-number 7 "7")
(insert-number 8 "8")
(insert-number 9 "9")

(defhydra hydra-insert-numbers (:hint nil)
  "insert numbers"
  ("a" insert-number-1 "1")
  ("s" insert-number-2 "2")
  ("d" insert-number-3 "3")
  ("f" insert-number-4 "4")
  ("g" insert-number-5 "5")
  ("h" insert-number-6 "6")
  ("j" insert-number-7 "7")
  ("k" insert-number-8 "8")
  ("l" insert-number-9 "9")
  (";" insert-number-0 "0")
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
  "ans" 'notdeft-open-query
  "ani" 'notdeft-insert-org-link
  "anI" 'notdeft-org-link-new-file)

;; tmux-pane
(spacemacs/set-leader-keys
  "wpv" 'tmux-pane-open-vertical
  "wph" 'tmux-pane-open-horizontal
  "wpl" 'tmux-pane-toggle-vertical
  "wpj" 'tmux-pane-toggle-horizontal
  "wpq" 'tmux-pane-close)

;; zeal
(spacemacs/set-leader-keys "oZ" 'zeal-at-point)

;; helpful
(spacemacs/set-leader-keys
  "hdk" #'helpful-key
  "hdf" #'helpful-callable
  "hdv" #'helpful-variable)

;; go-translate
(spacemacs/set-leader-keys
  "og" #'go-translate-popup
  "oG" #'go-translate)

;; wttrin
(spacemacs/set-leader-keys "otw" 'wttrin)

;; music, podcaster
;; (spacemacs/set-leader-keys "atm" 'mingus)
;; (spacemacs/set-leader-keys "atp" 'podcaster)

;; helm-chrome-bookmark
(spacemacs/set-leader-keys "obg" 'helm-chrome-bookmarks)

;; mermaid
(spacemacs/set-leader-keys-for-major-mode 'mermaid-mode
  "c" 'mermaid-compile
  "r" 'mermaid-compile-region
  "d" 'mermaid-open-doc
  "b" 'mermaid-open-browser
  "="  'mermaid-indent-line)

;; treemacs
(spacemacs/set-leader-keys "fT" 'treemacs-display-current-project-exclusively)
(spacemacs/set-leader-keys "pT" 'treemacs-add-and-display-current-project)

;; hydra-org-clock
(defhydra hydra-org-clock (:color pink :hint nil)
"
org-clock hydra key

clock                             ^^^^effort             ^^watcher
-------------------------------^^^^^^^---------------------------------
[_i_]  clock in     [_c_]  cancel     [_e_] set effort     [_t_] toggle
[_L_]  clock last   [_o_]  clock out  [_E_] reset effort   [_s_] start
[_r_]  resolve                                         ^^^^[_S_] stop
[_g_]  goto                                            ^^^^[_w_] status
[_J_]  jump2current                                    ^^^^[_O_] open plan

[_q_] cancel
"
      ("i" org-clock-in)
      ("o" org-clock-out :exit t)
      ("r" org-resolve-clocks :exit t)
      ("g" org-clock-goto :exit t)
      ("J" spacemacs/org-clock-jump-to-current-clock :exit t)
      ("c" org-clock-cancel :exit t)
      ("L" org-clock-in-last)
      ("e" org-set-effort :exit t)
      ("E" org-clock-modify-effort-estimate :exit t)
      ("t" org-clock-watch-toggle :exit t)
      ("s" (org-clock-watch-toggle 'on) :exit t)
      ("S" (org-clock-watch-toggle 'off) :exit t)
      ("w" (org-clock-watch-status))
      ("O" org-clock-watch-goto-work-plan)
      ("q" nil :color blue))

(spacemacs/set-leader-keys "oc" 'hydra-org-clock/body)
(evil-define-key '(normal visual) 'global-map (kbd "go") 'hydra-org-clock/body)

;; cmake-mode
(spacemacs/declare-prefix-for-mode 'cmake-mode
  "mh" "docs")
(spacemacs/set-leader-keys-for-major-mode 'cmake-mode
  "hd" 'cmake-help)

;; pandoc
(spacemacs/set-leader-keys "atp" 'spacemacs/run-pandoc)

;; gk-habit
(spacemacs/declare-prefix "ah" "habit")
(spacemacs/set-leader-keys
  "ahi" 'gkh-init
  "ahn" 'gkh-new
  "ahr" 'gkh-record
  "aha" 'gkh-archive
  "ahd" 'gkh-org-table-display
  "ahD" 'gkh-delete
  "ahc" 'gkh-report-current-week)

(spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
  "f" 'org-ref-bibtex-file/body
  "=" 'org-ref-bibtex-file/bibtex-reformat-and-exit
  "P" 'org-ref-eaf-open-bibtex-pdf)

;; lsp
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "vl" 'lsp-workon
  "vc" 'current-pyvenv-name)

;; ivy
(define-key ivy-minibuffer-map (kbd "C-RET") #'ivy-call)
(define-key ivy-minibuffer-map (kbd "C-d") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "C-o") #'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "C-.") #'hydra-ivy/body)

;; search engine
(spacemacs/set-leader-keys "as" 'spacemacs/search-engine-select)

;; telega
(spacemacs/declare-prefix "ac" "chat")
(spacemacs/set-leader-keys
  "act" 'telega
  "acc" 'telega-chat-with
  "acb" 'telega-switch-buffer
  "acF" 'telega-buffer-file-send
  "acf" 'telega-chatbuf-attach
  "aca" 'telega-account-switch
  "acw" 'telega-browse-url
  "acs" 'telega-saved-messages
  "aci" 'telega-switch-important-chat)

(spacemacs/set-leader-keys-for-major-mode 'python-mode "Tp" 'lpy-mode)
