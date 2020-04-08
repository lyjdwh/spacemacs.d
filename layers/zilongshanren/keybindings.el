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

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; (define-key 'ivy-occur-grep-mode-map (kbd "C-d") 'evil-scroll-down)

(global-set-key [(shift return)] 'zilongshanren/smart-open-line)
(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point-posframe)
(define-key global-map (kbd "<f9>") 'org-capture)
(define-key global-map (kbd "C-c t") 'org-capture)
(define-key global-map (kbd "<f8>") 'zilongshanren/show-current-buffer-major-mode)

(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c i e") 'spacemacs/auto-yasnippet-expand)
;; http://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab
;; (define-key input-decode-map [?\C-i] [C-i])
;; (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

;; (global-set-key (kbd "C-.") 'company-capf)

;; some easy functions for navigate functions
;;C-M-a beginning-of-defun
;;C-M-e end-of-defun
;;C-M-h mark-defun
(global-set-key (kbd "C-s-h") 'mark-defun)

(global-set-key (kbd "s-l") 'goto-line)
;; (global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "C-`") 'toggle-input-method)
(global-set-key (kbd "s-d") 'zilongshanren/my-mc-mark-next-like-this)
(bind-key* "s-r" 'mc/reverse-regions)
(global-set-key (kbd "<f5>") 'zilongshanren/run-current-file)

;; "http://endlessparentheses.com/transposing-keybinds-in-emacs.html?source=rss"
;; (global-set-key "\C-t" #'transpose-lines)
;; (define-key ctl-x-map "\C-t" #'transpose-chars)

(when (spacemacs/system-is-mac)
 (spacemacs/set-leader-keys "o!" 'zilongshanren/iterm-shell-command))

(global-set-key (kbd "s-s") 'save-buffer)
;; (bind-key* "s-k" 'scroll-other-window-down)
;; (bind-key* "s-j"  'scroll-other-window)
(bind-key* "C-c /" 'company-files)
;; (bind-key* "s-r" 'zilongshanren/browser-refresh--chrome-applescript)
(bind-key* "s-;" 'zilongshanren/insert-semicolon-at-the-end-of-this-line)
(bind-key* "C-s-;" 'zilongshanren/delete-semicolon-at-the-end-of-this-line)
(bind-key* "s-," 'zilongshanren/insert-comma-at-the-end-of-this-line)
;; (bind-key* "C-s-," 'zilongshanren/delete-comma-at-the-end-of-this-line)
(bind-key* "C-c l" 'zilongshanren/insert-chrome-current-tab-url)
(bind-key* "C-=" 'er/expand-region)
(bind-key* "M--" 'zilongshanren/goto-match-paren)
(bind-key* "C-c k" 'which-key-show-top-level)
(bind-key* "s-y" 'aya-expand)
(bind-key* "C-." 'zilongshanren/insert-space-after-point)
(bind-key* "M-i" 'string-inflection-java-style-cycle)
(bind-key* "M-u" 'dakra-upcase-dwim)
(bind-key* "M-l" 'dakra-downcase-dwim)
(bind-key* "M-c" 'dakra-capitalize-dwim)
;; (bind-key* "C-l" 'recenter)


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

(bb/define-key evil-normal-state-map
  "+" 'evil-numbers/inc-at-pt
  "-" 'evil-numbers/dec-at-pt
  "\\" 'evil-repeat-find-char-reverse
  (kbd "DEL") 'evil-repeat-find-char-reverse
  "[s" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
  "]s" (lambda (n) (interactive "p")
         (forward-char) (dotimes (c n nil) (insert " ")) (backward-char (1+ n))))

(bb/define-key ivy-occur-grep-mode-map
  (kbd "C-d") 'evil-scroll-down
  "d" 'ivy-occur-delete-candidate)

(with-eval-after-load 'company
  (progn
    (bb/define-key company-active-map
      (kbd "C-w") 'evil-delete-backward-word)

    (bb/define-key company-active-map
      (kbd "s-w") 'company-show-location)))

(spacemacs/declare-prefix "ot" "Toggle")


(global-set-key (kbd "<f1>") 'zilongshanren/helm-hotspots)
(spacemacs/set-leader-keys "oS" 'zilongshanren/helm-hotspots)

(spacemacs/set-leader-keys "oc" 'my-auto-update-tags-when-save)
;; (spacemacs/set-leader-keys "op" 'zilongshanren/org-save-and-export)
(spacemacs/set-leader-keys "fR" 'zilongshanren/rename-file-and-buffer)

;;Must set key to nil to prevent error: Key sequence b m s starts with non-prefix key b m
(spacemacs/set-leader-keys "bD" 'spacemacs/kill-other-buffers)
(spacemacs/declare-prefix "ob" "Bookmark")
(spacemacs/set-leader-keys "obs" 'bookmark-set)
(spacemacs/set-leader-keys "obr" 'bookmark-rename)
(spacemacs/set-leader-keys "obd" 'bookmark-delete)
(spacemacs/set-leader-keys "obj" 'counsel-bookmark)
(spacemacs/set-leader-keys "obJ" 'bookmark-jump-other-window)
(spacemacs/set-leader-keys "obe" 'edit-bookmarks)
(spacemacs/set-leader-keys "obi" 'bookmark-insert)

(spacemacs/set-leader-keys "od" 'occur-dwim)
(spacemacs/set-leader-keys "ok" 'zilongshanren-kill-other-persp-buffers)
(spacemacs/set-leader-keys "ox" 'org-open-at-point-global)
;; (spacemacs/set-leader-keys "or" 'zilongshanren/browser-refresh--chrome-applescript)

(spacemacs/set-leader-keys "rh" 'helm-resume)
(spacemacs/set-leader-keys "sj" 'counsel-imenu)

(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  "gU" 'xref-find-references)
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
(spacemacs/set-leader-keys "oll" 'zilongshanren/load-my-layout)
(spacemacs/set-leader-keys "ols" 'zilongshanren/save-my-layout)
;; (spacemacs/set-leader-keys "ob" 'popwin:display-last-buffer)
(spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point-posframe)
(spacemacs/set-leader-keys "oY" 'youdao-dictionary-search-from-input)
(spacemacs/set-leader-keys "bM" 'spacemacs/switch-to-messages-buffer)
(spacemacs/set-leader-keys "sS" 'spacemacs/swiper-region-or-symbol)

(bind-key* "s-p" 'find-file-in-project)
(spacemacs/set-leader-keys "os" 'counsel-ag-thing-at-point)

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
(spacemacs/set-leader-keys "aeu" 'eaf-open-url)
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

;; company-english-helper
(spacemacs/set-leader-keys "ote" 'toggle-company-english-helper)

;; rotate-text
(spacemacs/set-leader-keys "or" 'rotate-text)

;; pandoc
(spacemacs/set-leader-keys "op" 'spacemacs/run-pandoc)

;; speed type
(spacemacs/set-leader-keys "at" 'speed-type-text)

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

;; take screenshot
(spacemacs/declare-prefix "as" "shot")
(spacemacs/set-leader-keys "asf" 'screenshot)
(spacemacs/set-leader-keys "asF" 'screenshot-clip)

;; fzf
(spacemacs/set-leader-keys "of" 'counsel-fzf)

;; highlight todo and similar keywords
(spacemacs/declare-prefix "oh" "hl-todo")
(spacemacs/set-leader-keys "ohp" 'hl-todo-previous)
(spacemacs/set-leader-keys "ohn" 'hl-todo-next)
(spacemacs/set-leader-keys "oho" 'hl-todo-occur)
(spacemacs/set-leader-keys "ohi" 'hl-todo-insert)

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
  "otp" 'set-proxy
  "otP" 'unset-proxy
  )

;; google
(spacemacs/set-leader-keys "ag" 'engine/search-google)

;; rime，default: ctrl + \
(spacemacs/set-leader-keys "otr" 'toggle-input-method)

;; lsp
(define-key spacemacs-lsp-mode-map (kbd "hg") 'lsp-ui-doc-glance)

;; helm ag to overhide ivy ag
(spacemacs/set-leader-keys "sad" 'spacemacs/helm-dir-do-ag)
(spacemacs/set-leader-keys "saD" 'spacemacs/helm-dir-do-ag-region-or-symbol)

;; org
(spacemacs/set-leader-keys "ao." 'spacemacs/org-agenda-transient-state/org-agenda-set-tags)

;; ivy-bibex
;; (global-set-key (kbd "C-x p") 'ivy-bibtex-my-publications)
;; press M-o list actions in ivy-bibex
(spacemacs/set-leader-keys "ab" 'ivy-bibtex)

;; org noter
(spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode
  "i" 'org-noter-insert-precise-note
  "c" 'org-noter-create-skeleton
  )
(spacemacs/declare-prefix-for-mode 'org-mode  "mn" "org-noter")
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "nc" 'org-noter-sync-current-note
  "nj" 'org-noter-sync-next-note
  "nk" 'org-noter-sync-prev-note)
(spacemacs/set-leader-keys "aon" 'org-noter)

;; org ref
;; how to add new bibtex entry
;; 1. drag a pdf into bib file
;; 2. ....
(spacemacs/declare-prefix-for-mode 'bibtex-mode "ml" "add-entry")
(spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
  "lr" 'crossref-add-bibtex-entry)

(when (spacemacs/system-is-mswindows)
  (global-set-key (kbd "s-=") 'spacemacs/scale-up-font)
  (spacemacs/set-leader-keys "bf" 'locate-current-file-in-explorer)
  (global-set-key (kbd "s--") 'spacemacs/scale-down-font)
  (global-set-key (kbd "s-0") 'spacemacs/reset-font-size)
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-g") 'evil-avy-goto-char-2)
  (global-set-key (kbd "s-c") 'evil-yank)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo))
