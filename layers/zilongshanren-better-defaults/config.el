;; -*- coding: utf-8 -*-
(defmacro th/define-context-key (keymap key dispatch)
  "Define KEY in KEYMAP to execute according to DISPATCH.

        DISPATCH is a form that is evaluated and should return the
        command to be executed.

        If DISPATCH returns nil, then the command normally bound to KEY
        will be executed.

        Example:

          (th/define-context-key hs-minor-mode-map
             (kbd \"<C-tab>\")
             (cond
              ((not (hs-already-hidden-p))
               'hs-hide-block)
              ((hs-already-hidden-p)
               'hs-show-block)))

        This will make <C-tab> show a hidden block.  If the block is
        shown, then it'll be hidden."
  `(define-key ,keymap ,key
     `(menu-item "context-key" ignore
                 :filter ,(lambda (&optional ignored)
                            ,dispatch))))

(global-prettify-symbols-mode 1)

(setq large-file-warning-threshold 100000000)
(defun spacemacs/check-large-file ()
  (when (> (buffer-size) 500000)
    (progn (fundamental-mode)
           (hl-line-mode -1)))
  (if (and (executable-find "wc")
           (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
              5000))
      nil))

(add-hook 'find-file-hook 'spacemacs/check-large-file)

(add-hook 'minibuffer-inactive-mode-hook
          #'(lambda() (set (make-local-variable 'semantic-mode) nil)
              (set (make-local-variable 'electric-pair-mode) nil)))

;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun zilongshanren/stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'zilongshanren/stop-using-minibuffer)

(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; Remove useless whitespace before saving a fil
(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)
