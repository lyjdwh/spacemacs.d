;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defface position-hint-face
  '((t (:foreground "red")))
  "Position hint")

(defcustom move-to-position-hint-disabled-modes
  '(org-agenda-mode magit-mode git-rebase-mode elfeed-show-mode
                    elfeed-search-mode ranger-mode magit-repolist-mode
                    undo-tree-visualizer-mode tabulated-list-mode
                    Info-mode calc-mode treemacs-mode ibuffer-mode dired-mode
                    ;; `mu4e'
                    mu4e-main-mode mu4e-view-mode mu4e-headers-mode mu4e~update-mail-mode
                    ;; `notmuch'
                    notmuch-tree-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode)
  "A list of major modes where move-to-position-hint should not activate."
  :type  '(list symbol))

(defvar position-hint-move-function nil)

(defun move-to-position-hint (n)
  (dotimes (_i n)
    (call-interactively position-hint-move-function))

  (unless evil-visual-state-minor-mode
    (highlight-position-hint position-hint-move-function)))

(defmacro move-to-position-hint-num (num)
  `(defun ,(intern (format "move-to-position-hint-%s" num)) ()
     ,(format "move to position %s" num)
     (interactive)
     (move-to-position-hint, num)))

(move-to-position-hint-num 0)
(move-to-position-hint-num 1)
(move-to-position-hint-num 2)
(move-to-position-hint-num 3)
(move-to-position-hint-num 4)
(move-to-position-hint-num 5)
(move-to-position-hint-num 6)
(move-to-position-hint-num 7)
(move-to-position-hint-num 8)
(move-to-position-hint-num 9)
(move-to-position-hint-num 10)
(move-to-position-hint-num 11)
(move-to-position-hint-num 12)
(move-to-position-hint-num 13)
(move-to-position-hint-num 14)
(move-to-position-hint-num 15)
(move-to-position-hint-num 16)
(move-to-position-hint-num 17)
(move-to-position-hint-num 18)
(move-to-position-hint-num 19)
(move-to-position-hint-num 20)
(move-to-position-hint-num 21)
(move-to-position-hint-num 22)
(move-to-position-hint-num 23)
(move-to-position-hint-num 24)
(move-to-position-hint-num 25)
(move-to-position-hint-num 26)

(defvar position-hint-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "0") 'move-to-position-hint-0)
    (define-key keymap (kbd "a") 'move-to-position-hint-1)
    (define-key keymap (kbd "s") 'move-to-position-hint-2)
    (define-key keymap (kbd "d") 'move-to-position-hint-3)
    (define-key keymap (kbd "f") 'move-to-position-hint-4)
    (define-key keymap (kbd "g") 'move-to-position-hint-5)
    (define-key keymap (kbd "h") 'move-to-position-hint-6)
    (define-key keymap (kbd "j") 'move-to-position-hint-7)
    (define-key keymap (kbd "k") 'move-to-position-hint-8)
    (define-key keymap (kbd "l") 'move-to-position-hint-9)
    (define-key keymap (kbd "q") 'move-to-position-hint-10)
    (define-key keymap (kbd "w") 'move-to-position-hint-11)
    (define-key keymap (kbd "e") 'move-to-position-hint-12)
    (define-key keymap (kbd "r") 'move-to-position-hint-13)
    (define-key keymap (kbd "t") 'move-to-position-hint-14)
    (define-key keymap (kbd "y") 'move-to-position-hint-15)
    (define-key keymap (kbd "u") 'move-to-position-hint-16)
    (define-key keymap (kbd "i") 'move-to-position-hint-17)
    (define-key keymap (kbd "o") 'move-to-position-hint-18)
    (define-key keymap (kbd "p") 'move-to-position-hint-19)
    (define-key keymap (kbd "z") 'move-to-position-hint-20)
    (define-key keymap (kbd "x") 'move-to-position-hint-21)
    (define-key keymap (kbd "c") 'move-to-position-hint-22)
    (define-key keymap (kbd "v") 'move-to-position-hint-23)
    (define-key keymap (kbd "b") 'move-to-position-hint-24)
    (define-key keymap (kbd "n") 'move-to-position-hint-25)
    (define-key keymap (kbd "m") 'move-to-position-hint-26)
    keymap))

(defun highlight-position-hint (cmd)
  (let ((ovs)
        (symbols '(0 a s d f g h j k l q w e r t y u i o p z x c v b n m)))
    (save-mark-and-excursion
      (cl-loop for i from 0 to 25 do
               (let ((ov (make-overlay (point) (1+ (point)))))
                 (overlay-put ov 'display
                              (cond
                               ((looking-at-p "\n")
                                (format "%s\n" (nth i symbols)))
                               (t (format "%s" (nth i symbols)))))
                 (overlay-put ov 'face 'position-hint-face)
                 (push ov ovs))
               (call-interactively cmd)
               ))
    (sit-for 1.5)
    (mapcar #'delete-overlay ovs)
    (setq position-hint-move-function cmd)
    (set-transient-map position-hint-map nil)))

(defun my-forward-word ()
  (interactive)
  (call-interactively #'forward-word)
  (highlight-position-hint #'forward-word))

(defun my-backward-word ()
  (interactive)
  (call-interactively #'backward-word)
  (highlight-position-hint #'backward-word))


(defun my-forward-word-end ()
  (interactive)
  (call-interactively #'evil-forward-word-end)
  (highlight-position-hint #'evil-forward-word-end))

(defun my-backward-word-end ()
  (interactive)
  (call-interactively #'evil-backward-word-end)
  (highlight-position-hint #'evil-backward-word-end))

(defvar move-to-position-hint-local-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key* 'motion map
      "w" #'my-forward-word
      "b" #'my-backward-word
      "e" #'my-forward-word-end)
    map))

;;;###autoload
(defun turn-on-move-to-position-hint-mode ()
  "Enable move-to-position-hint-mode in the current buffer."
  (unless (apply #'derived-mode-p move-to-position-hint-disabled-modes)
    (move-to-position-hint-local-mode +1)))

;;;###autoload
(defun turn-off-move-to-position-hint-mode ()
  "Disable `move-to-position-hint-local-mode' in the current buffer."
  (move-to-position-hint-local-mode -1))


(defvar move-to-position-hint--keymaps-init nil)
(defun move-to-position-hint--normalize-keymaps ()
  (unless move-to-position-hint--keymaps-init
    (set (make-local-variable 'move-to-position-hint--keymaps-init)
         (and (evil-normalize-keymaps) t))))

;;;###autoload
(define-minor-mode move-to-position-hint-local-mode
  "Enable `move-to-position-hint' in the current buffer."
  :lighter " snipe"
  :group 'move-to-position-hint
  (if move-to-position-hint-local-mode (move-to-position-hint--normalize-keymaps)))

;;;###autoload
(define-globalized-minor-mode move-to-position-hint-mode
  move-to-position-hint-local-mode turn-on-move-to-position-hint-mode)

(provide 'move-to-position-hint)
