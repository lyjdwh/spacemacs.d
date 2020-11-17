;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defface position-hint-face
  '((t (:foreground "red")))
  "Position hint")

(defvar position-hint-move-function nil)

(defun move-to-position-hint (n)
  (dotimes (_i n)
    (call-interactively position-hint-move-function))
  (highlight-position-hint position-hint-move-function))

(defun move-to-position-hint-1 ()
  (interactive)
  (move-to-position-hint 1))

(defun move-to-position-hint-2 ()
  (interactive)
  (move-to-position-hint 2))

(defun move-to-position-hint-3 ()
  (interactive)
  (move-to-position-hint 3))

(defun move-to-position-hint-4 ()
  (interactive)
  (move-to-position-hint 4))

(defun move-to-position-hint-5 ()
  (interactive)
  (move-to-position-hint 5))

(defun move-to-position-hint-6 ()
  (interactive)
  (move-to-position-hint 6))

(defun move-to-position-hint-7 ()
  (interactive)
  (move-to-position-hint 7))


(defun move-to-position-hint-8 ()
  (interactive)
  (move-to-position-hint 8))


(defun move-to-position-hint-9 ()
  (interactive)
  (move-to-position-hint 9))

(defvar position-hint-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "1") 'move-to-position-hint-1)
    (define-key keymap (kbd "2") 'move-to-position-hint-2)
    (define-key keymap (kbd "3") 'move-to-position-hint-3)
    (define-key keymap (kbd "4") 'move-to-position-hint-4)
    (define-key keymap (kbd "5") 'move-to-position-hint-5)
    (define-key keymap (kbd "6") 'move-to-position-hint-6)
    (define-key keymap (kbd "7") 'move-to-position-hint-7)
    (define-key keymap (kbd "8") 'move-to-position-hint-8)
    (define-key keymap (kbd "9") 'move-to-position-hint-9)
    keymap))

(defun highlight-position-hint (cmd)
  (let (ovs)
    (save-mark-and-excursion
      (cl-loop for i from 1 to 9 do
               (call-interactively cmd)
               (let ((ov (make-overlay (point) (1+ (point)))))
                 (overlay-put ov 'display
                              (cond
                               ((looking-at-p "\n")
                                (format "%d\n" i))
                               (t (format "%d" i))))
                 (overlay-put ov 'face 'position-hint-face)
                 (push ov ovs))))
    (sit-for 1)
    (mapcar #'delete-overlay ovs)
    (setq position-hint-move-function cmd)
    (set-transient-map position-hint-map t (lambda () (setq position-hint-move-function nil)))))

(defun my-forward-word ()
  (interactive)
  (call-interactively #'forward-word)
  (highlight-position-hint #'forward-word))

(defun my-backward-word ()
  (interactive)
  (call-interactively #'evil-backward-word-begin)
  (highlight-position-hint #'evil-backward-word-begin))

(defun my-next-line ()
  (interactive)
  (call-interactively #'evil-next-line)
  (highlight-position-hint #'evil-next-line))

(defun my-previous-line ()
  (interactive)
  (call-interactively #'evil-previous-line)
  (highlight-position-hint #'evil-previous-line))

(define-key evil-normal-state-map (kbd "w") 'my-forward-word)
(define-key evil-visual-state-map (kbd "w") 'my-forward-word)

(define-key evil-normal-state-map (kbd "b") 'my-backward-word)
(define-key evil-visual-state-map (kbd "b") 'my-backward-word)

(provide 'move-to-position-hint)
