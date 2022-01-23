;; -*- coding: utf-8 -*-

;;http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun zilongshanren/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (deactivate-mark)
  (call-interactively 'occur))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

;; Remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.

The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(defun change-browser-function ()
  "change the browser for emacs"
  (interactive)
  (if (equal browse-url-browser-function 'eaf-open-browser)
      (progn
        (setq browse-url-browser-function 'browse-url-default-browser)
        (message "use default browser"))
    (progn
      (setq browse-url-browser-function 'eaf-open-browser)
      (message "use eaf as browser"))
    ))

(defun eaf--browser-get-window ()
  (get-window-with-predicate
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (string= eaf--buffer-app-name "browser")))))

(defun eaf--browser-display (buf)
  (let ((browser-window (eaf--browser-get-window)))
    (select-window (or browser-window (split-window-no-error (selected-window) nil 'left)))
    (switch-to-buffer buf)))
