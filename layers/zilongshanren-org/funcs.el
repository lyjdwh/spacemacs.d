;; -*- coding: utf-8 -*-

(require 'cl)

;; Screenshot
(defun zilongshanren//insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))

(defun zilongshanren/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (progn
    (setq final-image-full-path (concat basename ".png"))
    ;; (call-process "screencapture" nil nil nil "-s" final-image-full-path)
    (shell-command-to-string
     (concat "import " final-image-full-path))
    (if (executable-find "convert")
        (progn
          (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
          (shell-command-to-string resize-command-str)))
    (zilongshanren//insert-org-or-md-img-link "./" (concat basename ".png")))
  (insert "\n"))

(defun zilongshanren/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun zilongshanren/org-archive-cancel-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file))

(defun zilongshanren/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "typescript" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "mermaid")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun zilong/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun ivy-bibtex-my-publications (&optional arg)
  "Search BibTeX entries authored by “Jane Doe”.

With a prefix ARG, the cache is invalidated and the bibliography reread."
  (interactive "P")
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (ivy-read "BibTeX Items: "
            (bibtex-completion-candidates)
            :initial-input "protein design"
            :caller 'ivy-bibtex
            :action ivy-bibtex-default-action))

(defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
  (let ((bibtex-completion-pdf-open-function
         (lambda (fpath) (start-process "okular" "*ivy-bibtex-okular*" "/usr/bin/okular" fpath))))
    (bibtex-completion-open-pdf keys fallback-action)))

(defun bibtex-completion-open-annotated-pdf (keys)
  (--if-let
	  (-flatten
	   (-map (lambda (key)
		       (bibtex-completion-find-pdf (s-concat key "-annotated")))
	         keys))
	  (-each it bibtex-completion-pdf-open-function)
    (message "No PDF(s) found.")))

(defun org-babel-highlight-result ()
  "Highlight the result of the current source block.
Adapt from `org-babel-remove-result'."
  (interactive)
  (let ((location (org-babel-where-is-src-block-result nil nil)))
    (when location
      (save-excursion
        (goto-char location)
        (when (looking-at (concat org-babel-result-regexp ".*$"))
          (pulse-momentary-highlight-region
           (1+ (match-end 0))
           (progn (forward-line 1) (org-babel-result-end))))))))

(add-hook 'org-babel-after-execute-hook
          (defun org-babel-highlight-result-maybe ()
            (when (eq this-command 'org-ctrl-c-ctrl-c)
              (org-babel-highlight-result))))

(defun org-cycle-num-bullet ()
  "turn the list bullet to 1. "
  (interactive)
  (org-cycle-list-bullet 3))

(defun org-add-checkbox ()
  "add checkbox to current list item"
  (interactive)
  (org-toggle-checkbox '(4)))

(defun gkh-delete ()
  "Delete a habit"
  (interactive)
  (let* ((habits (emacsql gkh-db [:select name :from habit
					                      :where (= status "Active")]))
	     (habit (completing-read "Choose a habit: " habits nil t)))
    (emacsql gkh-db `[:delete
                      :from habit
			          :where (= name ,habit)])
    (gkh-org-table-draw)
    (message "habit %s has been deleted!" habit)))

(defun zilongshanren/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
