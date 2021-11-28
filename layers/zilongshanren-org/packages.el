;; -*- coding: utf-8 -*-

(defconst zilongshanren-org-packages
  '(
    (org :location built-in)
    org-pomodoro
    (notdeft :location local)
    org-roam
    (org-transclusion :location (recipe :fetcher github :repo "nobiot/org-transclusion" :files ("*")))
    org-roam-bibtex
    ivy-bibtex
    org-noter
    org-ref
    org-super-agenda
    (org-clock-watch :location (recipe :fetcher github :repo "wztdream/org-clock-watch" :files ("*")))
    (gkhabit :location (recipe :fetcher github :repo "Kinneyzhang/gkhabit"))
    (popweb :location (recipe :fetcher github :repo "manateelazycat/popweb" :files ("*.*")))
    (popweb-latex :location (recipe :fetcher github :repo "manateelazycat/popweb" :files ("extension/latex/*")))
    easy-hugo
    ox-hugo
    iscroll
    cdlatex
    xenops
    org-roam-ui
    aas
    laas
    ))

(defun zilongshanren-org/init-aas()
  (use-package aas
    :hook (LaTeX-mode . aas-activate-for-major-mode)
    :hook (org-mode . aas-activate-for-major-mode)
    ))

(defun zilongshanren-org/init-laas()
  (use-package laas
    :hook (LaTeX-mode . laas-mode)
    :hook (org-mode . laas-mode)
    :config
    (aas-set-snippets 'laas-mode
      "mk" (lambda () (interactive)
             (yas-expand-snippet "\\$$0$"))

      "dm" (lambda () (interactive)
             (yas-expand-snippet "\\[\n $0 \n \\]"))
      ;; set condition!
      :cond #'texmathp ; expand only while in math
      "supp" "\\supp"
      "On" "O(n)"
      "O1" "O(1)"
      "Olog" "O(\\log n)"
      "Olon" "O(n \\log n)"
      ;; bind to functions!
      "Sum" (lambda () (interactive)
              (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
      "Span" (lambda () (interactive)
               (yas-expand-snippet "\\Span($1)$0"))
      ;; add accent snippets
      :cond #'laas-object-on-left-condition
      "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt")))
    ))

(defun zilongshanren-org/init-xenops()
  (use-package xenops
    :hook (LaTeX-mode . xenops-mode)
    :hook (org-mode . xenops-mode)
    ))

(defun zilongshanren-org/init-cdlatex()
  (use-package cdlatex
    :hook (LaTeX-mode . turn-on-cdlatex)
    :hook (org-mode . turn-on-org-cdlatex)
    :config
    ;; Array/tabular input with org-tables and cdlatex
    (use-package org-table
      :bind (:map orgtbl-mode-map
                  ("<tab>" . lazytab-org-table-next-field-maybe)
                  ("TAB" . lazytab-org-table-next-field-maybe))
      :init
      (add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
      ;; Tabular environments using cdlatex
      (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
                                            "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                                            lazytab-position-cursor-and-edit
                                            nil nil t))
      (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
                                            "\\begin{bmatrix} ? \\end{bmatrix}"
                                            lazytab-position-cursor-and-edit
                                            nil nil t))
      (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
                                            "\\begin{pmatrix} ? \\end{pmatrix}"
                                            lazytab-position-cursor-and-edit
                                            nil nil t))
      (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
                                            "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                                             lazytab-position-cursor-and-edit
                                             nil t nil))
      (spacemacs/set-leader-keys-for-major-mode 'latex-mode "d" 'orgtbl-ctrl-c-ctrl-c)
      :config
      ;; Tab handling in org tables
      (defun lazytab-position-cursor-and-edit ()
        ;; (if (search-backward "\?" (- (point) 100) t)
        ;;     (delete-char 1))
        (cdlatex-position-cursor)
        (lazytab-orgtbl-edit))

      (defun lazytab-orgtbl-edit ()
        (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
        (orgtbl-mode 1)
        (open-line 1)
        (insert "\n|"))

      (defun lazytab-orgtbl-replace (_)
        (interactive "P")
        (unless (org-at-table-p) (user-error "Not at a table"))
        (let* ((table (org-table-to-lisp))
               params
               (replacement-table
                (if (texmathp)
                    (lazytab-orgtbl-to-amsmath table params)
                  (orgtbl-to-latex table params))))
          (kill-region (org-table-begin) (org-table-end))
          (open-line 1)
          (push-mark)
          (insert replacement-table)
          (align-regexp (region-beginning) (region-end) "\\([:space:]*\\)& ")
          (orgtbl-mode -1)
          (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)))

      (defun lazytab-orgtbl-to-amsmath (table params)
        (orgtbl-to-generic
         table
         (org-combine-plists
          '(:splice t
                    :lstart ""
                    :lend " \\\\"
                    :sep " & "
                    :hline nil
                    :llend "")
          params)))

      (defun lazytab-cdlatex-or-orgtbl-next-field ()
        (when (and (bound-and-true-p orgtbl-mode)
                   (org-table-p)
                   (looking-at "[[:space:]]*\\(?:|\\|$\\)")
                   (let ((s (thing-at-point 'sexp)))
                     (not (and s (assoc s cdlatex-command-alist-comb)))))
          (call-interactively #'org-table-next-field)
          t))

      (defun lazytab-org-table-next-field-maybe ()
        (interactive)
        (if (bound-and-true-p cdlatex-mode)
            (cdlatex-tab)
          (org-table-next-field))))

    ;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas
    ;; fields
   (use-package yasnippet
     :ensure t
     :hook (LaTeX-mode . yas-minor-mode)
     :hook ((cdlatex-tab . yas-expand)
            (cdlatex-tab . cdlatex-in-yas-field))
     :bind (:map yas-keymap
                 ("<tab>" . yas-next-field-or-cdlatex)
                 ("TAB" . yas-next-field-or-cdlatex))
     :config
     (setq yas-triggers-in-field t)

     (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
       (when-let* ((_ (overlayp yas--active-field-overlay))
                   (end (overlay-end yas--active-field-overlay)))
         (if (>= (point) end)
             ;; Call yas-next-field if cdlatex can't expand here
             (let ((s (thing-at-point 'sexp)))
               (unless (and s (assoc (substring-no-properties s)
                                     cdlatex-command-alist-comb))
                 (yas-next-field-or-maybe-expand)
                 t))
          ;; otherwise expand and jump to the correct location
           (let (cdlatex-tab-hook minp)
             (setq minp
                   (min (save-excursion (cdlatex-tab)
                                        (point))
                        (overlay-end yas--active-field-overlay)))
             (goto-char minp) t))))

     (defun yas-next-field-or-cdlatex ()
       (interactive)
       "Jump to the next Yas field correctly with cdlatex active."
       (if (bound-and-true-p cdlatex-mode)
           (cdlatex-tab)
         (yas-next-field-or-maybe-expand))))
   ))

(defun zilongshanren-org/init-iscroll()
  (use-package iscroll
    :hook (org-mode . iscroll-mode)
    :config
    (evil-define-motion iscroll-evil-next-line (count)
      "Move the cursor COUNT lines down."
      :type line
      (let (line-move-visual)
        (iscroll-evil-line-move (or count 1))))

    (evil-define-motion iscroll-evil-previous-line (count)
      "Move the cursor COUNT lines up."
      :type line
      (let (line-move-visual)
        (iscroll-evil-line-move (- (or count 1)))))

    (evil-define-motion iscroll-evil-next-visual-line (count)
      "Move the cursor COUNT screen lines down."
      :type exclusive
      (let ((line-move-visual t))
        (iscroll-evil-line-move (or count 1))))

    (evil-define-motion iscroll-evil-previous-visual-line (count)
      "Move the cursor COUNT screen lines up."
      :type exclusive
      (let ((line-move-visual t))
        (iscroll-evil-line-move (- (or count 1)))))

    (defun iscroll-evil-line-move (count &optional noerror)
      "A wrapper for line motions which conserves the column.
  Signals an error at buffer boundaries unless NOERROR is non-nil."
      (cond
       (noerror
        (condition-case nil
            (evil-line-move count)
          (error nil)))
       (t
        (evil-signal-without-movement
          (setq this-command (if (>= count 0)
                                 #'iscroll-next-line
                               #'iscroll-previous-line))
          (let ((opoint (point)))
            (condition-case err
                (with-no-warnings
                  (funcall this-command (abs count)))
              ((beginning-of-buffer end-of-buffer)
               (let ((col (or goal-column
                              (if (consp temporary-goal-column)
                                  (car temporary-goal-column)
                                temporary-goal-column))))
                 (if line-move-visual
                     (vertical-motion (cons col 0))
                   (line-move-finish col opoint (< count 0)))
                 ;; Maybe we should just `ding'?
                 (signal (car err) (cdr err))))))))))

    (define-advice iscroll-mode (:after (&optional arg) yang)
      "Add evil keybinings for iscroll."
      (if iscroll-mode
          (progn
            (evil-global-set-key 'normal (kbd "j") #'iscroll-evil-next-line)
            (evil-global-set-key 'normal (kbd "k") #'iscroll-evil-previous-line)
            (evil-global-set-key 'visual (kbd "j") #'iscroll-evil-next-visual-line)
            (evil-global-set-key 'visual (kbd "k") #'iscroll-evil-previous-visual-line))
        (global-set-key [remap evil-next-line] nil)
        (global-set-key [remap evil-previous-line] nil)
        (global-set-key [remap evil-next-visual-line] nil)
        (global-set-key [remap evil-previous-visual-line] nil)))
    ))

(defun zilongshanren-org/post-init-ox-hugo()
  (setq org-hugo-section "post")
  )

(defun zilongshanren-org/init-easy-hugo()
  (use-package easy-hugo
    :commands easy-hugo
    :config
    (setq easy-hugo-basedir "~/org-notes/blog/")
    (setq easy-hugo-postdir "content/post")
    (setq easy-hugo-image-directory "static/img")
    (setq easy-hugo-helm-ag t)
    (setq easy-hugo-url "")
    ))

(defun zilongshanren-org/init-popweb()
  (use-package popweb
    :commands popweb-latex-mode
    ))

(defun zilongshanren-org/init-popweb-latex()
  (use-package popweb-latex
    :init
    (spacemacs/set-leader-keys "otl" 'popweb-latex-mode)
    :commands popweb-latex-mode
    ))

(defun zilongshanren-org/init-gkhabit ()
  (use-package gkhabit
    :commands gkh-init gkh-new gkh-record gkh-archive gkh-org-table-display gkh-delete gkh-report-current-week
    :config
    (setq gkh-file "~/org-notes/habit.org")
    ))

(defun zilongshanren-org/init-org-clock-watch ()
  (use-package org-clock-watch
    :if (not (equal (framep-on-display) t))
    :after org
    :config
    (setq org-clock-watch-work-plan-file-path "~/org-notes/gtd.org")
    ;; (org-clock-watch-toggle 'on)
    ))

(defun zilongshanren-org/init-org-super-agenda ()
  (use-package org-super-agenda
    :after org-agenda
    :config
    (setq org-super-agenda-groups
          '((:name "Today"
                   :time-grid t
                   :todo "TODAY")
            (:name "Important"
                   :priority "A")
            (:name "Quick Picks"
                   :effort< "0:30")
            (:name "Next Items"
                   :tag "next")
            (:priority<= "B"
                         :scheduled future)))
    (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)
    ))

(defun zilongshanren-org/init-notdeft ()
  (use-package notdeft
    :load-path "~/bin/notdeft"
    :commands notdeft notdeft-mode-hydra/body notdeft-open-query notdeft-insert-org-link notdeft-org-link-existing-note notdeft-org-link-new-file
    :config
    (setq notdeft-directories '("~/org-notes/notes"))
    (setq notdeft-secondary-extensions '("md" "txt"))
    (setq notdeft-xapian-max-results 0)
    (setq notdeft-allow-org-property-drawers t)
    (add-to-list 'load-path "~/bin/notdeft/extras")
    (load "notdeft-example")
    ))

(defun zilongshanren-org/post-init-org-ref ()
  (progn
    ;; see org-ref for use of these variables
    (setq org-ref-bibliography-notes "~/org-notes/notes/bib-notes.org"
          org-ref-default-bibliography '("~/Documents/papers/bib/protein_design.bib" "~/Documents/papers/bib/mendeley.bib")
          org-ref-pdf-directory "~/Documents/papers")

    (add-hook 'bibtex-mode-hook (lambda () (require 'org-ref)))
    (with-eval-after-load 'org-ref
      (defun org-ref-open-bibtex-pdf ()
        "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
        (interactive)
        (save-excursion
          (bibtex-beginning-of-entry)
          (let* ((bibtex-expand-strings t)
                 (entry (bibtex-parse-entry t))
                 (pdf-path (reftex-get-bib-field "file" entry))
                 )
            (if (file-exists-p pdf-path)
                (start-process "okular" "*bibtex-okular*" "/usr/bin/okular" pdf-path)
              (message "%s doesn't exist" pdf-path))
            )))

      (defun org-ref-eaf-open-bibtex-pdf ()
        "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
        (interactive)
        (save-excursion
          (bibtex-beginning-of-entry)
          (let* ((bibtex-expand-strings t)
                 (entry (bibtex-parse-entry t))
                 (pdf-path (reftex-get-bib-field "file" entry))
                 )
            (if (file-exists-p pdf-path)
                (eaf-open pdf-path)
              (message "%s doesn't exist" pdf-path))
            )))

      (defun org-ref-open-pdf-at-point ()
        "Open the pdf for bibtex key under point if it exists."
        (interactive)
        (let* ((results (org-ref-get-bibtex-key-and-file))
               (key (car results))
               (pdf-path (format "%s.pdf" (org-ref-get-mendeley-filename key)))
               )
          (if (file-exists-p pdf-path)
              (start-process "okular" "*bibtex-okular*" "/usr/bin/okular" pdf-path)
            (message "no pdf found for %s" pdf-path))
          ))
      ;; (setq org-ref-notes-function 'org-ref-notes-function-one-file)
      )
    ))

(defun zilongshanren-org/init-org-noter ()
  (use-package org-noter
    :commands org-noter
    :config
    (setq org-noter-default-notes-file-names '("notes.org")
          org-noter-notes-search-path '("~/org-notes/notes/papers"))
    (setq org-noter-separate-notes-from-heading t)
    ;; (setq org-noter-property-doc-file "INTERLEAVE_PDF"
    ;;       org-noter-property-note-location "INTERLEAVE_PAGE_NOTE")
    (setq org-noter-always-create-frame nil)
    (setq org-noter-default-heading-title "Org noters")
    ))

(defun zilongshanren-org/init-ivy-bibtex ()
  (use-package ivy-bibtex
    :after org-roam-bibtex
    :config
    (setq bibtex-completion-bibliography '("~/Documents/papers/bib/protein_design.bib" "~/Documents/papers/bib/mendeley.bib"))
    (setq bibtex-completion-library-path '("~/Documents/papers"))
    (setq bibtex-completion-pdf-field "File")
    (setq bibtex-completion-notes-path "~/org-notes/notes/papers")
	(setq bibtex-completion-display-formats
          '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${tags:6} ${year:4} ${author:36} ${title:*}")
            (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${tags:6} ${year:4} ${author:36} ${title:*}")
            (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${tags:6} ${year:4} ${author:36} ${title:*}")
            (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${tags:6} ${year:4} ${author:36} ${title:*}")
            (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${tags:6} ${year:4} ${author:36} ${title:*}")))
    (setq bibtex-completion-additional-search-fields '(tags))
    (setq bibtex-completion-pdf-symbol "⌘")
    (setq bibtex-completion-notes-symbol "✎")
    (setq bibtex-completion-find-additional-pdfs t)

    (ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)
    (ivy-bibtex-ivify-action bibtex-completion-open-annotated-pdf ivy-bibtex-open-annotated-pdf)
    (ivy-bibtex-ivify-action bibtex-completion-open-pdf ivy-bibtex-open-pdf)

    (ivy-add-actions
     'ivy-bibtex
     '(("p" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")
       ("P" ivy-bibtex-open-pdf "Open PDF file (if present)")
       ("n" ivy-bibtex-open-annotated-pdf "Open annotated PDF (if present)")
       ))
    (setq bibtex-completion-format-citation-functions
          '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
            (latex-mode    . bibtex-completion-format-citation-cite)
            (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
            (default       . bibtex-completion-format-citation-default)))

    (advice-add 'bibtex-completion-candidates
                :filter-return 'reverse)
    ))

(defun zilongshanren-org/init-org-roam ()
  (use-package org-roam
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory deft-dir)
    (org-roam-file-extensions '("org" "md"))
    :init
    (setq org-roam-v2-ack t)
    (progn
      (spacemacs/declare-prefix "am" "org-roam")
      (spacemacs/set-leader-keys
        "amb" 'org-roam-buffer
        "amt" 'org-roam-buffer-toggle
        "amf" 'org-roam-node-find ;; orb-find-non-ref-file
        "amF" 'org-roam-ref-find
        "ami" 'org-roam-node-insert   ;; orb-insert-non-ref
        "amI" 'org-id-get-create
        "ama" 'org-roam-tag-add
        "amd" 'org-roam-tag-remove
        "amD" 'org-roam-demote-entire-buffer
        "amr" 'org-roam-refile
        ) ;;org-ref-helm-insert-cite-link

      (spacemacs/declare-prefix-for-mode 'org-mode "mm" "org-roam")
      (spacemacs/declare-prefix-for-mode 'org-mode "mmo" "node properties")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "mb" 'org-roam-buffer
        "mt" 'org-roam-buffer-toggle
        "mf" 'org-roam-node-find ;; orb-find-non-ref-file
        "mF" 'org-roam-ref-find
        "mi" 'org-roam-node-insert   ;; orb-insert-non-ref
        "mI" 'org-id-get-create
        "ma" 'org-roam-tag-add
        "md" 'org-roam-tag-delete
        "mD" 'org-roam-demote-entire-buffer
        "mr" 'org-roam-refile
        "moa" 'org-roam-alias-add
        "moA" 'org-roam-alias-remove
        "mot" 'org-roam-tag-add
        "moT" 'org-roam-tag-remove
        "mor" 'org-roam-ref-add
        "moR" 'org-roam-ref-remove
        ))
    :config
    (org-roam-db-autosync-mode)
    (setq org-id-link-to-org-use-id t)
    (setq org-roam-mode-sections
          (list #'org-roam-backlinks-section
                #'org-roam-reflinks-section
                ;; #'org-roam-unlinked-references-section
                ))

    (setq org-roam-db-gc-threshold most-positive-fixnum)

    (add-to-list 'display-buffer-alist
                 '("\\*org-roam\\*"
                   (display-buffer-in-side-window)
                   (side . right)
                   (slot . 0)
                   (window-width . 0.33)
                   (window-parameters . ((no-other-window . t)
                                         (no-delete-other-windows . t)))))

    (cl-defmethod org-roam-node-directories ((node org-roam-node))
      (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
          (format "(%s)" (string-join (f-split dirs) "/"))
        ""))
    (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
      "Return the file TITLE for the node."
      (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

    (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
      "Return the hierarchy for the node."
      (let ((title (org-roam-node-title node))
            (olp (org-roam-node-olp node))
            (level (org-roam-node-level node))
            (filetitle (org-roam-node-filetitle node)))
        (concat
         (if (> level 0) (concat filetitle " > "))
         (if (> level 1) (concat (string-join olp " > ") " > "))
         title))
      )

    (setq org-roam-node-display-template "${directories} ${tags:20 } ${hierarchy:*}")

    (setq org-roam-capture-templates
          '(("d" "org-roam" plain "%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n")
             :unnarrowed t)))

    (add-hook 'org-mode-hook (lambda () (add-to-list 'company-backends #'company-capf)))
    ))

(defun zilongshanren-org/init-org-transclusion()
  (use-package org-transclusion
    :commands org-transclusion-mode
    :init
    (define-key global-map (kbd "<f3>") #'org-transclusion-mode)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "me" 'org-transclusion-open-edit-src-buffer-at-point)
    ))

(defun zilongshanren-org/init-org-roam-ui ()
  (use-package org-roam-ui
    :init
    (spacemacs/set-leader-keys
      "ams" 'org-roam-ui-open
      "amz" 'org-roam-ui-node-zoom
      "amL" 'org-roam-ui-node-local)

    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "mz" 'org-roam-ui-node-zoom
      "mL" 'org-roam-ui-node-local)

    :commands org-roam-ui-mode org-roam-ui-node-zoom org-roam-ui-node-local org-roam-ui-open
    :config
    (setq org-roam-ui-open-on-start nil)
    (setq org-roam-ui-browser-function 'eaf-open-browser)
    ))

(defun zilongshanren-org/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :init
    (spacemacs/set-leader-keys
      "ama" 'orb-note-actions
      "aml" 'orb-insert-link
      )
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "ma" 'orb-note-actions
      "ml" 'orb-insert-link)

    :config
    (setq bibtex-completion-pdf-open-function 'eaf-open)
    (setq orb-note-actions-frontend 'hydra)
    (setq orb-insert-interface 'generic)
    (setq orb-insert-generic-candidates-format 'entry)

    (add-to-list 'orb-note-actions-user (cons "Open PDF file(s) Externally" #'bibtex-completion-open-pdf-external))

    (setq orb-preformat-keywords
          '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

    (add-to-list 'org-roam-capture-templates
                 '("r" "ref" plain
                   ""
                   :if-new
                   (file+head "papers/${title}.org" "#+title: ${title}\n")
                   :unnarrowed t)
                 )

    (add-to-list 'org-roam-capture-templates
                 '("n" "ref+noter" plain
                   "- tags ::
- keywords :: %^{keywords}

* %^{title}
:PROPERTIES:
:Custom_ID: %^{citekey}
:URL: %^{url}
:AUTHOR: %^{author-or-editor}
:NOTER_DOCUMENT: %^{file}  ; <== special file keyword: if more than one filename
:NOTER_PAGE:               ;     is available, the user will be prompted to choose
:END:"
                   :if-new
                   (file+head "papers/${title}.org" "#+title: ${title}\n")))
    ))

(defun zilongshanren-org/post-init-org-pomodoro ()
  (progn
    (zilongshanren/pomodoro-notification)
    ;; auto restart after break
    (add-hook 'org-pomodoro-break-finished-hook
              (lambda ()
                (interactive)
                (point-to-register 1)
                (org-clock-goto)
                (org-pomodoro '(25))
                (register-to-point 1)
))))

(defun zilongshanren-org/post-init-org ()
  (with-eval-after-load 'org
    (progn
      ;; 将org-capture“创建的”时间戳添加到properties
      (defvar org-created-property-name "CREATED"
        "The name of the org-mode property that stores the creation date of the entry")

      (defun org-set-created-property (&optional active NAME)
        "Set a property on the entry giving the creation time.

       By default the property is called CREATED. If given the `NAME'
       argument will be used instead. If the property already exists, it
       will not be modified."
        (interactive)
        (let* ((created (or NAME org-created-property-name))
               (fmt (if active "<%s>" "[%s]"))
               (now  (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
          (unless (org-entry-get (point) created nil)
            (org-set-property created now))))
      (add-hook 'org-capture-before-finalize-hook #'org-set-created-property)

      ;; 让org-agenda从归档文件中抽取数据
      (setq org-agenda-file-regexp "\\`[^.].*\\.org\\(_archive\\)?\\'")

      (setq org-confirm-babel-evaluate nil)
      ;; config stuck project
      (setq org-stuck-projects
            '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

      (setq org-agenda-span 'day)
      (setq org-agenda-window-setup 'current-window)

      (setq org-todo-keywords
            '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Change task state to STARTED when clocking in
      (setq org-clock-in-switch-to-state "STARTED")
      ;; Save clock data and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      (setq org-log-done nil)

      (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                        ("\\section{%s}" . "\\section*{%s}")
                                        ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      ;; export org-mode in Chinese into PDF
      (setq org-latex-default-class "ctexart")
      (setq org-latex-pdf-process
            '(
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "rm -fr %b.out %b.log %b.tex auto"))

      (setq org-latex-listings t)

      (setq org-plantuml-jar-path
            (expand-file-name "~/.spacemacs.d/resources/plantuml.jar"))
      (setq org-ditaa-jar-path "~/.spacemacs.d/resources/ditaa.jar")

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((perl . t)
         (ruby . t)
         (shell . t)
         (dot . t)
         (js . t)
         (latex .t)
         (python . t)
         (emacs-lisp . t)
         (plantuml . t)
         (C . t)
         (ditaa . t)))

      ;; define the refile targets
      (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
      (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
      (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
      (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
      (setq org-agenda-files (list org-agenda-dir))

      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
        (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
          "." 'spacemacs/org-agenda-transient-state/body)
        )
      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates
            '((;; gtd
               "t" "study" entry (file+headline org-agenda-file-gtd "Study")
               "* TODO [#B] %?\n  %i\n"
               :empty-lines 1)
              ("w" "work" entry (file+headline org-agenda-file-gtd "Work")
               "* TODO [#B] %?\n  %i\n"
               :empty-lines 1)
              ;; note
              ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
               "* %?\n  %i\n"
               :empty-lines 1)
              ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
               "* TODO [#B] %?\n  %i\n"
               :empty-lines 1)
	          ("L" "Protocol Link" entry (file+headline org-agenda-file-note "Web")
               "* %? [[%:link][%:description]] \n")
              ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n  %i\n %a \n"
               :empty-lines 1)
              ("p" "Protocol" entry (file+headline org-agenda-file-note "Web")
               "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
              ;; code snippet
              ("s" "Code Snippet" entry
               (file org-agenda-file-code-snippet)
               "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
              ;; journal
              ("j" "Journal Entry"
               entry (file+datetree org-agenda-file-journal)
               "* %?"
               :empty-lines 1)))

      (with-eval-after-load 'org-capture
        (defun org-hugo-new-subtree-post-capture-template ()
          "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
          (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
                 (fname (org-hugo-slug title)))
            (mapconcat #'identity
                       `(
                         ,(concat "* TODO " title)
                         ":PROPERTIES:"
                         ,(concat ":EXPORT_FILE_NAME: " fname)
                         ":END:"
                         "\n\n")        ;Place the cursor here finally
                       "\n")))

        (add-to-list 'org-capture-templates
                     '("h"              ;`org-capture' binding + h
                       "Hugo post"
                       entry
                       ;; It is assumed that below file is present in `org-directory'
                       ;; and that it has a "Blog Ideas" heading. It can even be a
                       ;; symlink pointing to the actual location of all-posts.org!
                       (file+headline org-agenda-file-blogposts "Blog Ideas")
                       (function org-hugo-new-subtree-post-capture-template))))

      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急
      (setq org-agenda-custom-commands
            '(
              ("w" . "任务安排")
              ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
              ("wb" "重要且不紧急的任务" tags-todo "-weekly-monthly-daily+PRIORITY=\"B\"")
              ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
              ("b" "Blog" tags-todo "BLOG")
              ("p" . "项目安排")
              ("pw" tags-todo "project+work")
              ("pd" tags-todo "project+dream")
              ("r" . "回顾")
              ("rm" "月度回顾"
               ((agenda ""
                        ((org-agenda-overriding-header "每月记录")
                         (org-agenda-span 30)
                         (org-agenda-start-day "-30d")
                         (org-agenda-show-log 'clockcheck)
                         (org-agenda-start-with-log-mode nil)
                         (org-agenda-log-mode-items '(closed clock))
                         (org-agenda-clockreport-mode t)
                         ))))

              ("rw" "周度回顾"
               ((agenda "" ((org-agenda-overriding-header "每周记录")
                            (org-agenda-span 7)
                            (org-agenda-start-day "-7d")
                            (org-agenda-show-log 'clockcheck)
                            (org-agenda-start-with-log-mode nil)
                            (org-agenda-log-mode-items '(closed clock))
                            (org-agenda-clockreport-mode t)
                            ))))
              ("rd" "每日回顾"
               ((agenda "" ((org-agenda-overriding-header "今日记录")
                            (org-agenda-span 'day)
                            (org-agenda-show-log 'clockcheck)
                            (org-agenda-start-with-log-mode nil)
                            (org-agenda-log-mode-items '(closed clock))
                            (org-agenda-clockreport-mode t)
                            ))))
              ("W" "Weekly Review"
               ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                ))))

      (add-hook 'org-after-todo-statistics-hook 'zilong/org-summary-todo)
      (add-hook 'org-mode-hook #'zilongshanren/org-ispell)
      )))

;;; packages.el ends here
