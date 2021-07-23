;; -*- coding: utf-8 -*-

(defconst zilongshanren-org-packages
  '(
    (org :location built-in)
    org-pomodoro
    (notdeft :location local)
    org-roam
    (org-transclusion :location (recipe :fetcher github :repo "nobiot/org-transclusion" :files ("*")))
    ;; org-roam-server
    org-roam-bibtex
    ivy-bibtex
    org-noter
    org-ref
    org-super-agenda
    (org-clock-watch :location (recipe :fetcher github :repo "wztdream/org-clock-watch" :files ("*")))
    (gkhabit :location (recipe :fetcher github :repo "Kinneyzhang/gkhabit"))
    org-latex-impatient
    easy-hugo
    ox-hugo
    iscroll
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
            (global-set-key [remap evil-next-line] #'iscroll-evil-next-line)
            (global-set-key [remap evil-previous-line] #'iscroll-evil-previous-line)
            (global-set-key [remap evil-next-visual-line] #'iscroll-evil-next-visual-line)
            (global-set-key [remap evil-previous-visual-line] #'iscroll-evil-previous-visual-line))
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

(defun zilongshanren-org/init-org-latex-impatient()
  (use-package org-latex-impatient
    :commands org-latex-impatient-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "To" 'org-latex-impatient-mode)
    (setq org-latex-impatient-tex2svg-bin
          "~/node_modules/mathjax-node-cli/bin/tex2svg")
    (setq org-latex-impatient-posframe-position-handler
          'posframe-poshandler-point-bottom-left-corner)
    ))

(defun zilongshanren-org/init-gkhabit ()
  (use-package gkhabit
    :commands gkh-init gkh-new gkh-record gkh-archive gkh-org-table-display gkh-delete gkh-report-current-week
    :config
    (setq gkh-file "~/org-notes/habit.org")
    ))

(defun zilongshanren-org/init-org-clock-watch ()
  (use-package org-clock-watch
    :after org
    :config
    (setq org-clock-watch-work-plan-file-path "~/org-notes/gtd.org")
    (org-clock-watch-toggle 'on)
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
    (add-hook 'org-mode-hook 'org-super-agenda-mode)
    ))

(defun zilongshanren-org/init-notdeft ()
  (use-package notdeft
    :load-path "~/bin/notdeft"
    :commands notdeft notdeft-mode-hydra/body notdeft-open-query notdeft-insert-org-link notdeft-org-link-new-file
    :config
    (setq notdeft-directories '("~/org-notes/notes"))
    (setq notdeft-secondary-extensions '("md" "txt"))
    (setq notdeft-xapian-max-results 0)
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
        "amt" 'org-roam-buffer
        "amf" 'org-roam-node-find ;; orb-find-non-ref-file
        "ami" 'org-roam-node-insert   ;; orb-insert-non-ref
        "ama" 'org-roam-tag-add
        "amd" 'org-roam-tag-remove
        ) ;;org-ref-helm-insert-cite-link
      ;; org-roam-ref-find
      ;; org-roam-alias-add
      ;; org-roam-alias-remove
      ;; org-roam-ref-add
      ;; org-roam-ref-remove

      (spacemacs/declare-prefix-for-mode 'org-mode "mm" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "mt" 'org-roam-buffer
        "mf" 'org-roam-node-find ;; orb-find-non-ref-file
        "mi" 'org-roam-node-insert   ;; orb-insert-non-ref
        "ma" 'org-roam-tag-add
        "md" 'org-roam-tag-delete
        ))
    :config
    (org-roam-setup)
    (setq org-id-link-to-org-use-id t)
    (setq org-roam-mode-sections
          (list #'org-roam-backlinks-section
                #'org-roam-reflinks-section
                ;; #'org-roam-unlinked-references-section
                ))

    (setq org-roam-db-gc-threshold most-positive-fixnum)

    (add-to-list 'display-buffer-alist
                 '(("\\*org-roam\\*"
                    (display-buffer-in-direction)
                    (direction . right)
                    (window-width . 0.33)
                    (window-height . fit-window-to-buffer))))

    (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)
    (setq org-roam-capture-templates
          '(("d" "org-roam" plain "%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n")
             :unnarrowed t)))

    (add-to-list 'org-roam-capture-ref-templates
                 '("a" "Annotation" plain (function org-roam-capture--get-point)
                   "%U ${body}\n"
                   :file-name "${slug}"
                   :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n"
                   :immediate-finish t
                   :unnarrowed t))

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

(defun zilongshanren-org/init-org-roam-server ()
  (use-package org-roam-server
    :init
    (spacemacs/set-leader-keys "ams" 'open-org-roam-server)
    :commands open-org-roam-server-other-window org-roam-server-mode
    :hook
    (kill-emacs . (lambda () (interactive) (org-roam-server-mode -1)))
    :config
    (setq org-roam-server-host "127.0.0.1"
          org-roam-server-port 8080
          org-roam-server-export-inline-images t
          org-roam-server-authenticate nil
          org-roam-server-network-poll t
          org-roam-server-network-arrows nil
          org-roam-server-network-label-truncate t
          org-roam-server-network-label-truncate-length 60
          org-roam-server-network-label-wrap-length 20)
    (org-roam-server-mode 1)
    ))

(defun zilongshanren-org/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :init
    (spacemacs/set-leader-keys
      "ama" 'orb-note-actions)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "ma" 'orb-note-actions)

    :config
    (setq bibtex-completion-pdf-open-function 'eaf-open)
    (setq orb-note-actions-frontend 'hydra)

    (add-to-list 'orb-note-actions-user (cons "Open PDF file(s) Externally" #'bibtex-completion-open-pdf-external))

    (setq orb-preformat-keywords
          '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

    (setq orb-templates
          '())
    (setq orb-templates
          '(("r" "ref" plain (function org-roam-capture--get-point) ""
             :file-name "papers/${title}"
             :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
             :unnarrowed t)
            ("n" "ref + noter" plain (function org-roam-capture--get-point)
             ""
             :file-name "papers/${title}"
             :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:NOTER_PAGE:
:END:")))
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
