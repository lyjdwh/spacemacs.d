;;; packages.el --- zilong-ui layer packages file for Spacemacs.
;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: liuyan <lyjdwh@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst zilongshanren-org-packages
  '(
    (org :location built-in)
    org-pomodoro
    (notdeft :location local)
    ob-typescript
    evil-org
    org-roam
    (org-transclusion :location (recipe :fetcher github :repo "nobiot/org-transclusion" :files ("*")))
    org-roam-server
    org-roam-bibtex
    ivy-bibtex
    org-noter
    org-ref
    org-super-agenda
    (org-clock-watch :location (recipe :fetcher github :repo "wztdream/org-clock-watch" :files ("*")))
    (gkhabit :location (recipe :fetcher github :repo "Kinneyzhang/gkhabit"))
    org-latex-impatient
    easy-hugo
    ))

(defun zilongshanren-org/init-easy-hugo()
  (use-package easy-hugo
    :commands easy-hugo
    :config
    (setq easy-hugo-basedir "~/org-notes/blog/")
    (setq easy-hugo-postdir "content/posts")
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
    (org-roam-buffer-position 'left)
    (org-roam-link-title-format "%s")
    (org-roam-completion-system 'ivy)
    (org-roam-buffer-width 0.2)
    (org-roam-tag-sources '(prop all-directories))
    (org-roam-file-extensions '("org" "md"))
    (org-roam-completion-everywhere t)
    :init
    (progn
      (spacemacs/declare-prefix "am" "org-roam")
      (spacemacs/set-leader-keys
        "amo" 'org-roam
        "amt" 'org-roam-today
        "amb" 'org-roam-switch-to-buffer
        "amf" 'orb-find-non-ref-file ;; org-roam-find-file
        "ami" 'orb-insert-non-ref    ;; org-roam-insert
        "amI" 'org-roam-insert-immediate
        "amc" 'org-roam-unlinked-references
        "amr" 'org-ref-helm-insert-cite-link
        "amg" 'org-roam-tag-add
        "amd" 'org-roam-tag-delete)

      (spacemacs/declare-prefix-for-mode 'org-mode "mm" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "mo" 'org-roam
        "mt" 'org-roam-today
        "mb" 'org-roam-switch-to-buffer
        "mf" 'orb-find-non-ref-file ;; org-roam-find-file
        "mi" 'orb-insert-non-ref    ;; org-roam-insert
        "mI" 'org-roam-insert-immediate
        "mc" 'org-roam-unlinked-references
        "mr" 'org-ref-helm-insert-cite-link
        "mg" 'org-roam-tag-add
        "md" 'org-roam-tag-delete
        ))
    :config
    (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)
    (setq org-roam-capture-templates
          '(("d" "org-roam" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}\" (current-time) t)"
             :head "#+title: ${title}\n"
             :unnarrowed t
             )))
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

(defun zilongshanren-org/post-init-evil-org ()
  (defun evil-org--populate-navigation-bindings ()
    "Configures gj/gk/gh/gl for navigation."
    (let-alist evil-org-movement-bindings
      (evil-define-key 'motion evil-org-mode-map
        (kbd (concat "g" .left)) 'org-previous-visible-heading
        (kbd (concat "g" .right)) 'org-next-visible-heading
        (kbd (concat "g" .up)) 'org-backward-element
        (kbd (concat "g" .down)) 'org-forward-element
        (kbd (concat "g" (capitalize .left))) 'evil-org-top))))

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

;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(defun zilongshanren-org/post-init-org ()
  (add-hook 'org-mode-hook (lambda () (spacemacs/toggle-line-numbers-off)) 'append)
  (add-hook 'org-mode-hook '(lambda ()
                              (progn
                                (if
                                    (member #'company-capf company-backends)
                                    (setq company-backends
                                          (delete #'company-capf company-backends)))
                                ;; (add-to-list 'company-backends '(company-tabnine :with company-capf))
                                )))
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
      ;; disable < auto pair for org mode
      ;; disable {} auto pairing in electric-pair-mode for web-mode
      (add-hook
       'org-mode-hook
       (lambda ()
         (setq-local electric-pair-inhibit-predicate
                     `(lambda (c)
                        (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

      (require 'org-tempo)
      ;; Allow multiple line Org emphasis markup.
      ;; http://emacs.stackexchange.com/a/13828/115
      (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
      ;; Below is needed to apply the modified `org-emphasis-regexp-components'
      ;; settings from above.
      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

      ;; (defun th/org-outline-context-p ()
      ;;   (re-search-backward org-outline-regexp))
      ;; ;; Some usages
      ;; (th/define-context-key org-mode
      ;;                        (kbd "RET")
      ;;                        (when (th/outline-context-p)
      ;;                          'org-insert-heading-respect-content))

      ;; Jump out of a TeX macro when pressing TAB twice.
      ;; (th/define-context-key TeX-mode-map (kbd "TAB")
      ;;                        (when (and (= 1 (length (this-command-keys-vector)))
	  ;;                                   (equal last-command-event (elt (this-command-keys-vector) 0))
	  ;;                                   (TeX-current-macro))
	  ;;                          #'th/TeX-goto-macro-end)))

      (spacemacs|disable-company org-mode)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "r" 'avy-org-refile-as-child)

      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "it" 'counsel-org-tag)

      (setq org-complete-tags-always-offer-all-agenda-tags t)

      (require 'org-compat)
      (require 'org)
      ;; (add-to-list 'org-modules "org-habit")
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)

      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets
            '((nil :maxlevel . 4)
              (org-agenda-files :maxlevel . 4)))
      ;; config stuck project
      (setq org-stuck-projects
            '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

      (setq org-agenda-inhibit-startup t) ;; ~50x speedup
      (setq org-agenda-span 'day)
      (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done nil)

      ;; 加密文章
      ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
      ;; org-mode 設定
      (require 'org-crypt)

      ;; 當被加密的部份要存入硬碟時，自動加密回去
      (org-crypt-use-before-save-magic)

      ;; 設定要加密的 tag 標籤為 secret
      (setq org-crypt-tag-matcher "secret")

      ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
      ;; (但是子項目還是會被加密喔)
      (setq org-tags-exclude-from-inheritance (quote ("secret")))

      ;; 用於加密的 GPG 金鑰
      ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
      (setq org-crypt-key nil)

      ;; (add-to-list 'auto-mode-alist '("\.org\\'" . org-mode))

      (setq org-todo-keywords
            '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Change task state to STARTED when clocking in
      (setq org-clock-in-switch-to-state "STARTED")
      ;; Save clock data and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

      (setq org-tags-match-list-sublevels nil)

      (add-hook 'org-mode-hook '(lambda ()
                                  ;; keybinding for editing source code blocks
                                  ;; keybinding for inserting code blocks
                                  (local-set-key (kbd "C-c i s")
                                                 'zilongshanren/org-insert-src-block)))
      (require 'ox-publish)
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

      ;; {{ export org-mode in Chinese into PDF
      ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
      ;; and you need install texlive-xetex on different platforms
      ;; To install texlive-xetex:
      ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
      ;; }}
      (setq org-latex-default-class "ctexart")
      (setq org-latex-pdf-process
            '(
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "rm -fr %b.out %b.log %b.tex auto"))

      (setq org-latex-listings t)

      (defun org-random-entry (&optional arg)
        "Select and goto a random todo item from the global agenda"
        (interactive "P")
        (if org-agenda-overriding-arguments
            (setq arg org-agenda-overriding-arguments))
        (if (and (stringp arg) (not (string-match "\\S-" arg))) (setq arg nil))
        (let* ((today (org-today))
               (date (calendar-gregorian-from-absolute today))
               (kwds org-todo-keywords-for-agenda)
               (lucky-entry nil)
               (completion-ignore-case t)
               (org-agenda-buffer (when (buffer-live-p org-agenda-buffer)
                                    org-agenda-buffer))
               (org-select-this-todo-keyword
                (if (stringp arg) arg
                  (and arg (integerp arg) (> arg 0)
                       (nth (1- arg) kwds))))
               rtn rtnall files file pos marker buffer)
          (when (equal arg '(4))
            (setq org-select-this-todo-keyword
                  (org-icompleting-read "Keyword (or KWD1|K2D2|...): "
                                        (mapcar 'list kwds) nil nil)))
          (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
          (catch 'exit
            (org-compile-prefix-format 'todo)
            (org-set-sorting-strategy 'todo)
            (setq files (org-agenda-files nil 'ifmode)
                  rtnall nil)
            (while (setq file (pop files))
              (catch 'nextfile
                (org-check-agenda-file file)
                (setq rtn (org-agenda-get-day-entries file date :todo))
                (setq rtnall (append rtnall rtn))))

            (when rtnall
              (setq lucky-entry
                    (nth (random
                          (safe-length
                           (setq entries rtnall)))
                         entries))

              (setq marker (or (get-text-property 0 'org-marker lucky-entry)
                               (org-agenda-error)))
              (setq buffer (marker-buffer marker))
              (setq pos (marker-position marker))
              (org-pop-to-buffer-same-window buffer)
              (widen)
              (goto-char pos)
              (when (derived-mode-p 'org-mode)
                (org-show-context 'agenda)
                (save-excursion
                  (and (outline-next-heading)
                       (org-flag-heading nil))) ; show the next heading
                (when (outline-invisible-p)
                  (show-entry))         ; display invisible text
                (run-hooks 'org-agenda-after-show-hook))))))

      ;;reset subtask
      (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

      ;; (add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)

      (setq org-plantuml-jar-path
            (expand-file-name "~/.spacemacs.d/resources/plantuml.jar"))
      (setq org-ditaa-jar-path "~/.spacemacs.d/resources/ditaa.jar")

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((perl . t)
         (ruby . t)
         (shell . t)
         (dot . t)
         (typescript . t)
         (js . t)
         (latex .t)
         (python . t)
         (emacs-lisp . t)
         (plantuml . t)
         (C . t)
         (ditaa . t)))


      (require 'ox-md nil t)

      ;; define the refile targets
      (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
      (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
      (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
      (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
      (setq org-agenda-files (list org-agenda-dir))

      ;; C-n for the next org agenda item
      (define-key org-agenda-mode-map (kbd "C-p") 'org-agenda-previous-item)

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

      (defvar zilongshanren-website-html-preamble
        "<div class='nav'>
<ul>
<li><a href='http://zilongshanren.com'>博客</a></li>
<li><a href='/index.html'>Wiki目录</a></li>
</ul>
</div>")
      (defvar zilongshanren-website-html-blog-head
        " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
      (setq org-publish-project-alist
            `(
              ("blog-notes"
               :base-directory "~/org-notes"
               :base-extension "org"
               :publishing-directory "~/org-notes/public_html/"

               :recursive t
               :html-head , zilongshanren-website-html-blog-head
               :publishing-function org-html-publish-to-html
               :headline-levels 4       ; Just the default for this project.
               :auto-preamble t
               :exclude "gtd.org"
               :exclude-tags ("ol" "noexport")
               :section-numbers nil
               :html-preamble ,zilongshanren-website-html-preamble
               :author "zilongshanren"
               :email "lyjdwh@gmail.com"
               :auto-sitemap t          ; Generate sitemap.org automagically...
               :sitemap-filename "index.org" ; ... call it sitemap.org (it's the default)...
               :sitemap-title "我的wiki"     ; ... with title 'Sitemap'.
               :sitemap-sort-files anti-chronologically
               :sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
               )
              ("blog-static"
               :base-directory "~/org-notes"
               :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
               :publishing-directory "~/org-notes/public_html/"
               :recursive t
               :publishing-function org-publish-attachment
               )
              ("blog" :components ("blog-notes" "blog-static"))))



      (add-hook 'org-after-todo-statistics-hook 'zilong/org-summary-todo)
      ;; used by zilong/org-clock-sum-today-by-tags

      (define-key org-mode-map (kbd "s-p") 'org-priority)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "tl" 'org-toggle-link-display)
      (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)

      ;; hack for org headline toc
      (defun org-html-headline (headline contents info)
        "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
        (unless (org-element-property :footnote-section-p headline)
          (let* ((numberedp (org-export-numbered-headline-p headline info))
                 (numbers (org-export-get-headline-number headline info))
                 (section-number (and numbers
                                      (mapconcat #'number-to-string numbers "-")))
                 (level (+ (org-export-get-relative-level headline info)
                           (1- (plist-get info :html-toplevel-hlevel))))
                 (todo (and (plist-get info :with-todo-keywords)
                            (let ((todo (org-element-property :todo-keyword headline)))
                              (and todo (org-export-data todo info)))))
                 (todo-type (and todo (org-element-property :todo-type headline)))
                 (priority (and (plist-get info :with-priority)
                                (org-element-property :priority headline)))
                 (text (org-export-data (org-element-property :title headline) info))
                 (tags (and (plist-get info :with-tags)
                            (org-export-get-tags headline info)))
                 (full-text (funcall (plist-get info :html-format-headline-function)
                                     todo todo-type priority text tags info))
                 (contents (or contents ""))
                 (ids (delq nil
                            (list (org-element-property :CUSTOM_ID headline)
                                  (org-export-get-reference headline info)
                                  (org-element-property :ID headline))))
                 (preferred-id (car ids))
                 (extra-ids
                  (mapconcat
                   (lambda (id)
                     (org-html--anchor
                      (if (org-uuidgen-p id) (concat "ID-" id) id)
                      nil nil info))
                   (cdr ids) "")))
            (if (org-export-low-level-p headline info)
                ;; This is a deep sub-tree: export it as a list item.
                (let* ((type (if numberedp 'ordered 'unordered))
                       (itemized-body
                        (org-html-format-list-item
                         contents type nil info nil
                         (concat (org-html--anchor preferred-id nil nil info)
                                 extra-ids
                                 full-text))))
                  (concat (and (org-export-first-sibling-p headline info)
                               (org-html-begin-plain-list type))
                          itemized-body
                          (and (org-export-last-sibling-p headline info)
                               (org-html-end-plain-list type))))
              (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
                    (first-content (car (org-element-contents headline))))
                ;; Standard headline.  Export it as a section.
                (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                        (org-html--container headline info)
                        (org-export-get-reference headline info)
                        (concat (format "outline-%d" level)
                                (and extra-class " ")
                                extra-class)
                        (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                                level
                                preferred-id
                                extra-ids
                                (concat
                                 (and numberedp
                                      (format
                                       "<span class=\"section-number-%d\">%s</span> "
                                       level
                                       (mapconcat #'number-to-string numbers ".")))
                                 full-text)
                                level)
                        ;; When there is no section, pretend there is an
                        ;; empty one to get the correct <div
                        ;; class="outline-...> which is needed by
                        ;; `org-info.js'.
                        (if (eq (org-element-type first-content) 'section) contents
                          (concat (org-html-section first-content "" info) contents))
                        (org-html--container headline info))))))))))

(defun zilongshanren-org/init-ob-typescript ()
  (use-package ob-typescript))

;;; packages.el ends here
