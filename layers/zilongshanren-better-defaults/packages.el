;; -*- coding: utf-8 -*-

(defconst zilongshanren-better-defaults-packages
  '(
    (eaf :location local)
    (snails :location local)
    (company-english-helper :location (recipe :fetcher github :repo "manateelazycat/company-english-helper"))
    (insert-translated-name :location (recipe :fetcher github :repo "manateelazycat/insert-translated-name"))
    super-save
    company-box
    (move-to-position-hint :location local)
    tree-sitter
    (tree-sitter-langs :location local)
    (grammatical-edit :location (recipe :fetcher github :repo "manateelazycat/grammatical-edit"))
    (find-orphan :location (recipe :fetcher github :repo "manateelazycat/find-orphan"))
    evil-textobj-tree-sitter
    undo-fu
    vundo
    dired
    diredfl
    vlf
    ))

(defun zilongshanren-better-defaults/init-vlf()
  (use-package vlf-setup
    :config
    (eval-after-load "vlf"
      '(define-key vlf-prefix-map "\C-cv" vlf-mode-map))
    ))

(defun zilongshanren-better-defaults/post-init-dired()
  (setq dired-recursive-copies  'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask)

  (define-key ranger-mode-map (kbd "ze") 'wdired-change-to-wdired-mode))

(defun zilongshanren-better-defaults/init-diredfl()
  (use-package diredfl
    :hook (dired-mode . diredfl-mode)))

(defun zilongshanren-better-defaults/init-undo-fu()
  (use-package undo-fu
    :config
    ;; Increase undo history limits to reduce likelihood of data loss
    (setq undo-limit 400000           ; 400kb (default is 160kb)
          undo-strong-limit 3000000   ; 3mb   (default is 240kb)
          undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

    (define-minor-mode undo-fu-mode
      "Enables `undo-fu' for the current session."
      :keymap (let ((map (make-sparse-keymap)))
                (define-key map [remap undo] #'undo-fu-only-undo)
                (define-key map [remap redo] #'undo-fu-only-redo)
                (define-key map (kbd "C-_")     #'undo-fu-only-undo)
                (define-key map (kbd "M-_")     #'undo-fu-only-redo)
                (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
                (define-key map (kbd "C-x r u") #'undo-fu-session-save)
                (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
                map)
      :init-value nil
      :global t)

    (undo-fu-mode)

    (with-eval-after-load 'evil
      (evil-set-undo-system 'undo-fu))
    ))

(defun zilongshanren-better-defaults/init-vundo()
  (use-package vundo
    :commands vundo
    :init
    (evil-define-key 'normal 'global-map (kbd "zu") 'vundo)
    ))

(defun zilongshanren-better-defaults/post-init-tree-sitter()
  (setq tree-sitter-debug-jump-buttons t)

  ;; show current class/function name in modeline
  (setq meain/tree-sitter-calss-like '((python-mode . (class_definition))))
  (setq meain/tree-sitter-function-like '((python-mode . (function_definition))))

  (defun meain/tree-sitter-thing-name (kind)
    "Get name of tree-sitter THING-KIND."
    (if tree-sitter-mode
        (let* ((node-types-list (pcase kind
                                  ('class-like meain/tree-sitter-calss-like)
                                  ('function-like meain/tree-sitter-function-like)))
               (node-types (alist-get major-mode node-types-list)))
          (if node-types
              (let ((node-at-point (car (remove-if (lambda (x) (eq nil x))
                                                   (seq-map (lambda (x) (tree-sitter-node-at-point x))
                                                            node-types)))))
                (if node-at-point
                    (let ((node-name-node-at-point (tsc-get-child-by-field node-at-point ':name)))
                      (if node-name-node-at-point
                          (tsc-node-text node-name-node-at-point)))))))))

  (add-hook 'python-mode-hook (lambda ()
      (setq-local header-line-format
          (list
           '(:eval (mode-line-idle 0.3
               '(:propertize
                 (:eval (let ((thing-name (meain/tree-sitter-thing-name 'class-like)))
                          (if thing-name
                              (format "%s" thing-name))))
                 face
                 font-lock-constant-face)
               ""))
           '(:eval (mode-line-idle 0.3
               '(:propertize
                 (:eval (let ((thing-name (meain/tree-sitter-thing-name 'function-like)))
                          (if thing-name
                              (format ":%s" thing-name))))
                 face
                 font-lock-constant-face)
               ""))
           ))))

  ;; (setq which-func-functions
  ;;       (list
  ;;        (lambda ()
  ;;          (concat (meain/tree-sitter-thing-name 'class-like)
  ;;                  ":"
  ;;                  (meain/tree-sitter-thing-name 'function-like)))
  ;;        ))
  )

(defun zilongshanren-better-defaults/init-tree-sitter-langs()
  (use-package tree-sitter-langs
    :load-path "~/bin/tree-sitter-langs"
    :after tree-sitter
    :config
    (tree-sitter-load 'elisp "elisp")
    (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
    (add-to-list 'tree-sitter-major-mode-language-alist '(inferior-emacs-lisp-mode . elisp))
    (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist '(emacs-lisp-mode . "elisp"))
    ))

(defun zilongshanren-better-defaults/init-grammatical-edit()
  (use-package grammatical-edit
    :config
    (dolist (hook (list
                   'c-mode-common-hook
                   'c-mode-hook
                   'c++-mode-hook
                   'java-mode-hook
                   'emacs-lisp-mode-hook
                   'lisp-interaction-mode-hook
                   'lisp-mode-hook
                   'sh-mode-hook
                   'python-mode-hook
                   'js-mode-hook
                   'go-mode-hook
                   'css-mode-hook
                   'html-mode-hook
                   'javascript-mode-hook
                   'typescript-mode-hook
                   'json-mode-hook
                   'rust-mode-hook
                   'minibuffer-inactive-mode-hook
                   ))
      (add-hook hook '(lambda () (grammatical-edit-mode 1))))

    (define-key grammatical-edit-mode-map (kbd "[") 'grammatical-edit-open-bracket)
    (define-key grammatical-edit-mode-map (kbd "{") 'grammatical-edit-open-curly)
    (define-key grammatical-edit-mode-map (kbd ")") 'grammatical-edit-close-round)
    (define-key grammatical-edit-mode-map (kbd "]") 'grammatical-edit-close-bracket)
    (define-key grammatical-edit-mode-map (kbd "}") 'grammatical-edit-close-curly)
    (define-key grammatical-edit-mode-map (kbd "=") 'grammatical-edit-equal)

    (define-key grammatical-edit-mode-map (kbd "%") 'grammatical-edit-match-paren)
    (define-key grammatical-edit-mode-map (kbd "\"") 'grammatical-edit-double-quote)
    (define-key grammatical-edit-mode-map (kbd "'") 'grammatical-edit-single-quote)

    (define-key grammatical-edit-mode-map (kbd "C-\"") 'grammatical-edit-wrap-double-quote)
    (define-key grammatical-edit-mode-map (kbd "C-'") 'grammatical-edit-wrap-single-quote)
    (define-key grammatical-edit-mode-map (kbd "C-)") 'grammatical-edit-wrap-round)
    (define-key grammatical-edit-mode-map (kbd "C-(") 'grammatical-edit-unwrap)
    ;; (define-key grammatical-edit-mode-map (kbd "M-[") 'grammatical-edit-wrap-bracket)
    ;; (define-key grammatical-edit-mode-map (kbd "M-{") 'grammatical-edit-wrap-curly)

    (define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)
    (define-key grammatical-edit-mode-map (kbd "C-d") 'grammatical-edit-backward-kill)

    ;; (define-key grammatical-edit-mode-map (kbd "M-p") 'grammatical-edit-jump-right)
    ;; (define-key grammatical-edit-mode-map (kbd "M-n") 'grammatical-edit-jump-left)
    (define-key grammatical-edit-mode-map (kbd "C-<return>") 'grammatical-edit-jump-out-pair-and-newline)
    ))

(defun zilongshanren-better-defaults/init-find-orphan()
  (use-package find-orphan
    :commands find-orphan-function-in-project find-orphan-function-in-buffer
    :init
    (spacemacs/set-leader-keys "sro" 'find-orphan-function-in-project)
    (spacemacs/set-leader-keys "srb" 'find-orphan-function-in-buffer)
    :config
    (defun find-orphan-function-in-project ()
      (interactive)
      (setq find-orphan-search-dir (or (projectile-project-root) default-directory))
      (find-orphan-function 'find-orphan-match-times-in-directory "directory"))
    ))

(defun zilongshanren-better-defaults/init-evil-textobj-tree-sitter()
  (use-package evil-textobj-tree-sitter
    :after tree-sitter
    :config
    ;; v/y + a/i + f/c/x
    (setq elisp-defun-query '((emacs-lisp-mode . [(function_definition) @defun])))
    (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "defun" elisp-defun-query))

    (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
    (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

    (define-key evil-outer-text-objects-map "F" (evil-textobj-tree-sitter-get-textobj "call.outer"))
    (define-key evil-inner-text-objects-map "F" (evil-textobj-tree-sitter-get-textobj "call.inner"))

    (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
    (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))

    (define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
    (define-key evil-inner-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.outer"))

    (define-key evil-outer-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
    (define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))

    (define-key evil-outer-text-objects-map "x" (evil-textobj-tree-sitter-get-textobj "statement.outer"))
    (define-key evil-inner-text-objects-map "x" (evil-textobj-tree-sitter-get-textobj "statement.outer"))

    (define-key evil-outer-text-objects-map "L" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
    (define-key evil-inner-text-objects-map "L" (evil-textobj-tree-sitter-get-textobj "loop.inner"))

    (defun meain/goto-and-recenter (group &optional previous end query)
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj group previous end query)
      (recenter 7))

    (defun tree-sitter-next-parameter () (interactive) (meain/goto-and-recenter "parameter.inner"))
    (defun tree-sitter-previous-parameter () (interactive) (meain/goto-and-recenter "parameter.inner" t))
    (defun tree-sitter-next-parameter-end () (interactive) (meain/goto-and-recenter "parameter.inner" nil t))
    (defun tree-sitter-previous-parameter () (interactive) (meain/goto-and-recenter "parameter.inner" t t))

    (defun tree-sitter-next-condition () (interactive) (meain/goto-and-recenter "conditional.outer"))
    (defun tree-sitter-previous-condition () (interactive) (meain/goto-and-recenter "conditional.outer" t))
    (defun tree-sitter-next-condition-end () (interactive) (meain/goto-and-recenter "conditional.outer" nil t))
    (defun tree-sitter-previous-condition-end () (interactive) (meain/goto-and-recenter "conditional.outer" t t))

    (defun tree-sitter-next-class () (interactive) (meain/goto-and-recenter "class.outer"))
    (defun tree-sitter-previous-class () (interactive) (meain/goto-and-recenter "class.outer" t))
    (defun tree-sitter-next-class-end () (interactive) (meain/goto-and-recenter "class.outer" nil t))
    (defun tree-sitter-previous-class-end () (interactive) (meain/goto-and-recenter "class.outer" t t))
    (defun tree-sitter-next-function-end () (interactive) (meain/goto-and-recenter "function.outer"nil t))
    (defun tree-sitter-previous-function-end () (interactive) (meain/goto-and-recenter "function.outer" t t))

    (defun tree-sitter-next-function () (interactive) (if (derived-mode-p 'emacs-lisp-mode)
                                                          (meain/goto-and-recenter "defun" nil nil elisp-defun-query)
                                                        (meain/goto-and-recenter "function.outer")))
    (defun tree-sitter-previous-function () (interactive) (if (derived-mode-p 'emacs-lisp-mode)
                                                              (meain/goto-and-recenter "defun" t nil elisp-defun-query)
                                                            (meain/goto-and-recenter "function.outer" t)))

    (define-key evil-normal-state-map (kbd "]f") 'tree-sitter-next-function)
    (define-key evil-normal-state-map (kbd "[f") 'tree-sitter-previous-function)

    (define-key evil-normal-state-map (kbd "]r") 'tree-sitter-next-parameter)
    (define-key evil-normal-state-map (kbd "[r") 'tree-sitter-previous-parameter)
    (define-key evil-normal-state-map (kbd "]R") 'tree-sitter-next-parameter-end)
    (define-key evil-normal-state-map (kbd "[R") 'tree-sitter-previous-parameter)
    (define-key evil-normal-state-map (kbd "]i") 'tree-sitter-next-condition)
    (define-key evil-normal-state-map (kbd "[i") 'tree-sitter-previous-condition)
    (define-key evil-normal-state-map (kbd "]I") 'tree-sitter-next-condition-end)
    (define-key evil-normal-state-map (kbd "[I") 'tree-sitter-previous-condition-end)
    (define-key evil-normal-state-map (kbd "]c") 'tree-sitter-next-class)
    (define-key evil-normal-state-map (kbd "[c") 'tree-sitter-previous-class)
    (define-key evil-normal-state-map (kbd "]C") 'tree-sitter-next-class-end)
    (define-key evil-normal-state-map (kbd "[C") 'tree-sitter-previous-class-end)
    (define-key evil-normal-state-map (kbd "]F") 'tree-sitter-next-function-end)
    (define-key evil-normal-state-map (kbd "[F") 'tree-sitter-previous-function-end)
    ))

(defun zilongshanren-better-defaults/init-move-to-position-hint ()
  (use-package move-to-position-hint
    :config
    (move-to-position-hint-mode)
    ))

(defun zilongshanren-better-defaults/post-init-company-box ()
  (use-package company-box
    :if (display-graphic-p)
    :defines company-box-icons-all-the-icons
    :custom
    (company-box-backends-colors nil)
    (company-box-doc-delay 0.1)
    (company-box-max-candidates 1000)
    (company-box-doc-frame-parameters '((internal-border-width . 1)
                                        (left-fringe . 3)
                                        (right-fringe . 3)))
    (company-box-scrollbar 'right)
    :config
    (delq 'company-preview-if-just-one-frontend company-frontends)

    (with-no-warnings
      ;; Prettify icons
      (defun my-company-box-icons--elisp (candidate)
        (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'lisp-mode))
          (let ((sym (intern candidate)))
            (cond ((fboundp sym) 'Function)
                  ((featurep sym) 'Module)
                  ((facep sym) 'Color)
                  ((boundp sym) 'Variable)
                  ((symbolp sym) 'Text)
                  (t . nil)))))
      (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

      ;; Display borders and optimize performance
      (defun my-company-box--display (string on-update)
        "Display the completions."
        (company-box--render-buffer string on-update)

        (let ((frame (company-box--get-frame))
              (border-color (face-foreground 'font-lock-comment-face nil t)))
          (unless frame
            (setq frame (company-box--make-frame))
            (company-box--set-frame frame))
          (company-box--compute-frame-position frame)
          (company-box--move-selection t)
          (company-box--update-frame-position frame)
          (unless (frame-visible-p frame)
            (make-frame-visible frame))
          (company-box--update-scrollbar frame t)
          (set-face-background 'internal-border border-color frame)
          (when (facep 'child-frame-border)
            (set-face-background 'child-frame-border border-color frame)))
        (with-current-buffer (company-box--get-buffer)
          (company-box--maybe-move-number (or company-box--last-start 1))))
      (advice-add #'company-box--display :override #'my-company-box--display)

      (setq company-box-doc-frame-parameters '((vertical-scroll-bars . nil)
                                               (horizontal-scroll-bars . nil)
                                               (internal-border-width . 1)
                                               (left-fringe . 8)
                                               (right-fringe . 8)))

      (defun my-company-box-doc--make-buffer (object)
        (let* ((buffer-list-update-hook nil)
               (inhibit-modification-hooks t)
               (string (cond ((stringp object) object)
                             ((bufferp object) (with-current-buffer object (buffer-string))))))
          (when (and string (> (length (string-trim string)) 0))
            (with-current-buffer (company-box--get-buffer "doc")
              (erase-buffer)
              (insert (propertize "\n" 'face '(:height 0.5)))
              (insert string)
              (insert (propertize "\n\n" 'face '(:height 0.5)))

              ;; Handle hr lines of markdown
              ;; @see `lsp-ui-doc--handle-hr-lines'
              (let (bolp next before after)
                (goto-char 1)
                (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
                  (when (get-text-property next 'markdown-hr)
                    (goto-char next)
                    (setq bolp (bolp)
                          before (char-before))
                    (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
                    (setq after (char-after (1+ (point))))
                    (insert
                     (concat
                      (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                      (propertize "\n" 'face '(:height 0.5))
                      (propertize " "
                                  'display '(space :height (1))
                                  'company-box-doc--replace-hr t
                                  'face `(:background ,(face-foreground 'font-lock-comment-face)))
                      (propertize " " 'display '(space :height (1)))
                      (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5))))))))

              (setq mode-line-format nil
                    display-line-numbers nil
                    header-line-format nil
                    show-trailing-whitespace nil
                    cursor-in-non-selected-windows nil)
              (current-buffer)))))
      (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

      ;; Display the border and fix the markdown header properties
      (defun my-company-box-doc--show (selection frame)
        (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
                  (window-configuration-change-hook nil)
                  (inhibit-redisplay t)
                  (display-buffer-alist nil)
                  (buffer-list-update-hook nil))
          (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                         company-box--bottom
                                         company-selection
                                         (company-box--get-frame)
                                         (frame-visible-p (company-box--get-frame))))
                       (candidate (nth selection company-candidates))
                       (doc (or (company-call-backend 'quickhelp-string candidate)
                                (company-box-doc--fetch-doc-buffer candidate)))
                       (doc (company-box-doc--make-buffer doc)))
            (let ((frame (frame-local-getq company-box-doc-frame))
                  (border-color (face-foreground 'font-lock-comment-face nil t)))
              (unless (frame-live-p frame)
                (setq frame (company-box-doc--make-frame doc))
                (frame-local-setq company-box-doc-frame frame))
              (set-face-background 'internal-border border-color frame)
              (when (facep 'child-frame-border)
                (set-face-background 'child-frame-border border-color frame))
              (company-box-doc--set-frame-position frame)

              ;; Fix hr props. @see `lsp-ui-doc--fix-hr-props'
              (with-current-buffer (company-box--get-buffer "doc")
                (let (next)
                  (while (setq next (next-single-property-change (or next 1) 'company-box-doc--replace-hr))
                    (when (get-text-property next 'company-box-doc--replace-hr)
                      (put-text-property next (1+ next) 'display
                                         '(space :align-to (- right-fringe 1) :height (1)))
                      (put-text-property (1+ next) (+ next 2) 'display
                                         '(space :align-to right-fringe :height (1)))))))

              (unless (frame-visible-p frame)
                (make-frame-visible frame))))))
      (advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

      (defun my-company-box-doc--set-frame-position (frame)
        (-let* ((frame-resize-pixelwise t)

                (box-frame (company-box--get-frame))
                (box-position (frame-position box-frame))
                (box-width (frame-pixel-width box-frame))
                (box-height (frame-pixel-height box-frame))
                (box-border-width (frame-border-width box-frame))

                (window (frame-root-window frame))
                ((text-width . text-height) (window-text-pixel-size window nil nil
                                                                    (/ (frame-pixel-width) 2)
                                                                    (/ (frame-pixel-height) 2)))
                (border-width (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0))

                (x (- (+ (car box-position) box-width) border-width))
                (space-right (- (frame-pixel-width) x))
                (space-left (car box-position))
                (fringe-left (or (alist-get 'left-fringe company-box-doc-frame-parameters) 0))
                (fringe-right (or (alist-get 'right-fringe company-box-doc-frame-parameters) 0))
                (width (+ text-width border-width fringe-left fringe-right))
                (x (if (> width space-right)
                       (if (> space-left width)
                           (- space-left width)
                         space-left)
                     x))
                (y (cdr box-position))
                (bottom (+ company-box--bottom (frame-border-width)))
                (height (+ text-height (* 2 border-width)))
                (y (cond ((= x space-left)
                          (if (> (+ y box-height height) bottom)
                              (+ (- y height) border-width)
                            (- (+ y box-height) border-width)))
                         ((> (+ y height) bottom)
                          (- (+ y box-height) height))
                         (t y))))
          (set-frame-position frame (max x 0) (max y 0))
          (set-frame-size frame text-width text-height t)))
      (advice-add #'company-box-doc--set-frame-position :override #'my-company-box-doc--set-frame-position)

      (when (require 'all-the-icons nil t)
        (setq company-box-icons-all-the-icons
              `((Unknown . ,(all-the-icons-material "format_align_center" :height 1.0 :v-adjust -0.2))
                (Text . ,(all-the-icons-faicon "text-width" :height 1.0 :v-adjust -0.02))
                (Method . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Function . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Constructor . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Field . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
                (Variable . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
                (Class . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Interface . ,(all-the-icons-material "share" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Module . ,(all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Property . ,(all-the-icons-faicon "wrench" :height 1.0 :v-adjust -0.02))
                (Unit . ,(all-the-icons-material "settings_system_daydream" :height 1.0 :v-adjust -0.2))
                (Value . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Enum . ,(all-the-icons-material "storage" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Keyword . ,(all-the-icons-material "filter_center_focus" :height 1.0 :v-adjust -0.2))
                (Snippet . ,(all-the-icons-material "format_align_center" :height 1.0 :v-adjust -0.2))
                (Color . ,(all-the-icons-material "palette" :height 1.0 :v-adjust -0.2))
                (File . ,(all-the-icons-faicon "file-o" :height 1.0 :v-adjust -0.02))
                (Reference . ,(all-the-icons-material "collections_bookmark" :height 1.0 :v-adjust -0.2))
                (Folder . ,(all-the-icons-faicon "folder-open" :height 1.0 :v-adjust -0.02))
                (EnumMember . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2))
                (Constant . ,(all-the-icons-faicon "square-o" :height 1.0 :v-adjust -0.1))
                (Struct . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Event . ,(all-the-icons-octicon "zap" :height 1.0 :v-adjust 0 :face 'all-the-icons-orange))
                (Operator . ,(all-the-icons-material "control_point" :height 1.0 :v-adjust -0.2))
                (TypeParameter . ,(all-the-icons-faicon "arrows" :height 1.0 :v-adjust -0.02))
                (Template . ,(all-the-icons-material "format_align_left" :height 1.0 :v-adjust -0.2)))
              company-box-icons-alist 'company-box-icons-all-the-icons)))
  ))

(defun zilongshanren-better-defaults/init-super-save ()
  (use-package super-save
    :custom
    (super-save-auto-save-when-idle t)
    (super-save-idle-duration 2)
    (auto-save-default nil)
    (make-backup-files nil)
    (save-silently t)
    :config
    (defcustom super-save-all-buffers t
      "Save all buffers when t, save only current buffer otherwise"
      :group 'super-save
      :type 'boolean)

    (defun super-save-command ()
      "Save the buffer if needed."
      (let ((buffer-to-save (if super-save-all-buffers
                                (buffer-list)
                              (list (current-buffer)))))
        (save-excursion
          (dolist (buf buffer-to-save)
            (set-buffer buf)
            (when (and buffer-file-name
                       (buffer-modified-p (current-buffer))
                       (file-writable-p buffer-file-name)
                       (if (file-remote-p buffer-file-name) super-save-remote-files t))
              (save-buffer))))))

    (super-save-mode 1)
    ))

(defun zilongshanren-better-defaults/init-company-english-helper ()
  (use-package company-english-helper
    :commands (toggle-company-english-helper)))

(defun zilongshanren-better-defaults/init-insert-translated-name ()
  (use-package insert-translated-name
    :commands insert-translated-name-insert insert-translated-region-replace insert-translated-name-insert-original-translation
    :config
    (defun insert-translated-region-replace ()
      "translate Chinese to English and replace it "
      (interactive)
      (insert-translated-name-replace-symbol "comment"))
    ))

(defun zilongshanren-better-defaults/init-snails ()
  (use-package snails
    :load-path "/home/liuyan/bin/snails"
    :commands (snails snails-search-point)
    :hook (snails-mode . evil-insert-state)
    :if (display-graphic-p)
    :custom-face
    (snails-content-buffer-face ((t (:background "#111" :height 130))))
    (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 130))))
    (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.2))))
    :config
    (add-to-list 'load-path "/home/liuyan/bin/fuz.el/")
    (setq snails-default-backends '(snails-backend-eaf-browser-search snails-backend-eaf-github-search snails-backend-google-suggestion))
    (setq snails-prefix-backends
          '((":" '(snails-backend-search-pdf))
            (";" '(snails-backend-eaf-pdf-table))
            ))
    (setq snails-show-with-frame nil)
    (define-key snails-mode-map (kbd "C-j") 'snails-select-next-item)
    (define-key snails-mode-map (kbd "C-k") 'snails-select-prev-item)
    (define-key snails-mode-map (kbd "C-h") 'snails-select-prev-backend)
    (define-key snails-mode-map (kbd "C-l") 'snails-select-next-backend)
    (define-key snails-mode-map (kbd "C-o") 'snails-candiate-alternate-do)
    ))

(defun zilongshanren-better-defaults/init-eaf ()
  (use-package eaf
    :load-path "/home/liuyan/bin/eaf" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
    :commands (open-file-with-eaf eaf-open-mail-as-html eaf-open-camera
               eaf-open-browser eaf-open-browser-other-window eaf-open-external eaf-toggle-fullscreen
               eaf-open eaf-open-url eaf-open-office eaf-open-mindmap eaf-open-airshare
               eaf-open-bookmark eaf-kill-process eaf-search-it eaf-file-browser-qrcode
               eaf-install-dependencies eaf-install-and-update eaf-open-file-manager eaf-open-in-file-manager
               eaf-pdf-synctex-forward-view eaf-open-rss-reader eaf-open-git)
    :diminish eaf-mode
    :init
    (use-package epc :defer t :ensure t)
    (use-package ctable :defer t :ensure t)
    (use-package deferred :defer t :ensure t)
    (use-package s :defer t :ensure t)

    (spacemacs/declare-prefix "ae" "eaf")
    ;; set eaf as default browse
    (setq browse-url-browser-function 'eaf-open-browser-other-window)
    (defalias 'browse-web #'eaf-open-browser-other-window)

    ;; use `emacs-application-framework' to open PDF file: link
    (defun eaf-org-open-file (file &optional link)
      "An wrapper function on `eaf-open'."
      (eaf-open file))

    (defun eaf-org-open-office-file (file &optional link)
      "An wrapper function on `eaf-open-office'."
      (eaf-open-office file))

    (with-eval-after-load 'org
      (add-to-list 'org-file-apps '("\\.pdf\\'" . eaf-org-open-file))
      ;; (add-to-list 'org-file-apps '("\\.xlsx\\'" . eaf-org-open-office-file))
      ;; (add-to-list 'org-file-apps '("\\.docx\\'" . eaf-org-open-office-file))
      (add-to-list 'org-file-apps '("\\.docx\\'" . "wps %s"))
      (add-to-list 'org-file-apps '("\\.xlsx\\'" . "et %s"))
      (add-to-list 'org-file-apps '("\\.pptx\\'" . "wpp %s"))
      )

    :custom
    (eaf-evil-leader-keymap  spacemacs-cmds)
    ;; (eaf-evil-leader-key "SPC")
    :config
    (require 'eaf-org-previewer)
    (require 'eaf-image-viewer)
    (require 'eaf-file-sender)
    (require 'eaf-file-browser)
    (require 'eaf-airshare)
    (require 'eaf-browser)
    (require 'eaf-pdf-viewer)
    (require 'eaf-video-player)
    (require 'eaf-mindmap)
    (require 'eaf-markdown-previewer)
    (require 'eaf-file-manager)
    (require 'eaf-camera)
    (require 'eaf-rss-reader)
    (require 'eaf-git)

    (require 'eaf-evil)
    (require 'eaf-all-the-icons)
    (require 'eaf-mail)

    (setq eaf-buffer-title-format "EAF/%s")
    (setq eaf-python-command "/usr/bin/python")

    ;; git
    (setq eaf-git-layout "V")

    ;; browser
    ;; (setq eaf-proxy-type "socks5")
    ;; (setq eaf-proxy-host "127.0.0.1")
    ;; (setq eaf-proxy-port "1080")
    ;; (setq eaf-browser-aria2-proxy-host "127.0.0.1")
    ;; (setq eaf-browser-aria2-proxy-port "12333")
    (setq eaf-browser-translate-language "zh-CN")
    (setq eaf-chrome-bookmark-file "~/.config/chromium/Default/Bookmarks")
    (setq eaf-browser-chrome-history-file "~/.config/chromium/Default/History")
    (setq eaf-browser-enable-autofill t)
    ;; (setq eaf-browser-default-zoom 1.25)
    (setq eaf-browser-enable-adblocker t)

    ;; set dark mode
    (setq eaf-pdf-dark-mode nil)
    (setq eaf-mindmap-dark-mode nil)

    ;; camera
    (setq eaf-camera-save-path "~/Pictures")

    ;; org
    (require 'eaf-org)
    (setq eaf-org-override-pdf-links-open t)
    (setq eaf-org-override-pdf-links-store t)

    ;; key customize
    (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_highlight "ah" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_underline "au" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_squiggly "as" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_inline_text "ai" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_popup_text "ap" eaf-pdf-viewer-keybinding)
    (eaf-bind-key add_annot_strikeout_or_delete_annot "ad" eaf-pdf-viewer-keybinding)
    (eaf-bind-key edit_annot_text "ae" eaf-pdf-viewer-keybinding)
    (eaf-bind-key move_annot_text "am" eaf-pdf-viewer-keybinding)
    (eaf-bind-key undo_annot_action "aU" eaf-pdf-viewer-keybinding)
    (eaf-bind-key redo_annot_action "aR" eaf-pdf-viewer-keybinding)

    (eaf-bind-key copy_select "C-c" eaf-pdf-viewer-keybinding)
    (eaf-bind-key kill_text "C-c" eaf-browser-keybinding)

    ;; The following lines create the major-mode leader key emulation map
    ;; in a similar way as how it was done in the evil-integration example
    (setq eaf-evil-leader-for-major-keymap (make-sparse-keymap))
    (define-key eaf-evil-leader-for-major-keymap (kbd "h") 'eaf-open-browser-with-history)
    (define-key eaf-evil-leader-for-major-keymap (kbd "b") 'eaf-open-bookmark)
    (define-key eaf-evil-leader-for-major-keymap (kbd "d") 'eaf-py-proxy-toggle_dark_mode)
    (define-key eaf-evil-leader-for-major-keymap (kbd "s") 'eaf-search-it)
    (define-key eaf-evil-leader-for-major-keymap (kbd "c") 'eaf-py-proxy-copy_code)
    (define-key eaf-evil-leader-for-major-keymap (kbd "u") 'eaf-py-proxy-copy_link)
    (define-key eaf-evil-leader-for-major-keymap (kbd "r") 'eaf-py-proxy-insert_or_switch_to_reader_mode)
    (define-key eaf-evil-leader-for-major-keymap (kbd "v") 'eaf-py-proxy-select_text)
    (define-key eaf-evil-leader-for-major-keymap (kbd "e") 'eaf-atomic-edit)
    (define-key eaf-evil-leader-for-major-keymap (kbd "E") 'eaf-open-external)

    (add-hook 'evil-normal-state-entry-hook
              (lambda ()
                (when (and (derived-mode-p 'eaf-mode))
                  (define-key eaf-mode-map (kbd "C-,") eaf-evil-leader-for-major-keymap))))

    (define-key key-translation-map (kbd ",")
                (lambda (prompt)
                  (if (and (derived-mode-p 'eaf-mode)
                           (string= eaf--buffer-app-name "browser"))
                      (if (eaf-call-sync "execute_function" eaf--buffer-id "is_focus")
                          (kbd ",")
                        (kbd "C-,")
                        ))))

    (define-key key-translation-map (kbd "SPC")
                (lambda (prompt)
                  (if (derived-mode-p 'eaf-mode)
                      (pcase eaf--buffer-app-name
                        ((or
                          (and "browser"
                               (guard (not (eaf-call-sync "execute_function" eaf--buffer-id "is_focus"))))
                          "image-viewer"
                          "pdf-viewer")
                         (kbd eaf-evil-leader-key))
                        (_  (kbd "SPC")))
                    (kbd "SPC"))))

    ;; (add-to-list 'eaf-preview-display-function-alist '("browser" . eaf--browser-display))

    (defun eaf-atomic-edit ()
      (interactive)
      (call-interactively 'eaf-py-proxy-insert_or_focus_input)
      (sleep-for 0.1)
      (call-interactively 'eaf-py-proxy-atomic_edit))

    (defun eaf-open-this (file)
      "Open html/pdf/image/video files whenever possible with EAF.
    Other files will open normally with `dired-find-file' or `dired-find-alternate-file'"
      (cond
       ((member (eaf-get-file-name-extension file) eaf-office-extension-list)
        (eaf-open-office file))
       ((eaf--get-app-for-extension
         (eaf-get-file-name-extension file))
        (eaf-open file))
       (t (dired-find-file))))

    (defun open-file-with-eaf ()
      "Open current file in eaf."
      (interactive)
      (let ((file-path (if (derived-mode-p 'dired-mode)
                           (dired-get-file-for-visit)
                         buffer-file-name)))
        (if file-path
            (eaf-open-this file-path)
          (message "No file associated to this buffer."))))

    (defun eaf-goto-left-tab ()
      "Go to left tab when awesome-tab exists."
      (interactive)
      (awesome-tab-backward-tab))

    (defun eaf-goto-right-tab ()
      "Go to right tab when awesome-tab exists."
      (interactive)
      (awesome-tab-forward-tab))

    (defun eaf-pdf-synctex-forward-view ()
      "View the PDF file of Tex synchronously."
      (interactive)
      (let* ((pdf-url (expand-file-name (TeX-active-master (TeX-output-extension))))
             (tex-buffer (window-buffer (minibuffer-selected-window)))
             (tex-file (buffer-file-name tex-buffer))
             (line-num (progn (set-buffer tex-buffer) (line-number-at-pos)))
             (opened-buffer (eaf-pdf--find-buffer pdf-url))
             (synctex-info (eaf-pdf--get-synctex-info tex-file line-num pdf-url)))
        (if (not opened-buffer)
            (progn
              (when (< (length (window-list)) 2)
                (split-window-right))
              (evil-window-top-left)
              (eaf-open pdf-url "pdf-viewer" (format "synctex_info=%s" synctex-info)))
          (pop-to-buffer opened-buffer)
          (eaf-call-sync "call_function_with_args" eaf--buffer-id
                         "jump_to_page_synctex" (format "%s" synctex-info)))))
    ))

;;; packages.el ends here
