;; -*- coding: utf-8 -*-

(define-abbrev-table 'global-abbrev-table '(
                                            ;; path
                                            ("8df" "/home/liuyan/org-notes/notes/imgs/")

                                            ;; url
                                            ("8go" "https://www.google.com")

                                            ;; math/unicode symbols
                                            ("8in" "∈")
                                            ("8nin" "∉")
                                            ("8inf" "∞")
                                            ("8lv" "♥")
                                            ("8sm" "☺")

                                            ;; email
                                            ("8me" "lyjdwh@gmail.com")

                                            ;; emacs regex
                                            ("8d" "\\([0-9]+?\\)")
                                            ("8str" "\\([^\"]+?\\)\"")))

(setq-default abbrev-mode t)

(setq user-mail-address "lyjdwh@gmail.com")

(setq http-proxy "127.0.0.1:12333")
(setq socks-noproxy nil)
