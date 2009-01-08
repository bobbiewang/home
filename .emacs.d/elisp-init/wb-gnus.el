(setq gnus-startup-file "~/.emacs.d/gnus/newsrc")
(setq gnus-save-newsrc-file nil)
(setq gnus-read-newsrc-file nil)

(setq gnus-default-directory "~/.emacs.d/gnus/news/")
(setq gnus-directory "~/.emacs.d/gnus/news/")
(setq gnus-article-save-directory "~/.emacs.d/gnus/news/")
(setq gnus-cache-directory "~/.emacs.d/gnus/news/cache/")
(setq gnus-kill-files-directory "~/.emacs.d/gnus/news/")
(setq gnus-dribble-directory "~/.emacs.d/gnus/news")
(setq gnus-agent-directory "~/.emacs.d/gnus/news/agent/")

(setq message-auto-save-directory "~/.emacs.d/gnus/mails/drafts/")
(setq message-directory "~/.emacs.d/gnus/mails/")
;; (setq gnus-secondary-select-methods
;;       '( (nnml "" (nnml-directory "~/.emacs.d/gnus/mails/"))
;;          ...other.servers... ))

(require 'gnus)

(setq user-full-name "Bo Wang")
(setq user-mail-address "firbir@gmail-SPAM.no")

(setq gnus-novice-user nil)
(setq gnus-expert-user nil)
(setq gnus-interactive-catchup t)
(setq gnus-interactive-exit t)

(setq gnus-verbose 10)
(setq gnus-large-newsgroup 9999)

;; 设置 NNTP 服务
(setq gnus-select-method '(nnml ""))
;; (setq gnus-select-method '(nntp "news.cn99.com"))

;; 设置 IMAG 服务
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnimap-list-pattern ("*"))))

;; 设置 SMTP 服务
(setq sendmail-program "msmtp")
(setq mail-host-address "gmail.com")

;; 因为使用了 agent，增加 demon 更新的间隔，需要刷新的时候按 g
(gnus-demon-init)
(gnus-demon-add-handler 'gnus-demon-scan-news 360 t)
(gnus-demon-add-handler 'gnus-demon-scan-mail 360 t)

;; 设置 Group Buffer
(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%G%) (%F) %O\n")

;; 隐藏只有 ticked 文章的 group，可以用 L 显示所有 group
(setq gnus-list-groups-with-ticked-articles nil)

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")    ; 当天的新闻/邮件
        (604800 . "W%w %H:%M")              ; 七天之内的新闻/邮件
        ((gnus-seconds-month) . "%d %H:%M") ; 当月的新闻/邮件
        ((gnus-seconds-year) . "%m %d")     ; 今天的新闻/邮件
        (t . "%y-%m-%d")))

(setq gnus-face-10 'shadow)

(defun wb-gnus-summary-line-format-ascii nil
  (interactive)
  (setq gnus-summary-line-format
        (concat
         "%0{%U%R%z%}" "%10{|%}" "%1{%8&user-date;%}" "%10{|%}"
         "%(%-15,15f %)" "%10{|%}" "%4k" "%10{|%}"
         "%B" "%s\n"))
  (setq gnus-sum-thread-tree-single-indent   "o "
        gnus-sum-thread-tree-false-root      "x "
        gnus-sum-thread-tree-root            "* "
        gnus-sum-thread-tree-vertical        "| "
        gnus-sum-thread-tree-leaf-with-other "|-> "
        gnus-sum-thread-tree-single-leaf     "+-> "
        gnus-sum-thread-tree-indent          "  ")
  (gnus-message 5 "Using ascii tree layout."))

(wb-gnus-summary-line-format-ascii)

(setq gnus-parameters
      '(("^cn\\.bbs\\.comp\\."
         (visible . t)
         (posting-style
          (name "firebird")
          (address "firebird@newsmth.net-SPAM.no")
          (signature "凤翱翔于千仞兮，非梧不栖")
          (eval (setq mm-coding-system-priorities
                 '(iso-8859-1 gb2312 utf-8)))))
        ("^nnimap\\+gmail:cpug"
         (agent-predicate and (not read) (not old))
         (gnus-list-identifiers '("\\[python-.*+\\]" "\\[CPyUG.*?\\]")))
        ("^nnimap\\+gmail:toplanguage"
         (gnus-list-identifiers "\\[TopLanguage\\]"))
        ("^nnimap\\+gmail:rorpassion"
         (gnus-list-identifiers "\\[rails-development\\]")
         (posting-style
           (body "\n\n-- Best Regards,\n\nfirebird")))
        ("^nnimap\\+gmail:emacs-muse"
         (gnus-list-identifiers "\\[Muse-el-discuss\\]"))
        ("^nnimap\\+gmail:emacs-org"
         (gnus-list-identifiers "\\[Orgmode\\]"))
        ("^nnimap\\+gmail:railsonsh"
         (gnus-list-identifiers "\\[shanghaionrails\\]"))))

(require 'gnus-agent)

(setq gnus-agent-go-online 'ask)
(setq gnus-agent-synchronize-flags t)

(defun wb-gnus-agent-article-old-p ()
  "Say whether an article is old."
  (< (time-to-days (date-to-time (mail-header-date gnus-headers)))
     (- (time-to-days (current-time)) gnus-agent-expire-days)))

(setq gnus-category-predicate-alist
      (append gnus-category-predicate-alist
              '((old . wb-gnus-agent-article-old-p))))

(add-hook 'gnus-startup-hook
          '(lambda ()
             (setq gnus-visible-headers "^From:\\|^Subject:")
             (setq gnus-ignored-headers
                   (concat "^Newsgroup:\\|To:\\|^Organization:\\|"
                           "Mail-Followup-To:\\|" gnus-visible-headers))))

(add-hook 'gnus-article-prepare-hook
          (lambda ()
            (setq fill-column 80)
            (gnus-article-fill-long-lines)))

;; 隐藏 citation，需要时可以用 W W c 命令显示
;; (setq gnus-treat-hide-citation t)
 
;; 隐藏 signature，需要时可以用 W W s 命令显示
;; (setq gnus-treat-hide-signature t)

(defun wb-gnus-update-imap-info ()
  (interactive)
  (progn
    (nnimap-request-group "toplanguage" "gmail" t)
    (nnimap-request-group "nkeric" "gmail" t)
    (nnimap-request-group "gfans" "gmail" t)
    (nnimap-request-group "cpug" "gmail" t)))
