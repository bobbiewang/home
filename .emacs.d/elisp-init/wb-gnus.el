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

(setq gnus-select-method '(nntp "news.cn99.com"))

(setq gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M")    ;; 当天的新闻/邮件
                                    (604800 . "W%w %H:%M")              ;; 七天之内的新闻/邮件
                                    ((gnus-seconds-month) . "%d %H:%M") ;; 当月的新闻/邮件
                                    ((gnus-seconds-year) . "%m %d")     ;; 今天的新闻/邮件
                                    (t . "%y-%m-%d")))

(setq gnus-summary-line-format "%U%R %8&user-date; (%4k) %B%(%*%S%) <<< %-8,8n\n"
      gnus-sum-thread-tree-vertical "│"
      gnus-sum-thread-tree-root "┬ "
      gnus-sum-thread-tree-false-root " ┌ "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-single-indent "┬ "
      gnus-sum-thread-tree-leaf-with-other "├→ "
      gnus-sum-thread-tree-single-leaf "└→ ")
