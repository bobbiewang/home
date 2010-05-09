;;; 加载 .emacs 过程中的 log 函数和变量

(defvar *emacs-load-start-time* (current-time) ".emacs 加载时刻")
(defvar *emacs-load-prev-time* nil "上一个扩展加载时刻")
(defvar *emacs-load-ext-start-time* nil "扩展开始加载时刻")
(defvar *emacs-load-time-alist* nil "各扩展加载的时间")

(defun deh-log-info (msg)
  (save-excursion
    (set-buffer (get-buffer-create "*Messages*"))
    ;; 输出信息到 minibuffer，这个函数同时将信息输出到 *Messages*
    (message msg)
    ;; 删除 *Messages* 中的信息
    (previous-line)
    (kill-whole-line 1)
    ;; 在 *Messages* 中输出彩色信息
    (put-text-property 0 (length msg) 'face 'font-lock-builtin-face msg)
    (insert msg)
    (insert "\n")))

(defun deh-log-warn (msg)
  (save-excursion
    (set-buffer (get-buffer-create "*Messages*"))
    ;; 输出信息到 minibuffer，这个函数同时将信息输出到 *Messages*
    (message msg)
    ;; 删除 *Messages* 中的信息
    (previous-line)
    (kill-whole-line 1)
    ;; 在 *Messages* 中输出彩色信息
    (put-text-property 0 (length msg) 'face 'font-lock-warning-face msg)
    (insert msg)
    (insert "\n")))

(defun deh-load-time (name)
  "输出加载当前扩展的时间"
  (let ((time (float-time (time-since *emacs-load-ext-start-time*))))
	(add-to-list '*emacs-load-time-alist* (cons name time))
	(deh-log-info (format "[%.2f] Loaded %s." time name))))

(defun deh-initialization-time (msg)
  "输出 Emacs 启动时间"
  (deh-log-info (format "[%.2f] %s."
                        (float-time (time-since *emacs-load-start-time*))
                        msg)))

(defun deh-initialization-stat ()
  "输出 Emacs 启动过程的统计信息"
  (interactive)
  (let* ((load-time-alist				; 复制一份加载时间表，因为输出过程会被修改
		  (mapcar (lambda (item)
					(cons (car item)
						  (cdr item)))
				  *emacs-load-time-alist*))
		 (load-times					; 提取加载时间用于排序
		  (mapcar 'cdr load-time-alist))
		 (sorted-load-times
		  (sort load-times '>)))
	(deh-log-info "\nTop 10 Time-consuming Extensions")
	(mapc (lambda (time)
			(let ((item (rassoc time load-time-alist)))
				  (deh-log-info (format "  %.2f => %s" time (car item)))
				  ;; 修改 cons 中的时间，表示已经被输出
				  (setcdr item -1)))
		  (butlast
		   sorted-load-times
		   (- (length sorted-load-times) 10)))))

;;; 加载 library 的 macro

(defmacro robust-require (symbol &rest body)
  "强壮的加载 library 的 macro，即使 library 不存在也不会出错"
  `(condition-case nil
       (progn
         (setq *emacs-load-ext-start-time* (current-time))
         (require ',symbol)
         ,@body
         (deh-load-time (format "%s" ',symbol)))
         (error (deh-log-warn (format "[WARNING] Failed to require %s!" ',symbol))
                nil)))

(put 'robust-require 'lisp-indent-function 1)

(defmacro with-library (library &rest body)
  "存在 library 的情况下的方案"
  (declare (indent 1))
  `(if (locate-library ',library)
       (progn
         (setq *emacs-load-ext-start-time* (current-time))
         ,@body
         (deh-load-time (format "%s" ',library)))
     (deh-log-warn (format "[WARNING] No library %s. Skipped." ',library))))

(defmacro with-without-library (library with-body without-body)
  "存在或者不存在 library 情况下采用不同方案"
  (declare (indent 1))
  `(if (locate-library ',library)
       (progn
         (setq *emacs-load-ext-start-time* (current-time))
         ,@with-body
         (deh-load-time (format "%s" ',library)))
     (progn
       (setq *emacs-load-ext-start-time* (current-time))
       ,@without-body
       (deh-log-warn (format "[WARNING] No library %s. Skipped." ',library)))))

;;; Lazy load 功能，可以用于一些耗时的 library

(defvar deh-lazy-require-symbols nil
  "Symbols which need to be autoloaded when Emacs is idle.")

(defun lazy-require (symbol)
  "Add SYMBOL to `deh-lazy-require-symbols'."
  (push symbol deh-lazy-require-symbols))

(defun deh-lazy-require-load-next ()
  "Load symbols from `deh-lazy-require-symbols.' until input occurs."
  (let (symbol)
    (message "Beginning lazy-require")
    (while (and deh-lazy-require-symbols
                (not (input-pending-p)))
      (setq symbol (pop deh-lazy-require-symbols))
      (message "lazy-require %s..." symbol)
      (require symbol)
      (sit-for 1)))
  (when (null deh-lazy-require-symbols)
    (cancel-timer deh-lazy-require-timer)
    (setq deh-lazy-require-timer nil)
    (message "lazy-require finished")))

(setq deh-lazy-require-timer
      (run-with-idle-timer 45 t 'deh-lazy-require-load-next))

;; 捕获启动 Emacs 过程中的过错
;; 参考：http://ourcomments.org/Emacs/DL/elisp/dot-emacs/

(defun deh-emacs--debug-init() (interactive)
  (call-process (concat exec-directory "emacs") nil 0 nil "--debug-init")
  (message "Started 'emacs --debug-init' - it will be ready soon ..."))

(defun deh-display-dot-emacs-error(the-error)
  (let ((message-log-max nil))
    (save-excursion
      (set-buffer (get-buffer-create "*Messages*"))
      (let ((s
             (concat
              "\n\n"
              (format "An error has occurred while loading `%s':\n\n"
                      user-init-file)
              (format "%s%s%s"
                      (get (car the-error) 'error-message)
                      (if (cdr the-error) ": " "")
                      (mapconcat (lambda (s) (prin1-to-string s t)) (cdr the-error) ", "))
              "\n\n"
              "To ensure normal operation, you should investigate and remove the\n"
              "cause of the error in your initialization file.  Start Emacs with\n"
              "the `--debug-init' option to view a complete error backtrace.\n\n"
              "Click here to do that: ")))
        (put-text-property
         0 (length s)
         'face 'font-lock-warning-face
         s)
        (insert s)
        (insert-text-button "emacs --debug-init"
                            'action (lambda(btn) (deh-emacs--debug-init))
                            'follow-link 'mouse-face)
        (insert "\n") ;; Needed to get normal face again.
        (message "Error in init file: %s%s%s"
                 (get (car the-error) 'error-message)
                 (if (cdr the-error) ": " "")
                 (mapconcat 'prin1-to-string (cdr the-error) ", "))
        (display-buffer "*Messages*") ;; display-buffer does the job.
        (redisplay t)
        (setq init-file-had-error t)))))

;; Local Variables:
;; coding: utf-8-unix
;; mode: outline-minor
;; outline-regexp: ";;;\\(;* [^     \n]\\|###autoload\\)"
;; End:
