;;;; wb-custom.el
(setq custom-file "~/.emacs.d/elisp-init/wb-custom.el")
(load custom-file 'noerror)

;;;; wb-platforms.el

;; Environment predication constants

(defconst *win32p*
    (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *cygwinp*
    (eq system-type 'cygwin)
  "Are we running on a WinTel cygwin system?")

(defconst *linuxp*
    (or (eq system-type 'gnu/linux)
        (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")

(defconst *unixp*
  (or (eq system-type 'usg-unix-v)
      (eq system-type 'berkeley-unix))
  "Are we running unix")

(defconst *linux-x-p*
    (and window-system *linuxp*)
  "Are we running under X on a GNU/Linux system?")

(defconst *officep*
  (or (string-match system-name "phoenix")
      (string-match system-name "APOLLO")
      (string-match system-name "linux6")
      (string-match system-name "linux11"))
  "Are we running in office?")

(defconst *homep*
  (or (string-match system-name "andlinux")
      (string-match system-name "SUNLAND"))
  "Are we running at home?")

(defconst *office-win32-p*
  (and *officep* *win32p*)
  "Are we running in office on a WinTel system?")

(defconst *office-linux-p*
  (and *officep* *linuxp*)
  "Are we running in office on a GNU/Linux system?")

(defconst *home-win32-p*
  (and *homep* *win32p*)
  "Are we running at home on a WinTel system?")

(defconst *home-linux-p*
  (and *homep* *linuxp*)
  "Are we running at home on a GNU/Linux system?")

(defconst *xemacsp* (featurep 'xemacs)
  "Are we running XEmacs?")

(defconst *emacs>=21p*
  (and (not *xemacsp*)
       (or (= emacs-major-version 21)
           (= emacs-major-version 22)
           (= emacs-major-version 23)))
  "Are we running GNU Emacs 21 or above?")

(defconst *emacs<=22p*
  (and (not *xemacsp*)
       (or (= emacs-major-version 21)
           (= emacs-major-version 22)))
  "Are we running GNU Emacs 22 or older?")

(defconst *emacs>=23p*
  (and (not *xemacsp*)
       (= emacs-major-version 23))
  "Are we running GNU Emacs 23 or above?")

;; (require 'cygwin-mount)
;; (cygwin-mount-activate)

;; NT-emacs assumes a Windows command shell, which you change
;; here.
;; (defun cygwin-shell()
;;   (interactive)
;;   (setq old-process-coding-system-alist process-coding-system-alist)
;;   (setq old-shell-file-name shell-file-name)
;;   (setq process-coding-system-alist '(("bash" . undecided-unix)))
;;   (setq shell-file-name "bash")
;;   (setenv "SHELL" shell-file-name)
;;   (setq explicit-shell-file-name shell-file-name)
;;   (switch-to-buffer (shell "*cygwin-shell*"))
;;   (delete-other-windows)
;;   (setq process-coding-system-alist old-process-coding-system-alist)
;;   (setq shell-file-name old-shell-file-name)
;;   (setenv "SHELL" shell-file-name)
;;   (setq explicit-shell-file-name shell-file-name))

;; (if (eq window-system 'w32)
;;     (defun insert-x-style-font()
;;       "Insert a string in the X format which describes a font the
;; user can select from the Windows font selector."
;;       (interactive)
;;       (insert (prin1-to-string (w32-select-font)))))

;;;; wb-functions.el

;;; Infrastructure

(defun wb-get-symbol-at-point (&optional msg-prompt prompt-always no-regexp-quote)
  (interactive)
  (let* ((region-string (if mark-active
                            (buffer-substring-no-properties
                             (region-beginning) (region-end))
                          nil))
         (symbol (cond
                  (mark-active
                   (progn
                     (setq region-string
                           (if no-regexp-quote
                               region-string
                             (regexp-quote region-string)))))
                  (t (thing-at-point 'symbol)))))
    (when (or prompt-always
              (not symbol))
      (when msg-prompt
        (setq symbol (read-string msg-prompt symbol))))
    (when symbol (substring-no-properties symbol))))

(defun touch-file (filename)
  "Touch FILENAME, that is set its modification time (modtime) to
current time."
  (interactive "fFile to touch: ")
  (set-file-times filename (current-time)))

;;; Move, Edit, View

(defvar wb-elisp-defun-re
  (regexp-opt '("defun" "defsubst" "defmacro" "defadvice") 'paren)
  "Regular expression used to identify a defun.")

(defun wb-jump-to-elisp-defun (func)
  "Jump to the definition of function FUNC in the current buffer, if found.
Return the position of the defun, or nil if not found."
  (interactive
   (let ((fn (function-called-at-point)))
     (list (completing-read (if fn
                                (format "Find defun for (default %s): " fn)
                              "Find defun for: ")
                            obarray 'fboundp t nil nil (symbol-name fn)))))
  (let (place)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (concat "^[ \t]*(" wb-elisp-defun-re "[ \t]+"
                   (regexp-quote func) "[ \t]+") (point-max) t)
          (setq place (point))))
    (if (not place)
        (if (interactive-p) (message "No defun found for `%s'" func))
      (when (interactive-p)
        (push-mark)
        (goto-char place)
        (message "Found defun for `%s'" func))
      place)))

;; 按百分率跳转到某一行
(defun wb-goto-line (percent)
  (interactive (list (or current-prefix-arg
                         (string-to-number
                          (read-from-minibuffer "Goto percent: ")))))
  (let* ((total (count-lines (point-min) (point-max)))
         (num (round (* (/ total 100.0) percent))))
    (goto-line num)))

;; 将当前行移动到本页第一行
(defun wb-line-to-top-of-window ()
  "Move the line point is on to top of window."
  (interactive)
  (recenter 0))

;; 找到当前 buffer 里最长的一行，并且跳转到那里
(defun wb-goto-longest-line (&optional goto)
  "Find visual length (ie in columns) of longest line in buffer.
If optional argument GOTO is non-nil, go to that line."
  (interactive "p")
  (let ((maxlen 0)
        (line 1)
        len maxline)
    (save-excursion
      (goto-char (point-min))
      (goto-char (line-end-position))
      ;; Not necessarily same as line-end - line-beginning (eg tabs)
      ;; and this function is for visual purposes.
      (setq len (current-column))
      (if (eobp)                        ; 1 line in buffer
          (setq maxlen len
                maxline line)
        (while (zerop (forward-line))
          (goto-char (line-end-position))
          (setq line (1+ line)
                len (current-column))
          (if (> len maxlen)
              (setq maxlen len
                    maxline line)))))
    (if (not (interactive-p))
        maxlen
      (message "Longest line is line %s (%s)" maxline maxlen)
      (if goto (goto-line maxline)))))

;; 自定义自动补齐命令，如果在单词中间就补齐，否则就是输入 tab
;; 可以绑定到 TAB 键
(defun wb-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command)))

;; 删除行尾的空白，只作用于某些指定的 major mode，比较安全
;; 可以设置为在写文件的时候自动运行
;; (add-hook (if (boundp 'write-file-functions) 'write-file-functions
;;             'write-file-hooks) 'my-delete-trailing-whitespace)
(defun my-delete-trailing-whitespace ()
  "Delete all trailing whitespace in buffer.
Return values are suitable for use with `write-file-functions'."
  (condition-case nil
      (progn
        ;; Don't want to do this to mail messages, etc.
        ;; Would an exclude list be better?
        ;; Error was occurring in VM-mode for some reason.
        (when (memq major-mode '(text-mode sh-mode emacs-lisp-mode
                                           f90-mode awk-mode c-mode))
          (message "Cleaning up whitespace...")
          (delete-trailing-whitespace)
          (message "Cleaning up whitespace...done")
          nil))
    (error (message "Cleaning up whitespace...ERROR")
           t)))

(defun wb-untabify-buffer (prefix)
  "Untabify the whole buffer. Calls untabify for the whole
buffer. If called with prefix argument: use prefix argument as
tabwidth"
  (interactive "p")
  (let ((tab-width (or current-prefix-arg tab-width)))
    (untabify (point-min) (point-max)))
  (message "Untabified buffer."))

(defun wb-delete-control-M ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "$" (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

(defun wb-exchange-slash-and-backslash ()
  "Exchanges / with \ and in the current line or in the region
when a region-mark is active."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((replace-count 0)
            (eol-pos (if mark-active
                         (region-end)
                       (progn (end-of-line) (point))))
            (bol-pos (if mark-active
                         (region-beginning)
                       (progn (beginning-of-line) (point)))))
        (goto-char bol-pos)
        (while (re-search-forward "/\\|\\\\" eol-pos t)
          (setq replace-count (+ replace-count 1))
          (cond ((string-equal (match-string 0) "/") (replace-match "\\\\" nil nil))
                ((string-equal (match-string 0) "\\") (replace-match "/" nil nil)))
          (message (format "%d changes made." replace-count)))))))

(defun wb-another-line ()
  "Copy line, preserving cursor column, and increment any numbers found."
  (interactive)
  (let* ((col (current-column))
	 (bol (progn (beginning-of-line) (point)))
	 (eol (progn (end-of-line) (point)))
	 (line (buffer-substring bol eol)))
    (beginning-of-line)
    (while (re-search-forward "[0-9]+" eol 1)
      (let ((num (string-to-int (buffer-substring
				  (match-beginning 0) (match-end 0)))))
	(replace-match (int-to-string (1+ num)))))
    (beginning-of-line)
    (insert line "\n")
    (move-to-column col)))

(defun wb-replace-identifier ()
  "Replace thing at point with another string."
  (interactive)
  (let* ((old-string (wb-get-symbol-at-point "Replace: " current-prefix-arg))
         (new-string (read-string
                      (concat "Replace `" old-string "' with: ") "" nil old-string)))
    (save-excursion
      (deactivate-mark)
      (goto-char (point-min))
      (query-replace-regexp old-string new-string))))

(defun wb-occur-identifier ()
  "Open occur buffer with identifier at point."
  (interactive)
  (occur (wb-get-symbol-at-point "Find occurances in buffer (regex): ")
         current-prefix-arg)
  (wb-resize-other-window))

(defun wb-copy-symbol-at-point ()
  "Copies the actual symbol to the kill ring."
  (interactive)
  (let ((string (wb-get-symbol-at-point)))
    ;(kill-new string)
    (if (eq last-command 'kill-region)
        (progn
          (kill-append (concat " " string) nil)
          (message "%s appended" string))
      (kill-new string)
      (message "%s copied" string))))

(defun wb-embrace-selection (&optional front-arg rear-arg)
  (interactive)
  (let* ((front (or front-arg (read-string "Front brace: ")))
         (rear (or rear-arg (read-string "Rear brace: "))))
    (if mark-active
        (progn
          (save-excursion
            (goto-char (region-beginning))
            (insert front))
          (save-excursion
            (goto-char (region-end))
            (insert rear)))
      (insert front)
      (save-excursion
        (insert rear)))))

(defun wb-copy-buffer-file-name-as-kill(choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy BufferName (f)ull, (d)irectory, (n)ame, (w)ikiname or (q)uit?")
  ;(message "your choice %c" choice)
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          ((eq choice ?w)
           (setq new-kill-string (run-hook-with-args-until-success
                                  'planner-annotation-functions))))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(defun wb-toggle-narrow()
  "Narrow to region, if region is marked, otherwise widen"
  (interactive)
  (if mark-active
      (narrow-to-region (region-beginning) (region-end))
    (widen)))

(defun wb-insert-date ()
  "Insert current date"
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

(defun wb-insert-time ()
  "Insert current time"
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun wb-count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

;; 自动换行显示
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;;; Buffer

(defun wb-quit-buffer ()
  "Delete the current buffer and the corresponding window also"
  (interactive)
  (kill-buffer (current-buffer))
  (when (> (count-windows) 1)
    (delete-window)))

(defun wb-unbury-buffer ()
  "Reverse bury-buffer."
  (interactive)
  (switch-to-buffer (nth (- (length (buffer-list)) 1) (buffer-list))))

(defun wb-show-message-buffer (arg)
  "Show the *message* buffer.
When called with a prefix argument, show the *trace-output* buffer."
  (interactive "P")
  (let ((buffer (current-buffer)))
    (pop-to-buffer (if arg "*trace-output*" "*Messages*"))
    (goto-char (point-max))
    (recenter -12)
    (pop-to-buffer buffer)))

;;; Window

;; 方便在 Windows 之间移动，但缺省的 Shift 加方向键似乎只能在 GUI 下用
(robust-require windmove
  (windmove-default-keybindings))

(defun wb-resize-other-window ()
  (interactive)
  (save-excursion
    (other-window 1)
    (resize-temp-buffer-window)
    (other-window -1)))

(defun wb-split-window ()
  "Split the current window and show in the window below the next
buffer in the buffer list.  When called twice restore the window
configuration before the split."
  (interactive)
  (if (eq last-command 'wb-split-window)
      (progn
        (set-window-configuration wb-split-window-configuration)
        (setq this-command 'wb-unsplit-window))
    (let ((buf-list)
          (cur-buf (current-buffer)))
      (setq wb-split-window-configuration (current-window-configuration))
      (delete-other-windows)
      (split-window-vertically)
      (setq buf-list (buffer-list))
      (delq (get-buffer " *Minibuf-0*") buf-list)
      (delq (get-buffer " *Minibuf-1*") buf-list)
      (pop-to-buffer (cadr buf-list))
      (pop-to-buffer cur-buf))))

(defun wb-flip-windows ()
  (interactive)
  (let ((cur-buffer (current-buffer))
        (top-buffer)
        (bottom-buffer))
    (pop-to-buffer (window-buffer (frame-first-window)))
    (setq top-buffer (current-buffer))
    (other-window 1)
    (setq bottom-buffer (current-buffer))
    (switch-to-buffer top-buffer)
    (other-window -1)
    (switch-to-buffer bottom-buffer)
    (pop-to-buffer cur-buffer)))

;; 如果当前 frame 只有两个 windows，旋转 windows
(defun wb-rotate-windows ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((wl    (window-list))
             (w1    (frame-first-window))
             (w2    (if (equal w1 (car wl)) (cadr wl) (car wl)))
             (b1    (window-buffer w1))
             (b2    (window-buffer w2))
             (first (if (equal (current-buffer) b1) t nil)))
        (if (= (window-width) (frame-width))
            (split-window-horizontally)
          (split-window-vertically))
        (other-window 2)
        (delete-window)
        (switch-to-buffer b1)
        (other-window 1)
        (switch-to-buffer b2)
        (when first (other-window 1)))
    (message "There are not exactly 2 windows.")))

;;; Frame

;; MS Windows 平台 frame 控制
(when *win32p*
  (defun wb-restore-frame (&optional arg)
    "Restore a minimized frame"
    (interactive)
    (w32-send-sys-command 61728 arg))
  (defun wb-maximize-frame (&optional arg)
    "Maximize the current frame"
    (interactive)
    (w32-send-sys-command 61488 arg))
  (add-hook 'after-make-frame-functions 'wb-maximize-frame))

;;; Utilities

(defun wb-max-line-length ()
  "Return the max line length in the current buffer"
  (let ((max-len 0))
    (save-excursion
      (goto-char (point-min))
      (while (eq (forward-line) 0)
        (end-of-line)
        (when (> (current-column) max-len)
          (setq max-len (current-column))))
      max-len)))

(defun wb-calculator-sum-column (start end)
  "Adds all integer, decimal, and floating-point numbers found in the
selected rectangle."
  (interactive "r")
  (save-excursion
    (kill-rectangle start end)
    (exchange-point-and-mark)
    (yank-rectangle)
    (set-buffer (get-buffer-create "*calc-sum*"))
    (erase-buffer)
    (yank-rectangle)
    (exchange-point-and-mark)
    (let ((sum 0))
      (while (re-search-forward
              "[-+]?\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][-+]?[0-9]+\\)?"
              nil t)
        ;; Examples of numbers it reads (nonexhaustive):  2 +2 -2
        ;; 2. +2. -2. 2.0 +2.0 -2.0 2e0 +2e0 -2e0 2E0 2e+0 2e-0,
        ;; 2.e0, 2.0e0, etc.
        (setq sum (+ sum (string-to-number (match-string 0)))))
      (message "Sum: %f" sum))))

(defun insert-gpl-license ()
  (interactive)
  (insert
"/*
 * Program Name
 * Copyright (C) 2008 Sunland
 * @author Bo Wang <Bo.Wang@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
"))

;; 打印出键盘图，显示全部热键
(defun wb-key-table (arg)
  "Print the key bindings in a tabular form.
Argument ARG Key."
  (interactive "sEnter a modifier string:")
  (with-output-to-temp-buffer "*Key table*"
    (let* ((i 0)
           (keys (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n"
                       "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                       "<return>" "<down>" "<up>" "<right>" "<left>"
                       "<home>" "<end>" "<f1>" "<f2>" "<f3>" "<f4>" "<f5>"
                       "<f6>" "<f7>" "<f8>" "<f9>" "<f10>" "<f11>" "<f12>"
                       "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
                       "`" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-" "_"
                       "=" "+" "\\" "|" "{" "[" "]" "}" ";" "'" ":" "\""
                       "<" ">" "," "." "/" "?"))
           (n (length keys))
           (modifiers (list "" "C-" "M-" "S-" "M-C-" "S-C-")))
      (or (string= arg "") (setq modifiers (list arg)))
      (setq k (length modifiers))
      (princ (format " %-10.10s |" "Key"))
      (let ((j 0))
        (while (< j k)
          (princ (format " %-50.50s |" (nth j modifiers)))
          (setq j (1+ j))))
      (princ "\n")
      (princ (format "_%-10.10s_|" "__________"))
      (let ((j 0))
        (while (< j k)
          (princ (format "_%-50.50s_|"
                         "__________________________________________________"))
          (setq j (1+ j))))
      (princ "\n")
      (while (< i n)
        (princ (format " %-10.10s |" (nth i keys)))
        (let ((j 0))
          (while (< j k)
            (let* ((binding
                    (key-binding (read-kbd-macro (concat (nth j modifiers)
                                                         (nth i keys)))))
                   (binding-string "_"))
              (when binding
                (if (eq binding 'self-insert-command)
                    (setq binding-string (concat "'" (nth i keys) "'"))
                  (setq binding-string (format "%s" binding))))
              (setq binding-string
                    (substring binding-string 0 (min (length
                                                      binding-string) 48)))
              (princ (format " %-50.50s |" binding-string))
              (setq j (1+ j)))))
        (princ "\n")
        (setq i (1+ i)))
      (princ (format "_%-10.10s_|" "__________"))
      (let ((j 0))
        (while (< j k)
          (princ (format "_%-50.50s_|"
                         "__________________________________________________"))
          (setq j (1+ j))))))
  (delete-window)
  (toggle-truncate-lines nil))

(defun wb-ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (save-excursion (let ((i -1))
    (insert "ASCII characters 0 thru 127.\n\n")
    (insert " Oct  Hex  Dec  Char |  Oct  Hex  Dec  Char |  Oct  Hex  Dec  Char |  Oct  Hex  Dec  Char\n")
    (while (< i 31)
      (insert (format "%4o %4x %4d %5s | %4o %4x %4d %5s | %4o %4x %4d %5s | %4o %4x %4d %5s\n"
                      (setq i (+ 1  i)) i i (single-key-description i)
                      (setq i (+ 32 i)) i i (single-key-description i)
                      (setq i (+ 32 i)) i i (single-key-description i)
                      (setq i (+ 32 i)) i i (single-key-description i)))
      (setq i (- i 96))))))

(defun wb-ascii-table-2 ()
  "Show a table of ASCII characters by decimal, hex, and octal value.
   Similar with M-x man RET ascii RET."
  (interactive)
  (switch-to-buffer "*ASCII2*")
  (erase-buffer)
  (let ((min 1) (max 255)
        (special-chars '(
                         (1 . "%c  SOH (start of heading)")
                         (2 . "%c  STX (start of text)")
                         (3 . "%c  ETX (end of text)")
                         (4 . "%c  EOT (end of transmission)")
                         (5 . "%c  ENQ (enquiry)")
                         (6 . "%c  ACK (acknowledge)")
                         (7 . "%c  BEL (bell)")
                         (8 . "%c  BS  (backspace)")
                         (9 . "    TAB (horizontal tab)")
                         (10 . "    LF  (NL line feed, new line)")
                         (11 . "%c  VT  (vertical tab)")
                         (12 . "    FF  (NP form feed, new page)")
                         (13 . "%c  CR  (carriage return)")
                         (14 . "%c  SO  (shift out)")
                         (15 . "%c  SI  (shift in)")
                         (16 . "%c  DLE (data link escape)")
                         (17 . "%c  DC1 (device control 1)")
                         (18 . "%c  DC2 (device control 2)")
                         (19 . "%c  DC3 (device control 3)")
                         (20 . "%c  DC4 (device control 4)")
                         (21 . "%c  NAK (negative acknowledge)")
                         (22 . "%c  SYN (synchronous idle)")
                         (23 . "%c  ETB (end of trans. block)")
                         (24 . "%c  CAN (cancel)")
                         (25 . "%c  EM  (end of medium)")
                         (26 . "%c  SUB (substitute)")
                         (27 . "%c  ESC (escape)")
                         (28 . "%c  FS  (file separator)")
                         (29 . "%c  GS  (group separator)")
                         (30 . "%c  RS  (record separator)")
                         (31 . "%c  US  (unit separator)")
                         (32 . "%c       (space)")
                         (9999)
                         )))
    (insert (format "ASCII characters %d-%d\n\n" min max))
    (insert " Dec   Hex   Oct    Character\n")
    (let ((i 0))
      (while (< i 60)
        (insert "=")
        (setq i (+ i 1))))
    (insert "\n")
    (let ((i min))
      (while (<= i max)
        (let ((line "%4d  0x%02X  %04o    ") (char "%c"))
          (while (> i (car (car special-chars)))
            (setq special-chars (cdr special-chars)))
          (if (= (car (car special-chars)) i)
              (setq char (cdr (car special-chars))))
          (insert (format (concat line char "\n") i i i i))
          (setq i (+ i 1))))))
  (beginning-of-buffer))

;; command to start automatic documentation generation
(defvar generate-doc-command "doc.bat"
  "Shell command to start the automatic documentation generation.")
(make-variable-buffer-local 'generate-doc-command)
(defun wb-generate-documentation ()
  "Start the automatic documentation generation"
  (interactive)
  (message "Running %s ..." generate-doc-command)
  (shell-command-to-string generate-doc-command)
  (message "Running %s ... finished." generate-doc-command))

;; command to start automatic TAGS generation
(defvar generate-tags-command "create_tags.bat"
  "Shell command to regenerate the TAGS file.")
(make-variable-buffer-local 'generate-tags-command)
(defun wb-recreate-tags ()
  "Recreate the TAGS file"
  (interactive)
  (message "%s => %s" generate-tags-command
           (shell-command-to-string generate-tags-command)))

;;; Emacs Lisp

(defun wb-balance-defuns (buffname)
  "Check that every defun in BUFF is balanced (current-buffer if interactive)."
  (interactive "bBuffer to balance: ")
  (let ((buff (get-buffer buffname)))
    (set-buffer buff)
    (let ((next-end (point-min)))
      (condition-case ddd
          (progn
            (while (setq next-end (scan-lists next-end 1 0)))
            (if (interactive-p)
                (message "All defuns balanced.")
              t))
        (error
         (push-mark nil t)
         (goto-char next-end)
         (re-search-forward "\\s(\\|\\s)")
         (backward-char 1)
         (cond ((interactive-p)
                (ding)
                (message "Unbalanced defun."))
               (t nil)))))))

(defun wb-trace-function (arg)
  (interactive "p")
   (let* ((untracing (< arg 0))
          (function (intern (completing-read
                             (if untracing "Untrace function: " "Trace function: ")
                             obarray 'fboundp t (symbol-name (function-at-point))))))
     (cond ((eq current-prefix-arg nil)
            (message "tracing %S in background" function)
            (trace-function-background function))
           ((> arg 0)
            (message "tracing %S" function)
            (trace-function function))
           (untracing
            (message "untracing %S" function)
            (untrace-function function)))))

;;;; wb-std.el

;;; General

;; 设置 Emacs 启动后的缺省路径
(setq default-directory "~/")

;; 在 *Message* buffer 里保留 256 条消息，缺省只保留 100 条
(setq message-log-max 256)

;; t：遇到错误的时候自动进入 Debugger
(setq debug-on-error nil)

;; 禁止启动 Emacs/Gnus 后显示的欢迎屏幕
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

;; 在 *scratch* buffer 中不显示初始信息
(setq initial-scratch-message nil)

;; 启动 Emacs Server
(server-start)

;;; I18N

(when *emacs<=22p*
  (robust-require mule-gbk))

(if (and *win32p* (or *emacs>=23p* (and *emacs<=22p* (fboundp 'mule-gbk-selection-setup))))
    ;; Windows 环境使用 chinese-gbk 编码
    (progn
      ;; Setup GBK environment
      (set-terminal-coding-system 'chinese-gbk)
      (set-keyboard-coding-system 'chinese-gbk)
      (set-language-environment 'chinese-gbk)
      (setq locale-coding-system 'chinese-gbk)
      ;; Setup X Selection for mule-gbk
      (if *emacs<=22p* (mule-gbk-selection-setup))
      ;; Unicode support, for Emacs CVS (21.3.50) only
      (when (fboundp 'utf-translate-cjk-mode)
        ;; Load modified utf-translate-cjk-mode
        (require 'gbk-utf-mode)
        (utf-translate-cjk-load-tables)
        ;; Turn on utf-translate-cjk-mode
        (utf-translate-cjk-mode 1)
        ;; Setup X selection for unicode encoding
        (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

      (define-coding-system-alias 'chinese-iso-8bit 'chinese-gbk)
      (define-coding-system-alias 'cn-gb-2312 'chinese-gbk)
      (define-coding-system-alias 'euc-china 'chinese-gbk)
      (define-coding-system-alias 'euc-cn 'chinese-gbk)
      (define-coding-system-alias 'cn-gb 'chinese-gbk)
      (define-coding-system-alias 'gb2312 'chinese-gbk)
      (define-coding-system-alias 'cp936 'chinese-gbk)
      (define-coding-system-alias 'gb18030 'chinese-gbk)
      (define-coding-system-alias 'GB18030 'chinese-gbk)
      (define-coding-system-alias 'chinese-gb18030 'chinese-gbk)
      (define-coding-system-alias 'cn-gb18030 'chinese-gbk)

      (setq w32-charset-info-alist
            (cons '("gbk" w32-charset-gb2312 . 936) w32-charset-info-alist))

      ;; Windows 环境要用 GB2312 作为 Selection Coding System
      (set-selection-coding-system 'gb2312))
  ;; *nix 环境使用 UTF-8 编码
  (setq locale-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8-unix)
  ;; (set-terminal-coding-system 'utf-8-unix)
  ;; (set-keyboard-coding-system 'utf-8-unix)
  ;; (set-buffer-file-coding-system 'utf-8)
  ;; (set-file-name-coding-system 'utf-8)
  ;; (set-selection-coding-system 'utf-8)
  ;; (set-clipboard-coding-system 'utf-8)
  ;; (setq-default pathname-coding-system 'utf-8)
  ;; (setq default-buffer-file-coding-system 'utf-8-unix)
  ;; (setq-default buffer-file-coding-system 'utf-8-unix)
  ;; (setq save-buffer-coding-system 'utf-8-unix)
  )

;; Consolas 和雅黑的 2:1 组合
;; 18/20 20/22 22/24

(when *win32p*
  ;; 定义字体
  (if *emacs<=22p*
      ;; Emacs 22 定义字体的方法
      (create-fontset-from-fontset-spec
       (concat
        "-outline-Consolas-normal-r-normal-normal-18-97-96-96-c-*-fontset-gbk,"
        "chinese-gb2312:-outline-微软雅黑-normal-r-normal-*-20-*-96-96-c-*-iso10646-1,"
        "mule-unicode-0100-24ff:-*-微软雅黑-normal-r-*-*-20-*-96-96-c-*-iso10646-1,"
        "chinese-cns11643-5:-*-微软雅黑-normal-r-normal-*-20-*-96-96-c-*-iso10646-1,"
        "chinese-cns11643-6:-*-微软雅黑-normal-r-normal-*-20-*-96-96-c-*-iso10646-1,"
        "chinese-cns11643-7:-*-微软雅黑-normal-r-normal-*-20-*-96-96-c-*-iso10646-1"))
    ;; Emacs 23 定义字体的方法
    (create-fontset-from-fontset-spec
     "-*-Consolas-normal-r-*-*-18-*-*-*-c-*-fontset-gbk")
    (set-fontset-font
     "fontset-default" nil
     "-*-微软雅黑-normal-r-*-*-20-*-*-*-*-*-iso10646-1" nil 'prepend)
    (set-fontset-font
     "fontset-gbk" 'chinese-big5-1
     "-*-微软雅黑-normal-r-*-*-20-*-*-*-*-*-iso10646-1" nil 'prepend)
    (set-fontset-font
     "fontset-gbk" 'chinese-big5-2
     "-*-微软雅黑-normal-r-*-*-20-*-*-*-*-*-iso10646-1" nil 'prepend)
    (set-fontset-font
     "fontset-gbk" 'kana
     "-*-微软雅黑-normal-r-*-*-20-*-*-*-*-*-iso10646-1" nil 'prepend)
    (set-fontset-font
     "fontset-gbk" 'han
     "-*-微软雅黑-normal-r-*-*-20-*-*-*-*-*-iso10646-1" nil 'prepend)
    (set-fontset-font
     "fontset-gbk" 'gb18030
     "-*-微软雅黑-normal-r-*-*-20-*-*-*-*-*-iso10646-1" nil 'prepend)
    (set-fontset-font
     "fontset-gbk" 'cjk-misc
     "-*-微软雅黑-normal-r-*-*-20-*-*-*-*-*-iso10646-1" nil 'prepend)
    (set-fontset-font
     "fontset-gbk" 'symbol
     "-*-微软雅黑-normal-r-*-*-20-*-*-*-*-*-iso10646-1" nil 'prepend))

  ;; 使用字体
  (set-default-font "fontset-gbk")

  ;; 在 C-x 5 2 打开的 Frame 中也正常显示字体
  (setq default-frame-alist
        (append
         '((font . "fontset-gbk")) default-frame-alist)))

(when *win32p*
  ;; Windows 环境下使用 unical 识别编码
  (robust-require unicad)
  )

;; 支持中文句尾标点，支持 M-a M-e 等命令
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; eim 中文输入法

(with-library "eim"
  (autoload 'eim-use-package "eim" "Another emacs input method")
  ;; Tooltip 暂时还不好用
  (setq eim-use-tooltip nil)

  (add-hook 'eim-py-load-hook
            (lambda ()
              (let ((map (eim-mode-map)))
                (define-key map "," 'eim-previous-page)
                (define-key map "." 'eim-next-page))))

  (register-input-method
   "eim-py" "euc-cn" 'eim-use-package
   "拼音" "汉字拼音输入法" "py.txt")

  ;; 用 ; 暂时输入英文
  (require 'eim-extra)
  (global-set-key ";" 'eim-insert-ascii)

  (setq default-input-method "eim-py"))

;;; Interface

(when window-system
  (tool-bar-mode -1)
  (set-scroll-bar-mode 'left))
(menu-bar-mode -1)

;; 如果 buffer 有对应的 file，在 title 显示 buffer 的全路径
;; 否则，在 title 显示 buffer 的名字
(setq frame-title-format '(buffer-file-name "%n %F %f" ("%n %F %b")))

;; 以闪烁整个 frame 的形式代替警铃，可以通过 ring-bell-function
;; 自定义警铃的方式。比如希望没有任何提示，可以
;; (setq ring-bell-function (lambda ()))
(setq visible-bell t)

;; 支持滚轮鼠标
(mouse-wheel-mode t)

;; 当鼠标移动的时候自动转换 frame，window 或者 minibuffer
(setq mouse-autoselect-window t)

;; 滚动页面的方式
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

;;; Display

(global-font-lock-mode t)

;; 显示括号匹配
(show-paren-mode t)
;; 光标靠近鼠标时鼠标跳开
(mouse-avoidance-mode 'animate)
(blink-cursor-mode -1) ; 光标不要闪烁
;; 可以显示图片
(auto-image-file-mode t)
;; 高亮显示选中区域
(transient-mark-mode t)
;; 提示末尾的空白行
(setq-default indicate-empty-lines t)
(setq truncate-partial-width-windows t)

;; 提示行尾的空格
(setq-default show-trailing-whitespace t)

;; 以像素为单位的文本间距，nil 表示没有额外的间距
(setq-default line-spacing nil)

;; Ways to highlight the current column
(with-library "vline"
  (autoload 'vline-mode "vline"
    "Highlight the current column" t)
  (autoload 'vline-global-mode "vline"
    "Highlight the current column" t)
  ;; 使用和 hl-line-mode 相同的 face
  (setq vline-face 'hl-line))

;; 即使在中文操作系统，mode-line 和 dired 等模式下星期、月份等信息不用中文
(setq system-time-locale "C")

;; Modeline 的时间显示设置
(setq display-time-24hr-format t)     ; 以 24 小时格式显示时间
(setq display-time-day-and-date nil)  ; 不显示日期以节省空间，
                                      ; 可以进一步用 display-time-format 设置格式
(setq display-time-use-mail-icon nil) ; 在时间旁边的邮件显示
(setq display-time-interval 60)       ; 时间的更新频率
(display-time)                        ; 在 Modeline 显示时间
;; (set-time-zone-rule "Asia/Shanghai")  ; 设置正确的时区（某些版本的 Emacs 不能自动设置准确时区）

;; Modeline 上显示行号、列号
(line-number-mode t)
(column-number-mode t)

;; M-x color-theme-select 选择配色方案，在配色方案上按 I 可以改变当前
;; Frame 的配色，按 i 可以改变所有 Frame 的配色，按 p 可以把当前配色方
;; 案的 Lisp 代码打印出来，加入 .emacs 后就可以不用加载 color-theme
(when (> (display-color-cells) 16)
  (with-without-library "color-theme"
    ((require 'color-theme)
     (color-theme-initialize)
     ;; (color-theme-calm-forest)
     (color-theme-dark-laptop))
    ((setq default-frame-alist
           '((foreground-color . "Wheat")
             (background-color . "DarkSlateGray")
             (cursor-color . "Orchid")
             (mouse-color . "slateblue"))))))

;; 单独设置背景色
;; (set-background-color "darkblue")

;;; View, Navigator

;; 支持查看图片
(auto-image-file-mode t)

;; 读 man 文档时，使用当前 window
(setq Man-notify-method 'pushy)

(setq outline-minor-mode-prefix [(control o)])

(robust-require hideshow
  ;; 在需要的 mode 中使用 hideshow
  ;; (dolist (hook '(c++-mode-hook c-mode-hook))
  ;;   (add-hook hook 'hs-minor-mode))

  ;; 为 Hideshow Mode 设置和 Outline Mode 相似的键绑定
  (define-key hs-minor-mode-map (kbd "C-o C-a") 'hs-show-all)
  (define-key hs-minor-mode-map (kbd "C-o C-t") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-o C-s") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-o C-c") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-o C-o") 'hs-toggle-hiding))

;; 起始移动点在行末的话，垂直移动时始终保持在行末
(setq track-eol t)

(defun wb-next-line (&optional line)
  "next-line over continuation lines"
  (interactive "p")
  (when (not (memq last-command '(wb-next-line wb-previous-line)))
    (setq temporary-goal-column
          (mod (current-column)
               (if truncate-lines (screen-width) (window-width)))))
  (vertical-motion line)
  (forward-char
   (if (< (- (line-end-position) (point)) temporary-goal-column)
       (- (line-end-position) (point))
     temporary-goal-column)))

(defun wb-prev-line (&optional line)
  "previous-line over continuation lines"
  (interactive "p")
  (wb-next-line (- 0 line)))

(global-set-key "n" (quote wb-next-line))
(global-set-key "p" (quote wb-prev-line))

;;; Search

;; 搜索时翻页查看，搜索时使用 C-v M-v C-l 不会中断搜索
(setq isearch-allow-scroll t)
;; 搜索对大小写敏感
;; (setq-default case-fold-search t)

(defun isearch-to-query-replace ()
  "Go straight from isearch(or regexp isearch) into query
replace. Replace the text that you're presently isearching for."
  (interactive)
  (let ((start (min (point) (or isearch-other-end (point-max))))
        (func (if isearch-regexp 'query-replace-regexp 'query-replace)))
    (isearch-done)
    (isearch-clean-overlays)
    (goto-char start)
    (let ((query-replace-interactive t))
      (call-interactively func))))

;; 在 isearch 过程中，用 C-o 显示当前查找内容的 occur 结果
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun isearch-yank-symbol-simple ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(defun isearch-yank-regexp (regexp)
  "Pull REGEXP into search regexp."
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (isearch-search-and-update))

(defun isearch-yank-symbol (&optional partialp)
  "Put symbol at current point into search string.

  If PARTIALP is non-nil, find all partial matches."
  (interactive "P")
  (let* ((sym (find-tag-default))
         ;; Use call of `re-search-forward' by `find-tag-default' to
         ;; retrieve the end point of the symbol.
         (sym-end (match-end 0))
         (sym-start (- sym-end (length sym))))
    (if (null sym)
        (message "No symbol at point")
      (goto-char sym-start)
      ;; For consistent behavior, restart Isearch from starting point
      ;; (or end point if using `isearch-backward') of symbol.
      (isearch-search)
      (if partialp
          (isearch-yank-string sym)
        (isearch-yank-regexp
         (concat "\\_<" (regexp-quote sym) "\\_>"))))))

(defun isearch-current-symbol (&optional partialp)
  "Incremental search forward with symbol under point.

  Prefixed with \\[universal-argument] will find all partial
  matches."
  (interactive "P")
  (let ((start (point)))
    (isearch-forward-regexp nil 1)
    (isearch-yank-symbol partialp)))

(defun isearch-backward-current-symbol (&optional partialp)
  "Incremental search backward with symbol under point.

  Prefixed with \\[universal-argument] will find all partial
  matches."
  (interactive "P")
  (let ((start (point)))
    (isearch-backward-regexp nil 1)
    (isearch-yank-symbol partialp)))

;; F3 向下正则搜索当前光标所在的单词，C-F3 则向上正则搜索
;; 如果不希望正则搜索，可以用 C-u F3 或者 C-u C-F3
(global-set-key [f3] 'isearch-current-symbol)
(global-set-key [(control f3)] 'isearch-backward-current-symbol)
(define-key isearch-mode-map [f3] 'isearch-repeat-forward)
(define-key isearch-mode-map [(control f3)] 'isearch-repeat-backward)

;; 在 isearch 过程中，用 C-h 查看 isearch-mode 的 help
(define-key isearch-mode-map [(control h)] 'isearch-mode-help)

;; 在 isearch 过程中，用 PageDown 和 PageUp 继续向下/向上搜索
(define-key isearch-mode-map '[next]  'isearch-repeat-forward)
(define-key isearch-mode-map '[prior] 'isearch-repeat-backward)

;; 启动 isearch 后，用 C-a 搜索当前单词
(define-key isearch-mode-map "\C-a" 'isearch-yank-symbol-simple)
(define-key isearch-mode-map "\M-%" 'isearch-to-query-replace)

;; 因为 occor 只搜索出匹配行，所以不希望折行
(add-hook 'occur-mode-hook (lambda () (setq truncate-lines t)))

;; 在 Occur Mode 中定义 flush 和 keep 的快捷键

(define-key occur-mode-map "F"
  (lambda (str) (interactive "sflush: ")
    (let ((buffer-read-only))
      (save-excursion
        (beginning-of-buffer)
        (flush-lines str)))))

(define-key occur-mode-map "K"
  (lambda (str) (interactive "skeep: ")
    (let ((buffer-read-only))
      (save-excursion
        (beginning-of-buffer)
        (keep-lines str)))))

;; 自定义 grep 命令
;; (setq grep-command "grep -i -nH -e ")

;;; Replace

(defalias 'qrr 'query-replace-regexp)

;;; Edit

;; 设置缺省的 mode 为 text-mode，而不是一点功能都没有的 fundamental-mode
(setq default-major-mode 'text-mode)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; 选中了一些文字时，如果再输入一个字符，这个字符把选中的文字替换掉，而
;; 不是直接在光标的位置插入。也可以按 DEL 将选中的文件删除
(delete-selection-mode t)

;; 设定删除保存记录为 200，可以方便以后无限恢复
(setq kill-ring-max 200)

;; 70 是 Emacs 的缺省值
(setq default-fill-column 70)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; 在文档最后自动插入一个空行
;; 可以用 mode-require-final-newline 针对 mode 设置
(setq require-final-newline 't)

;; 允许 Emacs 和外部其他程序的复制粘贴
(setq x-select-enable-clipboard t)

;; Minibuffer 交互功能的设置
(icomplete-mode t)            ; 自动提示补全函数和变量
(partial-completion-mode t)   ; 首字母完成功能，比如 q-r-r 相当于 query-replace-regexp
(fset 'yes-or-no-p 'y-or-n-p) ; 所有的问题用 y/n 确认，而不用 yes/no
(setq resize-mini-windows 'grow-only) ; 允许 minibuffer 变化大小
(setq enable-recursive-minibuffers t) ; 可以递归的使用 minibuffer

;; M-@ 选择当前整个单词，原来的功能可以用 C-@ M-f 代替
(defun wb-mark-word (&optional arg allow-extend)
  (interactive "P\np")
  (progn
    (mark-word arg allow-extend)
    (skip-chars-backward "a-zA-Z")))

(global-set-key (kbd "M-@") 'wb-mark-word)

;; Preserve hard links to the file you’re editing (this is especially important if you edit system files).
;; (setq backup-by-copying-when-linked t)
;; Preserve the owner and group of the file you’re editing (this is especially important if you edit files as root).
;; (setq backup-by-copying-when-mismatch t)

;; C-/ 或 C-_ 或 C-x u：undo
;; C-x C-/ 或 C-+：redo
(robust-require redo
  (defun undo-redo (arg)
    "Undo or redo changes.  If ARG is present or negative, redo ARG
    changes.  If ARG is positive, repeatedly undo ARG changes."
    (interactive "P")
    (if (null arg)
        (undo)
      (let ((n (prefix-numeric-value arg)))
        (cond ((= n 0) (redo))
              ((< n 0) (redo (- n)))
              ((> n 0) (undo n))))))

  (global-set-key (kbd "C-+") 'redo)
  (global-set-key (kbd "C-x C-/") 'redo)
  (global-set-key (kbd "C-x C-_") 'redo)
  (global-set-key (kbd "C-_") 'undo-redo))

;; If the point is at the beginning of the line, move to the first noblank char. To enhance C-a
(defun wb-beginning-of-line ()
  "If the point is not on beginning of current line, move point
to beginning of current line, as 'beginning-of-line' does.
If the point already is on the beginning of current line, then
move the point to the first non-space character, if it exists."
  (interactive)
  (if (not (eq (point) (line-beginning-position)))
      (beginning-of-line)
    (when (re-search-forward "\[^\[:blank:\]　\]" (line-end-position) t)
      (backward-char))
    ))
(global-set-key [?\C-a] 'wb-beginning-of-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 有时觉得选中再用C-w或者M-w很麻烦，有点羡慕vim中类似dd和yy的功能。然后发现没有选
;; 中一块内容时，C-w和M-w都没有任何作用，刚好利用一下。
;; 将下面内容加入.emacs，则当你选中一块区域时，C-w和M-w做的事和以前完全相同。如果没
;; 有选中区域，则C-w是kill当前行，M-w是copy当前行，就像dd和yy一样。而且可以传递参数
;; 给C-w和M-w，表示kill或者copy多行，行数从当前行算起，负数表示往前。
;; 注：两个变量wb-kill-ring-save-include-last-newline（默认为nil)和
;;     wb-kill-region-include-last-newline（默认为t）设置是否将最后一行的换行符包含
;;     进去
(defun wb-kill-ring-save (&optional line)
  "This function is a enhancement of `kill-ring-save', which is normal used
to copy a region.  This function will do exactly as `kill-ring-save' if
there is a region selected when it is called. If there is no region, then do
copy lines as `yy' in vim."
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  (if line
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (when (>= line 2)
          (setq end (line-end-position line)))
        (when (<= line -2)
          (setq beg (line-beginning-position (+ line 2))))
        (if (and wb-kill-ring-save-include-last-newline
                 (not (= end (point-max))))
            (setq end (1+ end)))
        (kill-ring-save beg end))
    (call-interactively 'kill-ring-save)))
;; set the following var to t if you like a newline to the end of copied text.
(setq wb-kill-ring-save-include-last-newline nil)
;; bind it
(global-set-key [?\M-w] 'wb-kill-ring-save)

(defun wb-kill-region (&optional line)
  "This function is a enhancement of `kill-region', which is normal used to
kill a region to kill-ring.  This function will do exactly as `kill-region'
if there is a region selected when it is called. If there is no region, then
do kill lines as `dd' in vim."
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  (if line
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (when (>= line 2)
          (setq end (line-end-position line)))
        (when (<= line -2)
          (setq beg (line-beginning-position (+ line 2))))
        (if (and wb-kill-region-include-last-newline
                 (not (= end (point-max))))
            (setq end (1+ end)))
        (kill-region beg end))
    (call-interactively 'kill-region)))
;; set the following var to t if you like a newline in the end of killed text.
(setq wb-kill-region-include-last-newline t)
;; bind it
(global-set-key [?\C-w] 'wb-kill-region)

;; M-y 时浏览 kill ring
;; http://www.todesschaf.org/projects/bkr.html
(robust-require browse-kill-ring
  (browse-kill-ring-default-keybindings))

;; 开启一些缺省被禁止 feature
(put 'set-goal-column 'disabled nil)  ; C-x C-n
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'narrow-to-page 'disabled nil)   ; C-x n p
(put 'narrow-to-defun 'disabled nil)  ; C-x n d
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)      ; C-x <,  C-x >

;; Backup 策略
(setq version-control t)     ; 启用文件备份方式的版本控制
(setq kept-old-versions 2)   ; 备份最原始的版本两次，即第一次、第二次编辑前的文件
(setq kept-new-versions 5)   ; 备份最新的版本五次
(setq delete-old-versions t) ; 删掉不属于以上12中版本的版本
;; 设置备份文件的路径
(setq backup-directory-alist
      '(("" . "~/.emacs.d/auto-backup")))
;; 备份设置方法，直接拷贝
(setq backup-by-copying t)
(setq make-backup-files t)
;; 使用 Tramp 编辑文件时，也使用和 backup-directory 相同的备份目录
;; (setq tramp-backup-directory-alist backup-directory-alist)

;; Auto Save 策略
;; auto-save-default 为 t（除了 batch mode），所以缺省打开 Auto Save
(setq auto-save-interval 100)            ; 每输入 N 个字符后自动保存
(setq auto-save-timeout 30)              ; 至少 N 秒后才自动保存
(setq delete-auto-save-files t)
(setq auto-save-file-name-transforms
      `(;; 缺省值，Tramp 编辑文件时，自动保存到本地的 tmp 目录
        ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(concat temporary-file-directory "\\2") t)
        ;; 编辑 dropbox 的文件时，自动保存到本地的 tmp 目录
        ("\\`/?\\([^/]*/\\)*\\.?[Dd]ropbox/\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(concat temporary-file-directory "\\3") t)
        ;; 下面的规则适用于 *nix 平台所有文件
        ;; ("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.emacs.d/auto-save/\\2" t)
        ))

;; 时间戳（time-stamp）设置，记录文档保存的时间。如果文档里有
;; Time-stamp: 的文字，就会自动保存时间戳
(setq time-stamp-active t)                ; 启用时间戳
(setq time-stamp-warn-inactive t)         ; 去掉时间戳的警告
(setq time-stamp-format
      "%:u %02m/%02d/%04y%02H:%02M:%02S") ; 设置time-stamp的格式
(add-hook 'write-file-hooks 'time-stamp)  ; 保存文件时更新时间戳

;; Chmod of scripts to u+x
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; Register, Bookmark

;; 设置常用的文件和目录，可以用 "C-x r j R" 快速访问
(set-register ?e '(file . "~/.emacs.d/elisp-init/wb-emacs-init.el"))
(set-register ?g '(file . "~/.emacs.d/org/gtd"))

;; Emacs 内置的 bookmark
;; bookmark-set    C-x r m
;; bookmark-jump   C-x r b
;; list-bookmarks  C-x r l
(setq bookmark-default-file "~/.emacs.d/.emacs.bmk") ; 自定义 bookmark 文件的位置
(setq bookmark-save-flag 1)                          ; 设置 bookmark 同时 save

;; 参考命令

;; bm：支持当个文件内的 bookmark，高亮设置 bookmark 的行
(robust-require bm
  (global-set-key (kbd "<C-f2>")   'bm-toggle)
  (global-set-key (kbd "<M-f2>")   'bm-show)
  (global-set-key (kbd "<f2>")     'bm-next)
  (global-set-key (kbd "<S-f2>")   'bm-previous))

;;; Buffers, Files, Dired

;; 当打开两个同名的文件，在 buffer 名字前面加上目录名
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; 把常用文件加入 File Name Cache
(lazy-require 'filecache)

(eval-after-load "filecache"
  '(progn
     (message "Loading file cache...")
     (file-cache-add-directory-using-find "~/muse/source/")
     (file-cache-add-directory-list (list "~/" "~/bin"))
     (file-cache-add-directory "~/.emacs.d/elisp-init")
     (file-cache-add-file-list (list "~/.bash_profile" "~/.bashrc"))))

(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))

(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
         (lambda ()
           (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))

(global-set-key (kbd "ESC ESC f") 'file-cache-ido-find-file)

;; ido
(robust-require ido
  (setq ido-save-directory-list-file    ; 自定义 ido 文件的路径
        (expand-file-name "~/.emacs.d/.ido.last"))
  (ido-mode t)                         ; 启用 ido mode
  (setq ido-enable-flex-matching t)    ; 可以用 wei 匹配文件名 wb-emacs-init.el
  (setq ido-create-new-buffer 'always) ; 没有名字匹配的 buffer 时，直接创建新 buffer
  (setq ido-use-filename-at-point t)   ; 先查找光标处文件，可以用于替换 ffap
  (add-hook 'ido-setup-hook
            (lambda ()
              ;(define-key ido-completion-map [tab] 'ido-complete)
              (define-key ido-completion-map "\C-h" 'ido-delete-backward-updir)
              ;; 缺省为 C-s 和 C-r，不习惯，增加 C-n 和 C-p
              (define-key ido-completion-map "\C-n" 'ido-next-match)
              (define-key ido-completion-map "\C-p" 'ido-prev-match))))

;; ibuffer
(with-library "ibuffer"
  (autoload 'ibuffer "ibuffer" "Dired lik Ibuffer." t)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("C/C++" (or
                           (mode . c-mode)
                           (mode . c++-mode)))
                 ("Muse" (mode . muse-mode))
                 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (mode . emacs-lisp-mode)))
                 ("LSC" (or
                         (mode . lsc-acd-mode)
                         (mode . lsc-bfd-mode)
                         (mode . lsc-mrp-mode)
                         (mode . lsc-par-mode)
                         (mode . lsc-ncl-mode)))
                 ("Dired" (mode . dired-mode))
                 ("Gnus" (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . mail-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

;; 一些 buffer 长时间不用的话自动关闭
;; 可以用 (mapc (lambda (x) (add-hook x 'turn-on-tempbuf-mode))
;;         '(dired-mode-hook custom-mode-hook)))
;; 方式批量设置，但可读性还是分开设置的好
(robust-require tempbuf
  (setq tempbuf-minimum-timeout 3600)
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'Man-mode-hook 'turn-on-tempbuf-mode))

;; 在保存文件时，如果目录不存在（如打开 foo/bar 文件），则自动创建目录
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;; Dired Settings

;;设置 ls 的参数（缺省为 -al），显示易读的大小，按版本排序
(setq dired-listing-switches "-avhl")

;; dired-mode 下不折行显示
(defun wb-dired-long-lines ()
  (setq truncate-lines t))
(add-hook 'dired-after-readin-hook 'wb-dired-long-lines)

;; 复制和删除时递归处理子目录
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; 复制和移动时把当前 emacs 中另一个窗口中的目录为对象。这通常是我们希望的方式。
(setq dired-dwim-target t)

;; 另外 dired-mode 下还有不常用但是比较有用的命令。比如
;; dired-compare-directories 可以用于比较文件夹。

;; dired-x 是 dired-mode 的一个扩展。提供了许多很有用的命令和特性。
;; 1. 隐藏配置文件和其它类型的文件。通过设置 dired-omit-extensions 和
;;    dired-omit-files
;; 2. 把文件和特定的 shell 程序关联。通过设置
;;    dired-guess-shell-alist-default 或 dired-guess-shell-alist-user，
;;    在文件上使用 ! 会调用相应的命令

(robust-require dired-x
  (add-hook 'dired-mode-hook
            (lambda ()
              (setq dired-omit-files-p t)))
  ;; 忽略指定名字的目录和后缀文件
  (setq dired-omit-extensions
        '("CVS/" ".o" "~" ".bak" ".obj" ".map"))
  ;; 隐藏 . 和 ..，以及以 . 引导的目录/文件，以# 引导的文件，以 ~ 引导
  ;; 的文件等，可以使用 M-o 切换隐藏和显示
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.\\|^~")
  ;; 设置文件对应的命令
  (setq dired-guess-shell-alist-user
        (list
         (list "\\.tar\\.bz2$" "tar jxvf * &")
         '("\\.tar\\.gz$" "tar zxvf * &")
         '("\\.chm$" "chmsee * &")
         '("\\.tar$" "tar xvvf * &")
         '("\\.ps$" "gv * &")
         '("\\.html?$" "firefox * &" "urxvt -e w3m * &")
         '("\\.pdf$" "acroread * &" "evince * &")
         '("\\.\\(jpe?g\\|gif\\|png\\|bmp\\|xbm\\|xpm\\|fig\\|eps\\)$"
           "gthumb * &" "gqview *  &" "display *   &" "xloadimage *   &" )
         '("\\.\\([Ww][Mm][Vv]\\|[Vv][Oo][Bb]\\|[Mm][Pp][Ee]?[Gg]\\|asf\\|[Rr][Aa]?[Mm]\\)$"
           "mplayer * &")
         '("\\.rmvb$" "mplayer * &")
         '("\\.RMVB$" "mplayer * &")))
  (add-to-list 'dired-guess-shell-alist-default '("\\.dvi$" "dvipdfmx"))
  (add-to-list 'dired-guess-shell-alist-default '("\\.pl$" "perltidy")))

;; wdired 提供修改文件名的一种非常方便方法。它把 dired-mode 当作一般的
;; 文本处理，这样无论是修改一个文件，还是批量修改文件都不是一般的爽。
(robust-require wdired
  (autoload 'wdired-change-to-wdired-mode "wdired")
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;; List directories first in dired mode
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'sof/dired-sort)

;;; Self Documentation

;; 增大 apropos 函数查找的范围
(setq apropos-do-all t)

;; 添加自己的 info 文件目录，可以在 list 里添加多个目录
;; 也可以通过 shell 的环境变量 $INFOPATH 设置
(setq Info-default-directory-list (append
                                   Info-default-directory-list
                                   '("~/.emacs.d/info")))

;; Xray provides a way to display internal Emacs object structures.
;; Usage: M-x xray-symbol RET describe-function RET
;; http://www.emacswiki.org/cgi-bin/wiki/XrayPackage
(with-library "xray"
  (autoload 'xray-symbol "xray" "Display internal Emacs object structures." t))

;;; Misc

(setq default-directory "~/")

;; Shell Mode
;; Color support
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;; wb-template.el

(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
(setq save-abbrevs nil)

(robust-require autoinsert
  (auto-insert-mode))

;; 设置 hippie-expand 的补全方式。这是一个优先列表，hippie-expand 会依
;; 次尝试列表中的函数来补全。当前使用的匹配方式会在 echo 区域显示
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev                 ; 搜索当前 buffer
        try-expand-line                    ; 补全当前行
        try-expand-line-all-buffers
        try-expand-list                    ; 补全一个列表
        try-expand-list-all-buffers
        try-expand-dabbrev-visible         ; 搜索当前可见窗口
        try-expand-dabbrev-all-buffers     ; 搜索所有 buffer
        try-expand-dabbrev-from-kill       ; 搜索 kill-ring
        try-complete-file-name             ; 文件名匹配
        try-complete-file-name-partially   ; 文件名部分匹配
        try-complete-lisp-symbol           ; 补全 lisp symbol
        try-complete-lisp-symbol-partially ; 部分补全 elisp symbol
        try-expand-whole-kill))
(global-set-key "\M-/" 'hippie-expand)

(robust-require snippet)

(with-library "company"
  (autoload 'company-mode "company" nil t)
  (setq company-idle-delay t)
  (setq company-begin-commands '(self-insert-command)))

;;;; wb-muse.el

(robust-require muse-autoloads
  (eval-after-load "muse-mode"
    '(progn
       ;; 加载需要的格式和其他辅助 library
       (require 'muse-html)         ; load (X)HTML publishing style
       (require 'muse-latex)        ; load LaTeX/PDF publishing styles
       (require 'muse-project)      ; publish files in projects
       (require 'muse-latex2png)    ; publish <latex> tags
       (require 'muse-colors)       ; load coloring/font-lock module
       (require 'muse-wiki)         ; load Wiki support

       ;; 设置输出 HTML 编码
       (setq muse-html-charset-default "utf-8")
       (setq muse-html-encoding-default 'utf-8)

       ;; Muse 项目的源文件和输出文件目录
       (defvar wb-muse-sd "~/muse/source/"  "My muse source directory.")
       (defvar wb-muse-pd "~/muse/publish/" "My muse publish directory.")

       ;; 自定义输出格式
       (muse-derive-style "wiki-xhtml" "xhtml"
                          :header (concat wb-muse-pd "common/templates/header.html")
                          :footer (concat wb-muse-pd "common/templates/footer.html"))

       ;; Muse 项目
       (setq muse-project-alist
             `(("Computer"
                (,(concat wb-muse-sd "computer")
                 :default "Computer"
                 :force-publish ("WikiIndex"))
                (:base "wiki-xhtml" :path ,(concat wb-muse-pd "computer")))
               ("Emacs"
                (,(concat wb-muse-sd "emacs")
                 :default "Emacs"
                 :force-publish ("WikiIndex"))
                (:base "wiki-xhtml" :path ,(concat wb-muse-pd "emacs")))
               ("Programming"
                (,(concat wb-muse-sd "programming")
                 :default "Programming"
                 :force-publish ("WikiIndex"))
                (:base "wiki-xhtml" :path ,(concat wb-muse-pd "programming")))
               ("SPA"
                (,(concat wb-muse-sd "spa")
                 :default "SPA"
                 :force-publish ("WikiIndex"))
                (:base "wiki-xhtml" :path ,(concat wb-muse-pd "spa")))
               ("Reading"
                (,(concat wb-muse-sd "reading")
                 :default "Reading"
                 :force-publish ("WikiIndex"))
                (:base "wiki-xhtml" :path ,(concat wb-muse-pd "reading")))
               ("ICCAD"
                (,(concat wb-muse-sd "iccad")
                 :default "ICCAD"
                 :force-publish ("WikiIndex"))
                (:base "wiki-xhtml" :path ,(concat wb-muse-pd "iccad")))
               ("WiKi" (,@(muse-project-alist-dirs wb-muse-sd)
                           :default "index"
                           :force-publish ("WikiIndex"))
                ,@(muse-project-alist-styles wb-muse-sd
                                             wb-muse-pd
                                             "wiki-xhtml"))))

       ;; 键绑定
       (define-key muse-mode-map (kbd "C-c C-c") 'wb-muse-preview-source)
       (define-key muse-mode-map (kbd "C-c C-j") 'wb-muse-preview-html)
       (define-key muse-mode-map (kbd "C-c C-m") 'wb-muse-preview-with-w3m)

       ;; 其他 Muse 设置

       ;; 禁止把 Project 名作为链接，我不喜欢不受控制的到处高亮
       (setq muse-wiki-ignore-bare-project-names t)
       ;; 象 Outline 那样用颜色表示标题，在终端上特别好用
       (setq muse-colors-autogen-headings 'outline)
       ;; 禁止 evaluate 在 <lisp> 标签中的 lisp 语句
       (setq muse-colors-evaluate-lisp-tags nil)
       ;; C-c TAB u 插入 url 时不自动插入 http://
       (setq muse-insert-url-initial-input "")
       ;; 设置输出 LaTeX 公式的大小
       (setq muse-latex2png-scale-factor 1.5)

       ;; 需要在 Muse 各种 Hook 中加载的设置
       (add-hook 'muse-mode-hook
                 '(lambda ()
                    (outline-minor-mode 1)
                    (setq abbrev-mode 1)
                    (footnote-mode)))
       (add-hook 'muse-before-publish-hook
                 'wb-remove-leading-space)
       (add-hook 'muse-after-publish-hook
                 'wb-muse-remove-html-cjk-space)


       ;; 辅助函数
       (defun wb-muse-relative-path (file)
         (concat
          (file-relative-name
           wb-muse-pd
           (file-name-directory muse-publishing-current-output-path))
          file))

       (defun wb-muse-remove-html-cjk-space ()
         "删除输出 HTML 时两行中文之间的空格。"
         (when (string= (muse-style-element :base muse-publishing-current-style) "html")
           (save-excursion
             (goto-char (point-min))
             (while (re-search-forward "\\(\\cc\\)\n\\(\\cc\\)" nil t)
               (unless (get-text-property (match-beginning 0) 'read-only)
                 (replace-match "\\1\\2"))))))

       (defun wb-remove-leading-space ()
         "删除行首缩进的两个空格。"
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward "\n\n  \\([^-]\\)" nil t)
             (unless (get-text-property (match-beginning 0) 'read-only)
               (replace-match "\n\n\\1")))))

       (defun wb-muse-output-file ()
         "Get output file name"
         (let ((styles (muse-project-applicable-styles buffer-file-name
                                                       (cddr (muse-project))))
               output-dir)
           (while (and styles
                       (progn
                         (setq output-dir (muse-style-element :path (car styles)))
                         (not (file-exists-p output-dir))))
             (setq styles (cdr styles)))
           (when output-dir
             (muse-publish-output-file
              buffer-file-name
              output-dir
              "html"))))

       (defun wb-muse-preview-with-w3m ()
         "Preview the html file with w3m."
         (interactive)
         (muse-project-publish-this-file)
         (let ((file (wb-muse-output-file)))
           (w3m-goto-url (concat "file://" file))))

       (defun wb-muse-preview-html ()
         "Preview the html file with web browser."
         (interactive)
         (muse-project-publish-this-file)
         (browse-url (wb-muse-output-file)))

       (defun wb-muse-preview-source ()
         "Find and open the html file in emacs."
         (interactive)
         (muse-project-publish-this-file)
         (find-file (wb-muse-output-file)))

       ;; Muse Mode 的 Skeleton
       (define-skeleton skeleton-muse-mode-auto-insert
         "Auto insert to new muse file." "Title: "
         "#title " str \n \n "<contents>" \n \n "* " _)

       (defvar muse-src-tag-lang-last nil)
       (defvar muse-src-tag-lang-history nil)

       (setq muse-src-tag-lang-alist
             '(("c++")
               ("conf")
               ("emacs-lisp")
               ("python")
               ("ruby")
               ("sh")))

       (define-skeleton skeleton-muse-mode-tag-src
         "Insert muse mode src tag"
         (identity
          (setq muse-src-tag-lang-last
                (completing-read
                 (if (> (length muse-src-tag-lang-last) 0)
                     (format "Lang (default %s): " muse-src-tag-lang-last)
                   "Lang: ")
                 muse-src-tag-lang-alist nil nil nil
                 'muse-src-tag-lang-history muse-src-tag-lang-last)))
         "<src lang=\"" str "\">" \n
         _ \n
         "</src>" \n \n)

       (define-skeleton skeleton-muse-mode-tag-example
         "Insert muse mode example tag"
         nil
         "<example>" \n
         _ \n
         "</example>")

       (define-skeleton skeleton-muse-mode-tag-latex
         "Insert muse mode example tag"
         nil
         "<latex>" \n
         "\\[" \n
         _ \n
         "\\]" \n
         "</latex>")

       ;; 绑定 skeleton 到 auto insert
       (define-auto-insert '(muse-mode . "muse document")
         'skeleton-muse-mode-auto-insert)

       ;; 绑定 skeleton 到 abbrev
       (define-abbrev-table 'muse-mode-abbrev-table
         '(("src" "" skeleton-muse-mode-tag-src)
           ("ex"  "" skeleton-muse-mode-tag-example)
           ("la"  "" skeleton-muse-mode-tag-latex)))
       )))

;;;; wb-modes.el

(require 'generic-x)

(defvar lsc-ncl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\_ "w" st)     ; 定义 _ 为 word 的一部分
    (modify-syntax-entry ?\\ "w" st)     ; 定义 \ 为 word 的一部分
    st)
  "Syntax table for `lsc-ncl-mode'.")

(define-generic-mode 'lsc-ncl-mode
  '("//" "::")
  '("design" "architecture" "device" "package" "speed" "property"
    "logical" "site" "cellmodel-name" "program" "primitive"
    "signal-pins" "route")
  '(("\\<\\(comp\\|signal\\)\\>\\s-+\\(.+\\)" ; 定义 comp SLICE_0 的 face
     (1 'font-lock-type-face)                 ; comp 关键字用 type
     (2 'font-lock-variable-name-face)) ; SLICE_0 名字用 variable
    ("\\[[ABCD0123,FIMOX]+\\]" . 'font-lock-comment-face)) ; 增加一种注释
  '(".ncl\\'")
  (list
   (function
    (lambda ()
      (progn
        (set-syntax-table lsc-ncl-mode-syntax-table)
        (setq imenu-generic-expression
              '((nil "\\(\\<\\(comp\\|signal\\)\\>\\s-+.+\\)" 1)
                ("*Comps*" "\\<\\(comp\\)\\>\\s-+\\(.+\\)" 2)
                ("*Signals*" "\\<\\(signal\\)\\>\\s-+\\(.+\\)" 2)))
        (set (make-local-variable 'outline-regexp)
             "   \\(\\<device\\|property\\|comp\\|signal\\>\\)")
        (outline-minor-mode 1)
        (hide-body)))))
  "Major mode to support lsc ncl files.")

(define-generic-mode 'lsc-acd-mode
  '("#")
  nil
  '(("^\\([: ]\\{2\\}.*\\)\\s-*=" . 'font-lock-keyword-face) ; key
    ("\\<[0-9.]+\\>" . 'font-lock-variable-name-face))       ; 数字
  '(".acd\\'")
  nil
  "Major mode to support lsc acd files.")

(define-generic-mode 'lsc-bfd-mode
  '("#")
  '("Tile" "Sites" "Nodes" "Ram")
  '(("\\<\\(Columns\\|Nodes\\|R\\(?:ams\\|ows?\\)\\)=\\([0-9]+\\)\\>"
     (1 'font-lock-type-face)
     (2 'font-lock-variable-name-face)))
  '(".bfd\\'")
  (list
   (function
    (lambda ()
      (progn
        (setq imenu-generic-expression
              '((nil "^Tile \"\\(.*?\\)\"" 1)))
        (set (make-local-variable 'outline-regexp)
             "Tile\\|  Nodes\\|  Ram\\|  Sites\\|  #equations")
        (outline-minor-mode 1)
        (hide-body)
        (toggle-truncate-lines 1)))))
  "Major mode to support lsc bfd files.")

(define-generic-mode 'lsc-mrp-mode
  nil
  '("Design Information" "Design Summary")
  '(("^.*?:" . 'font-lock-builtin-face)
    ("\\<[0-9.]+\\>" . 'font-lock-variable-name-face)) ; 数字
  '(".mrp\\'")
  nil
  "Major mode to support lsc mrp files.")

(define-generic-mode 'lsc-par-mode
  nil
  '("End of iteration" "Starting Placer Phase")
  '(("Command line: \\(.*\\)" . 'hi-yellow)
    ("\\<[0-9.]+\\>" . 'font-lock-variable-name-face)) ; 数字
  '(".par\\'")
  nil
  "Major mode to support lsc par files.")

(define-generic-mode 'lsc-prf-mode
  '("#")
  '("SCHEMATIC START" "FREQUENCY" "FREQUENCY PORT" "FREQUENCY NET" "SCHEMATIC END")
  nil
  '(".prf\\'")
  nil
  "Major mode to support lsc prf files.")

(define-generic-mode 'eml-mode
  nil
  nil
  '(("^\\(Bcc\\|\\(?:Cc\\|To\\):\\)" . 'font-lock-keyword-face)
    ("^Subject.*$" . 'font-lock-keyword-face)
    ("^\\(>\\|-=\\).*$" . 'dired-ignored))
  '(".eml\\'")
  (list
   (function
    (lambda ()
      (flyspell-mode 1)))
   (function
    (lambda ()
      (auto-fill-mode))))
  "Major mode to support eml files.")

;;;; wb-de.el

;; smart-compile 根据当前文件名，提供合适的编译、运行命令
(robust-require smart-compile+
  (setq smart-compile-alist
        '(("\\.c$"          . "gcc -Wall -ggdb -o %n %f")
          ("\\.[Cc]+[Pp]*$" . "g++ -o %n %f")
          ("\\.java$"       . "javac %f")
          ("\\.f90$"        . "f90 %f -o %n")
          ("\\.[Ff]$"       . "f77 %f -o %n")
          ("\\.mp$"         . "runmpost.pl %f -o ps")
          ("\\.php$"        . "php %f")
          ("\\.tex$"        . "latex %f")
          ("\\.l$"          . "lex -o %n.yy.c %f")
          ("\\.y$"          . "yacc -o %n.tab.c %f")
          ("\\.rb$"         . "ruby %f")
          ("\\.py$"         . "python %f")
          ("\\.sql$"        . "mysql < %f")
          ("\\.ahk$"        . "start d:\\Programs\\AutoHotkey\\AutoHotkey %f")
          ("\\.sh$"         . "./%f")
          (emacs-lisp-mode  . (emacs-lisp-byte-compile))))
  (setq smart-run-alist
        '(("\\.c$"          . "./%n")
          ("\\.[Cc]+[Pp]*$" . "./%n")
          ("\\.java$"       . "java %n")
          ("\\.php$"        . "php %f")
          ("\\.m$"          . "%f")
          ("\\.scm"         . "%f")
          ("\\.tex$"        . "dvisvga %n.dvi")
          ("\\.rb$"         . "ruby %f")
          ("\\.py$"         . "python %f")
          ("\\.pl$"         . "perl \"%f\"")
          ("\\.pm$"         . "perl \"%f\"")
          ("\\.bat$"        . "%f")
          ("\\.mp$"         . "mpost %f")
          ("\\.ahk$"        . "start d:\\Programs\\AutoHotkey\\AutoHotkey %f")
          ("\\.sh$"         . "./%f")))
  (setq smart-executable-alist
        '("%n.class"
          "%n.exe"
          "%n"
          "%n.mp"
          "%n.m"
          "%n.php"
          "%n.scm"
          "%n.dvi"
          "%n.rb"
          "%n.py"
          "%n.pl"
          "%n.ahk"
          "%n.pm"
          "%n.bat"
          "%n.sh"))
  ())

;;;; wb-elispde.el

(defun wb-emacs-lisp-mode-hook ()
  (local-set-key (kbd "C-c .") 'wb-jump-to-elisp-defun)
  (if (eq major-mode 'emacs-lisp-mode)
      (setq mode-name "Elisp"))
  (when (boundp 'comment-auto-fill-only-comments)
    (setq comment-auto-fill-only-comments t)
    (kill-local-variable 'normal-auto-fill-function)))
(add-hook 'emacs-lisp-mode-hook 'wb-emacs-lisp-mode-hook)

(defun my-lisp-interaction-mode-hook ()
  (setq mode-name "Lisp Int"))
(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)

(setq eval-expression-print-level  10
      eval-expression-print-length 100)

;; 增加一些高亮设置
(font-lock-add-keywords
 'emacs-lisp-mode
 '((";" ("\\<\\(GM\\|NB\\|TODO\\|FIXME\\)\\>"  nil nil
         (0 'font-lock-warning-face t)))
   (";" ("[* ]\\*[ \t]*\\(\\w.*\\)\\*" nil nil
         (1 'font-lock-warning-face t)))))

;;;; wb-cppde.el

;; CC Mode 配置  http://cc-mode.sourceforge.net/
(require 'cc-mode)

(robust-require xcscope
  (define-key cscope-list-entry-keymap "q" 'wb-quit-buffer))

(with-library "gtags"
    (autoload 'gtags-mode "gtags" "" t))

(robust-require xgtags
  (defun xgtags-pop-stack ()
    "Move to previous point on the stack."
    (interactive)
    (let ((delete (and xgtags-kill-buffers
                       (not (xgtags--stacked-p (current-buffer)))))
          (context (xgtags--pop-context)))
      (assert context nil "The tags stack is empty")
      (when delete
        (kill-buffer (current-buffer)))
      (when (> (count-windows) 1)       ; 增加这两行代码，退出 xgtags 时
        (delete-window))                ; 关闭因为 xgtags 弹出的 window
      (xgtags--update-buffer context)
      (switch-to-buffer (xgtags--context-buffer context))
      (goto-char (xgtags--context-point context))))

  (add-hook 'xgtags-select-mode-hook
              (lambda ()
                (define-key xgtags-select-mode-map (kbd "o")
                  'xgtags-select-tag-near-point))))

;; emacs21 好像没有 number-sequence 函数，那就用其它代替好了。比如
;; (require 'cl) 后用 loop 命令，或者这样
(when (not (fboundp 'number-sequence))
  (defun number-sequence (from &optional to inc)
    (if (and to (<= from to))
        (cons from
              (number-sequence (+ from (or inc 1)) to inc)))))
;; 注意上面的定义虽然比 subr.el 中简洁，但是会出错。只有没有 number-sequence
;; 这个函数时才用。

;; 所有基于 C 的语言的通用设置
(defun wb-c-mode-common-hook()
  (c-toggle-electric-state 1)
  (c-toggle-auto-newline 1)
  (c-toggle-hungry-state 1)
  ;; clean up 方式
  (setq c-cleanup-list
        '(scope-operator
          defun-close-semi))
  ;; tab 设置
  (setq tab-width 4)
  (set (make-local-variable 'tab-stop-list)
       (number-sequence tab-width 80 tab-width))
  (setq c-basic-offset tab-width)
  ;; 预处理设置
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  ;; 编译命令
  (when (featurep 'smart-compile+)
    (define-key c-mode-map [(f5)] 'smart-run)
    (define-key c-mode-map [(f7)] 'smart-compile))
  ;; 调试环境
  (setq gdb-show-main t)
  (setq gdb-many-windows t)
  ;; 一起启动的 Minor Modes
  (setq hs-minor-mode t)
  (setq abbrev-mode t)
  (which-func-mode t)
  ;; 其他键绑定
  (local-set-key  (kbd "C-c o") 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'wb-c-mode-common-hook)

;; C 语言特殊设置
(defun wb-c-mode-hook()
  (c-set-style "stroustrup"))
(add-hook 'c-mode-hook 'wb-c-mode-hook)

;; C++ 语言特殊设置

;; 某些开发环境使用 .c/.h 文件开发 C++ 语言
(add-to-list 'auto-mode-alist '("rel/env[^/]*/\\([^/]*/\\)*[^/]*\\.c$" . c++-mode))
(add-to-list 'auto-mode-alist '("rel/env[^/]*/\\([^/]*/\\)*[^/]*\\.h$" . c++-mode))

(defun wb-c++-mode-hook()
  (c-set-style "stroustrup")
  (c-set-offset 'inline-open 0)
  (c-set-offset 'friend '-)
  ;; gtags，优先使用 xgtags-mode
  (cond ((fboundp 'xgtags-mode) (xgtags-mode t))
        ((fboundp 'gtags-mode)  (gtags-mode t)))
  ;; xref
  )
(add-hook 'c++-mode-hook 'wb-c++-mode-hook)

;; 设置编译命令和环境

(setq compilation-window-height 8)
(setq compilation-finish-functions
      (lambda (buf str)
        ;; grep 结果不能自动关闭，这里也可以用 (equal major-mode 'c++-mode) 判断
        (when (not (or (string-match "*grep*" (buffer-name buf))
                       (string-match "*search*" (buffer-name buf))))
          (if (string-match "exited abnormally" str)
              ;;there were errors
              (message "compilation errors, press C-x ` to visit")
            ;; no errors, make the compilation window go away in 2 seconds
            (run-at-time 2 nil 'delete-windows-on buf)
            (message "NO COMPILATION ERRORS!")))))

(add-hook 'gud-mode-hook
          (lambda ()
            (define-key gud-mode-map (kbd "<f10>") 'gud-next)
            (define-key gud-mode-map (kbd "<f11>") 'gud-step)))

;;;; wb-rudyde.el

;; Ruby 开发环境
(robust-require ruby-mode
  (autoload 'ruby-mode     "ruby-mode" "Mode for editing ruby source files" t)
  (require 'ruby-electric)
  (autoload 'run-ruby      "inf-ruby"  "Run an inferior Ruby process")
  (autoload 'inf-ruby-keys "inf-ruby"  "Set local key defs for inf-ruby in ruby-mode")
  (autoload 'rubydb        "rubydb3x"  "Ruby debugger" t)

  (setq auto-mode-alist
        (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
  (setq interpreter-mode-alist
        (append '(("ruby" . ruby-mode)) interpreter-mode-alist))

  (setq ri-ruby-script
        (expand-file-name "~/.emacs.d/elisp-3rdparty/ruby/ri-emacs/ri-emacs.rb"))
  (autoload 'ri "~/.emacs.d/elisp-3rdparty/ruby/ri-emacs/ri-ruby.el" nil t)

  (fset 'ruby-hash-header
        "#!/usr/bin/env ruby\C-m\C-m")

  (defun ruby-eval-buffer () (interactive)
    "Evaluate the buffer with ruby."
    (shell-command-on-region (point-min) (point-max) "ruby" "*ruby-output*"))

  (defun ruby-xmp-region (reg-start reg-end)
    "Pipe the region through Ruby's xmp utility and replace
   the region with the result."
    (interactive "r")
    (shell-command-on-region reg-start reg-end
                             "ruby -r xmp -I/users/bowang/local/lin/lib/ruby/1.8/irb/ -n -e 'xmp($_, \"%l\t\t# %r\n\")'" t))

  ;; 支持 Hide-show
  (add-to-list 'hs-special-modes-alist
               (list 'ruby-mode
                     (concat "\\(^\\s-*"
                             ruby-electric-simple-keywords-re
                             "\\|{\\|\\[\\)")
                     "end\\|\\]\\|}" "#"
                     'ruby-forward-sexp nil))

  ;; 支持 outline-minor-mode
  (defun rb-outline-level ()
    "This gets called by outline to deteremine the level. Just use
the length of the whitespace"
    (let (buffer-invisibility-spec)
      (save-excursion
        (skip-chars-forward "\t ")
        (current-column))))

  (add-hook 'ruby-mode-hook
            '(lambda ()
               ;; 启动 outline-minor-mode
               (outline-minor-mode t)
               (set (make-local-variable 'outline-regexp)
                    "^if[ \t]\\| *\\(module[ \t]+\\|class[ \t]+\\|def[ \t]+\\)")
               ;; 以行首的空格数目作为 outline level
               (set (make-local-variable 'outline-level) 'rb-outline-level)
               ;; (hide-body)             ; 开始的时候隐藏所有函数的 body
               (inf-ruby-keys)
               (ruby-electric-mode t)
               (setq abbrev-mode t)
               (local-set-key "\C-c\C-c" 'ruby-eval-buffer)
               (local-set-key "\C-[#"    'ruby-hash-header))))

;; Rails 开发环境
(robust-require rinari
  (setq rinari-tags-file-name "TAGS"))

(robust-require rhtml-mode
  (add-to-list 'auto-mode-alist '("\.rhtml$". rhtml-mode))
  (add-to-list 'auto-mode-alist '("\.html\.erb$". rhtml-mode))
  (add-hook 'rhtml-mode-hook
            (lambda () (rinari-launch))))

;; Rails Snippets
(robust-require yasnippet
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/elisp-personal/yasnippets"))

;; YAML 支持
(robust-require yaml-mode
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;;;; wb-pythonde.el

(with-library "python-mode"
  (autoload 'python-mode "python-mode" "Python editing mode." t)

  (setq auto-mode-alist
        (append '(("\\.py$" . python-mode)) auto-mode-alist))
  (setq interpreter-mode-alist
        (append '(("python" . python-mode)) interpreter-mode-alist))

  (defun py-outline-level ()
    "This gets called by outline to deteremine the level. Just use
the length of the whitespace"
    (let (buffer-invisibility-spec)
      (save-excursion
        (skip-chars-forward "\t ")
        (current-column))))

  (defun electric-pair ()
    "Insert character pair without sournding spaces"
    (interactive)
    (let (parens-require-spaces)
      (insert-pair)))

  (add-hook 'python-mode-hook
            (lambda ()
              (set (make-variable-buffer-local 'beginning-of-defun-function)
                   'py-beginning-of-def-or-class)
              ;; 启动 outline-minor-mode
              (outline-minor-mode t)
              ;; 以 * 个空格引导的 def、class 语句作为 outline-regex
              ;; 下面是另外两个例子，还需要研究哪个更好
              ;; "[^ \t]\\|[ \t]*\\(def\\|class\\) "
              ;; "[^ \t\n]\\|[ \t]*\\(def[ \t]+\\|class[ \t]+\\)"
              (set (make-local-variable 'outline-regexp) "[ \t]*\\(def[ \t]+\\|class[ \t]+\\)")
              ; 以行首的空格数目作为 outline level
              (set (make-local-variable 'outline-level) 'py-outline-level)
              (hide-body)  ; 开始的时候隐藏所有函数的 body
              (abbrev-mode)
              (setq py-indent-offset 4)
              ;; 自动输入反括号
              (define-key py-mode-map "\"" 'electric-pair)
              (define-key py-mode-map "\'" 'electric-pair)
              (define-key py-mode-map "(" 'electric-pair)
              (define-key py-mode-map "[" 'electric-pair)
              (define-key py-mode-map "{" 'electric-pair))))

;;;; wb-vhdlde.el

;; 可以参考 xsteve-functions.el

;;;; wb-verilogde.el

;;;; wb-lispde.el
(setq inferior-lisp-program "sbcl")

(with-library "slime"
  (autoload 'slime "slime" "Superior Lisp Interaction Mode for Emacs" t)
  (eval-after-load "slime"
    '(progn
       (slime-setup))))

;;;; wb-schemede.el
(setq scheme-program-name "mzscheme")
(with-library "quack"
  (autoload 'scheme-mode "quack"
    "enhanced support for editing and running Scheme code" t)
  (autoload 'run-scheme "quack"
    "enhanced support for editing and running Scheme code" t))

;;;; wb-perlde.el

(defalias 'perl-mode 'cperl-mode)
(eval-after-load "cperl-mode"
  '(progn
     (setq cperl-indent-level 4
           cperl-hairy t
           cperl-auto-newline nil)

     (define-skeleton skeleton-perl-mode-sub
       "Insert a perl subroutine with arguments."
       "Subroutine name: "
       " " str " {"
       \n "my (" ("Argument name: " "$" str ", ") -2 ") = @_;"
       "\n"
       \n _
       \n "}" '(progn (indent-according-to-mode) nil)
       \n)

     (define-skeleton skeleton-perl-mode-open
       "Insert a perl open file statment."
       ""
       > " " (setq v1 (skeleton-read "File handle: ")) ", \""
       (setq v2 (skeleton-read "File name: ")) "\" or die \"Cannot "
       (setq v3 (skeleton-read "Read/Write/Create? ")) " " v2 ": $!.\\n\";"
       \n
       \n)))

;;;; wb-shde.el

(defun sh-check-finish-hook (buf msg)
  "Function, that is executed at the end of sh check"
  (when (not (string-match "finished" msg))
    (next-error 1 t)))

(define-compilation-mode sh-check-mode "SH"
  "Mode for check sh source code."
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-finish-functions)
       (list 'sh-check-finish-hook)))

(defun sh-check-syntax ()
  "Check syntax of current file"
  (interactive)
  (when (string-match "^\\(ba\\|z\\)sh" (symbol-name sh-shell))
    (save-some-buffers t)
    (compilation-start (concat (symbol-name sh-shell) " -n " (buffer-file-name))
                       'sh-check-mode)))

;;;; wb-utils.el

;;; Ediff

;; 启动 ediff 的时候嵌入到当前 frame，而不是缺省的弹出一个新的 frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; 启动 ediff 的时候垂直分割 windows，而不是缺省的垂直分割 windows
(setq ediff-split-window-function 'split-window-horizontally)
;; 结束 ediff 的时候恢复原来的 windows 布局
(add-hook 'ediff-load-hook
          (lambda ()
            (add-hook 'ediff-before-setup-hook
                      (lambda ()
                        (setq ediff-saved-window-configuration (current-window-configuration))))
            (let ((restore-window-configuration
                   (lambda ()
                     (set-window-configuration ediff-saved-window-configuration))))
              (add-hook 'ediff-quit-hook restore-window-configuration 'append)
              (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

;;; Calendar, Diary

;; Calendar
(setq view-diary-entries-initially t
      ;; 设置所在地的经度、纬度，这样在 Calendar 中按 S，就可以显示每天
      ;; 的日出、日落时间
      calendar-longitude +121.26
      calendar-latitude +31.12
      calendar-location-name "上海 - 徐家汇"
      ;; 当退出日历的时候把 frame 删除
      calendar-remove-frame-by-deleting t
      mark-diary-entries-in-calendar t      ; 标记有日记记录的日子
      calendar-week-start-day 1             ; 每周第一天是周一
      appt-issue-message nil
      mark-holidays-in-calendar t           ; 标记节假日
      view-calendar-holidays-initially nil  ; 不显示节日列表
      ;; 在 calendar 中按 p C 看中文的天干地支
      chinese-calendar-celestial-stem
      ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"]
      chinese-calendar-terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"]
      general-holidays
      '((holiday-fixed  1  1   "元旦")
        (holiday-chinese-new-year)
        (holiday-fixed  2 14   "情人节")
        (holiday-fixed  3  8   "妇女节")
        (holiday-fixed  3  12  "植树节")
        (holiday-fixed  4  1   "愚人节")
        (holiday-fixed  5  1   "劳动节")
        (holiday-fixed  5  4   "青年节")
        (holiday-float  5  0 2 "母亲节")
        (holiday-fixed  6  1   "儿童节")
        (holiday-float  6  0 3 "父亲节")
        (holiday-fixed  9  10  "教师节")
        (holiday-fixed  10 1   "国庆节")
        (holiday-fixed  12 25  "圣诞节"))
      ;; 农历节日
      local-holidays
      '((holiday-chinese 1 15 "元宵节 (正月十五)")
        (holiday-chinese 5  5 "端午节 (五月初五)")
        (holiday-chinese 9  9 "重阳节 (九月初九)")
        (holiday-chinese 8 15 "中秋节 (八月十五)"))
      ;; 是否显示这些类别的节日，t 显示, nil 不显示
      christian-holidays nil
      hebrew-holidays nil
      islamic-holidays nil
      solar-holidays nil
      bahai-holidays nil)

(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(autoload 'chinese-year "cal-china" "Chinese year data" t)

(defun holiday-chinese (cmonth cday string)
  "Chinese calendar holiday, month and day in Chinese calendar (CMONTH, CDAY).
If corresponding MONTH and DAY in gregorian calendar is visible,
the value returned is the list \(((MONTH DAY year) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let* ((m displayed-month)
         (y displayed-year)
         (gdate (calendar-gregorian-from-absolute
                 (+ (cadr (assoc cmonth (chinese-year y))) (1- cday))))
         (gm (car gdate))
         (gd (cadr gdate))
         (gy (caddr gdate)))
    (increment-calendar-month m y (- 11 gm))
    (if (> m 9)
        (list (list (list gm gd gy) string)))))

;; Diary
(setq diary-file "~/.emacs.d/.diary")
(setq view-diary-entries-initially t
    mark-diary-entries-in-calendar t
    number-of-diary-entries 7)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; Todo
(setq todo-file-do "~/.emacs.d/.todo-do"
      todo-file-done "~/.emacs.d/.todo-done"
      todo-file-top "~/.emacs.d/.todo-top")

;;; Gnus

(setq gnus-verbose 10)

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

(setq gnus-init-file "~/.emacs.d/elisp-init/wb-gnus.el")

;;; Org Mode

(robust-require org-install ; 下载 Org 后用 make 命令生成 org-install 文件
  ;;;;;;;;;;;;;;
  ;; 基本设置 ;;
  ;;;;;;;;;;;;;;

  ;; 设置使用 Org Mode 的文件后缀
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

  ;; 设置 mode hook
  (add-hook 'org-mode-hook
            (lambda ()
              ;; 使用 yasnippet
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (define-key yas/keymap [tab] 'yas/next-field-group)
              ;; 激活 flyspell mode 进行拼写检查
              (flyspell-mode 1)))

  ;; Org 文档内容都用 utf-8-unix 编码
  (add-to-list 'auto-coding-alist '("org\\'" . utf-8-unix))

  ;; 设置几个方便使用 Org 的全局键绑定和函数
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)

  (defun gtd ()
    (interactive)
    (find-file "~/.dropbox/GTD/gtd"))

  ;; 微调 Org 中的键绑定的行为
  (setq org-return-follows-link t)    ; 用 RET 而不是 C-c C-o 打开连接
  (setq org-special-ctrl-a/e t)
  (setq org-yank-adjusted-subtrees t)

  ;; 每小时保存一次所有的 org 文件
  (run-at-time "00:59" 3600 'org-save-all-org-buffers)

  ;;;;;;;;;;;;;;;
  ;; NOTE 设置 ;;
  ;;;;;;;;;;;;;;;

  ;; 调整一些内容的显示

  (setq org-hide-block-startup t)       ; 隐藏所有 block

  (defface org-date
    '((((class color) (background light)) (:foreground "#7f7f7f" :underline t))
      (((class color) (background dark)) (:foreground "#7f7f7f" :underline t))
      (t (:underline t)))
    "Face for org dates.")

  (defface org-embedded-code-face
    '((t (:foreground "grey40")))
    "Used in org-mode to indicate code block.")

  (font-lock-add-keywords
   'org-mode
   '(("#\\+BEGIN_SRC.*$" (0 'org-embedded-code-face t))
     ("#\\+END_SRC" (0 'org-embedded-code-face t)))
   t)

  ;; 利用 iimage 在 Org 文档中显示图片
  (with-library "iimage"
    (defun org-toggle-iimage-in-org ()
      "display images in your org file"
      (interactive)
      (if (face-underline-p 'org-link)
          (set-face-underline-p 'org-link nil)
        (set-face-underline-p 'org-link t))
      (iimage-mode)))

  ;; 微调 publish 时的行为
  (setq org-export-with-sub-superscripts nil) ; 缺省不把正文中的 ^、_ 作为上下标的标志
  (setq org-export-html-inline-images t)      ; 缺省图片都内嵌到文档中
  ;; org-export-htmlize-output-type

  (defun wb-org-remove-html-cjk-space ()
    "删除输出 HTML 时两行中文之间的空格。"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(\\cc\\)\n\\(\\cc\\)" nil t)
        (unless (get-text-property (match-beginning 0) 'read-only)
          (replace-match "\\1\\2")))
      ;; (goto-char (point-min))
      ;; (while (re-search-forward "\\([^\n]\\)\n\\(.\\)" nil t)
      ;;   (unless (get-text-property (match-beginning 0) 'read-only)
      ;;     (replace-match "\\1 \\2")))
      ))

  (add-hook 'org-export-preprocess-final-hook
            'wb-org-remove-html-cjk-space)

  ;; 定义 Org 文档项目
  (setq org-publish-project-alist
        '(("index"
           :base-directory "~/muse/source"
           :publishing-directory "~/public_html/"
           :base-extension "org"
           :section-numbers nil
           :table-of-contents nil
           :author-info nil
           :creator-info nil
           :style-include-default nil
           :auto-preamble nil
           :auto-postamble nil
           :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
           :preamble "<div class=\"container\">
                      <div class=\"gfx\"><span></span></div>
                      <div class=\"top\">
                        <div class=\"navigation\">
                          <a href=\"index.html\" id=\"selected\">Home</a>
                          <a href=\"spa/SPA.html\">S.P.A.</a>
                          <a href=\"reading/Reading.html\">Reading</a>
                          <a href=\"programming/Programming.html\">Programming</a>
                          <a href=\"iccad/ICCAD.html\">ICCAD</a>
                          <a href=\"computer/Computer.html\">Computer</a>
                          <a href=\"emacs/Emacs.html\">Emacs</a>
                        </div>
                        <div class=\"pattern\"><span></span></div>
                        <div class=\"header\">
                          <h1>The Power of Mind</h1>
                          <p>Observe, Think and Practise</p>
                        </div>
                        <div class=\"pattern\"><span></span></div>
                      </div>
                      <div class=\"content\">
                      <div class=\"spacer\"></div>"
           :postamble "  </div>
                       <div class=\"footer\">&copy; 2009 Bo Wang</div>
                     </div>"
           :publishing-function org-publish-org-to-html)
          ("emacs"
           :base-directory "~/muse/source/emacs"
           :publishing-directory "~/public_html/emacs"
           :base-extension "org"
           :author-info nil
           :creator-info nil
           :style-include-default nil
           :auto-preamble nil
           :auto-postamble nil
           :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\" />"
           :preamble "<div class=\"container\">
                      <div class=\"gfx\"><span></span></div>
                      <div class=\"top\">
                        <div class=\"navigation\">
                          <a href=\"../index.html\">Home</a>
                          <a href=\"spa/SPA.html\">S.P.A.</a>
                          <a href=\"reading/Reading.html\">Reading</a>
                          <a href=\"programming/Programming.html\">Programming</a>
                          <a href=\"iccad/ICCAD.html\">ICCAD</a>
                          <a href=\"computer/Computer.html\">Computer</a>
                          <a href=\"emacs/Emacs.html\" id=\"selected\">Emacs</a>
                        </div>
                        <div class=\"pattern\"><span></span></div>
                        <div class=\"header\">
                          <h1>The Power of Mind</h1>
                          <p>Observe, Think and Practise</p>
                        </div>
                        <div class=\"pattern\"><span></span></div>
                      </div>
                      <div class=\"content\">
                      <div class=\"spacer\"></div>"
           :postamble "  </div>
                       <div class=\"footer\">
                         <div class=\"left\">
                           &copy; 2009
                           <a href=\"index.html\">Website.com</a>.
                           Valid
                           <a href=\"http://jigsaw.w3.org/css-validator/check/referer\">CSS</a>
                           &amp;
                           <a href=\"http://validator.w3.org/check?uri=referer\">XHTML</a>
                         </div>
                         <div class=\"right\">
                           <a href=\"index.html\">Website</a>
                           by
                           <a href=\"../index.html\">Bo Wang</a>
                         </div>
                       </div>
                     </div>"
           :publishing-function org-publish-org-to-html)
          ("computer"
           :base-directory "~/muse/source/computer"
           :publishing-directory "~/public_html/computer"
           :base-extension "org"
           :publishing-function org-publish-org-to-html)
          ("iccad"
           :base-directory "~/muse/source/iccad"
           :publishing-directory "~/public_html/iccad"
           :base-extension "org"
           :publishing-function org-publish-org-to-html)
          ("programming"
           :base-directory "~/muse/source/programming"
           :publishing-directory "~/public_html/programming"
           :base-extension "org"
           :publishing-function org-publish-org-to-html)
          ("reading"
           :base-directory "~/muse/source/reading"
           :publishing-directory "~/public_html/reading"
           :base-extension "org"
           :publishing-function org-publish-org-to-html)
          ("spa"
           :base-directory "~/muse/source/spa"
           :publishing-directory "~/public_html/spa"
           :base-extension "org"
           :publishing-function org-publish-org-to-html)
          ("website" :components ("index" "emacs" "computer" "iccad"
                                  "programming" "reading" "spa"))))

  ;;;;;;;;;;;;;;;
  ;; TODO 设置 ;;
  ;;;;;;;;;;;;;;;

  ;; 集成 remember，并设置模板
  (robust-require remember
    (org-remember-insinuate)
    (setq org-directory "~/.dropbox/GTD/")
    (setq org-default-notes-file (concat org-directory "/gtd"))
    (setq org-remember-templates
          '(("Todo" ?t "* TODO %? %^g\n  %u" "gtd" "Inbox")
            ("Note" ?n "* %?\n  %T" "notes" top)))
    ;; 正向记录 note，新的在下面
    (setq org-reverse-note-order nil)
    ;; 设置一个全局键绑定快速调用 remember
    (global-set-key (kbd "C-M-r") 'remember))

  ;; 微调 Refile 操作
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
  (setq org-completion-use-ido t)       ; 使用 ido 方式的补全 (v6.13)
  (setq org-refile-use-outline-path t)  ; 使用多级的 path（设为 'file 则包括文件名）
  (setq org-outline-path-complete-in-steps t) ; 多级的 path 依次完成

  ;; 在 Agenda 中高亮当前行
  (add-hook 'org-agenda-mode-hook
            '(lambda ()
               (hl-line-mode 1)))

  ;; 设定 agenda 文件列表
  (setq org-agenda-files '("~/.dropbox/GTD/gtd"))

  ;; Agenda 中不显示某些继承的 tag
  (setq org-tags-exclude-from-inheritance '("PROJECT"))

  ;; 以类似设置 TAG 的界面设置 TODO KEYWORD
  (setq org-use-fast-todo-selection t)

  ;; 在 Agenda Overview 中不显示已完成的任务
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  ;; Agenda Overview 显示的天数
  (setq org-agenda-ndays 7)
  ;; Agenda Overview 从周几开始显示，缺省 1 表示周一，nil 表示当天
  (setq org-agenda-start-on-weekday nil)

  ;; 设置 TODO 关键字的 face
  (setq org-todo-keyword-faces
        '(("TODO"      . org-todo)
          ("ONGO"      . (:foreground "red" :weight bold))
          ("WAIT"      . (:foreground "grey80" :background "grey40"))
          ("DELE"      . (:foreground "grey40"))
          ("CANCELED"  . (:foreground "blue" :weight bold))))

  ;; 设置 TAG 的 face (v6.14)
  (setq org-tag-faces
        '(("PROJECT"    . org-level-2)))

  ;; 把任务的状态转换情况记录到 drawer 里，缺省为 LOGBOOK
  ;; 该变量同时设置 clock 记录位置（org-clock-into-drawer）
  (setq org-log-into-drawer t)

  ;; 调整 Clock（计时）行为
  (setq org-clock-persist t) ; 保存计时的内容，以及计时历史；启动 Emacs 时重新载入
  (setq org-clock-persistence-insinuate) ; Emacs 重启后继续计时
  (setq org-clock-in-resume t)           ; 计时时继续未完成的计时
  (setq org-clock-into-drawer t)         ; 计时信息记录到 drawer 里
  (setq org-clock-out-remove-zero-time-clocks t) ; 如果任务耗时为 0，删去计时内容

  ;; 在 Clock Report 中最多显示第 4 级的任务
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4))

  ;; 自定义 Agenda Custom View
  (setq org-agenda-custom-commands
        '(("c" . "Context Agenda View")
          ("co" agenda "Office Agenda"
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'notregexp "OFFICE")))
            (org-agenda-ndays 1)
            (org-agenda-overriding-header "Today's Office tasks: ")))
          ("cp" agenda "Office+Computer Agenda"
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'notregexp "OFFICE\\|COMPUTER")))
            (org-agenda-ndays 1)
            (org-agenda-overriding-header "Today's Office+Computer tasks: ")))
          ("ch" agenda "Home Agenda"
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'notregexp "HOME")))
            (org-agenda-ndays 1)
            (org-agenda-overriding-header "Today's Office tasks: ")))
          ("ci" agenda "Home+Computer Agenda"
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'notregexp "HOME\\|COMPUTER")))
            (org-agenda-ndays 1)
            (org-agenda-overriding-header "Today's Home+Computer tasks: ")))
          ("ce" agenda "Errand Agenda"
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'notregexp "ERRAND")))
            (org-agenda-ndays 1)
            (org-agenda-overriding-header "Today's Errand tasks: ")))
          ("u" alltodo ""
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'scheduled 'deadline
                                         'regexp "<[^>\n]+>")))
            (org-agenda-overriding-header "Unscheduled TODO entries")))
          ("x" . "Checklist Exporters")
          ("xa" agenda "Agenda Checklist"
           ((org-agenda-prefix-format " [ ] ")
            (org-agenda-with-colors nil)
            (org-agenda-remove-tags t)))
          ("xc" "Context Checklist"
           ((tags "PROJECT"
                  ((org-agenda-overriding-header "PROJECT:")))
            (tags "ANYWHERE"
                  ((org-agenda-overriding-header "ANYWHERE:")))
            (tags "OFFICE"
                  ((org-agenda-overriding-header "OFFICE:")))
            (tags "COMPUTER"
                  ((org-agenda-overriding-header "COMPUTER:")))
            (tags "HOME"
                  ((org-agenda-overriding-header "HOME:")))
            (tags "ERRAND"
                  ((org-agenda-overriding-header "ERRAND:"))))
           ((org-use-tag-inheritance nil)
            (org-agenda-prefix-format " [ ] ")
            (org-agenda-with-colors nil)
            (org-agenda-remove-tags t)
            (org-agenda-add-entry-text-maxlines 5)
            (org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'regexp "DONE"))))))))

;;; Anything
(robust-require anything-config
  (setq anything-sources
        (list anything-c-source-buffers
              anything-c-source-file-name-history
              anything-c-source-file-cache
              ;; anything-c-source-emacs-commands
              ;; anything-c-source-info-pages
              ;; anything-c-source-man-pages
              ))
  (setq anything-c-adaptive-history-file
        "~/.emacs.d/.anything-c-adaptive-history")
  (global-set-key (kbd "ESC ESC SPC") 'anything))

;;;; wb-tools.el

;;; Tramp

(setq tramp-persistency-file-name "~/.emacs.d/.tramp")

;;; etags

;; 让 etags 在当前目录和上四级目录中搜索 TAGS 文件
(setq tags-table-list '("." ".." "../.." "../../.." "../../../.."))

;; 一些生成 TAGS 的命令
;; 可以考虑使用 http://www.sixfingeredman.net/proj/xemacs/build-tags.el
(defun create-c-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; 如果同一个 tag 有不同结果，etags-select 能显示出列表
(robust-require etags-select
  (global-set-key "\M-?" 'etags-select-find-tag-at-point))

;; imenu-tree, tags-tree
(autoload 'imenu-tree "imenu-tree" "Imenu tree" t)
(autoload 'tags-tree "tags-tree" "TAGS tree" t)
(eval-after-load "tree-widget"
  '(if (boundp 'tree-widget-themes-load-path)
       (add-to-list 'tree-widget-themes-load-path
                    "~/.emacs.d/Extensions/imenu-tags-tree/tree-widget/imenu")))
(add-hook 'tree-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;;; Shell

;; shell buffer 的标题加上当前路径，这样可以开多个 shell buffer
(defun wb-shell-mode-auto-rename-buffer (text)
  (if (eq major-mode 'shell-mode)
      (rename-buffer (concat "*shell: " default-directory "*") t)))

(add-hook 'comint-output-filter-functions
          'wb-shell-mode-auto-rename-buffer)

(defun wb-shell-mode-kill-buffer-on-exit (process state)
  ;; shell 退出时记录命令历史
  (shell-write-history-on-exit process state)
  ;; shell 退出时删除 shell buffer
  (kill-buffer (process-buffer process)))

(defun wb-shell-mode-hook-func ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'wb-shell-mode-kill-buffer-on-exit))

(defun wb-shell-mode-hook nil
  (wb-shell-mode-hook-func)
  ;; 打开 ansi-color
  (ansi-color-for-comint-mode-on)
  ;; 启用 abbrev
  (abbrev-mode t))

(add-hook 'shell-mode-hook 'wb-shell-mode-hook)

;;; Spell Checking

;; 新版的 Emacs 已经缺省使用 aspell 了
; (setq-default ispell-program-name "/usr/bin/aspell")
(setq ispell-personal-dictionary "~/.emacs.d/.ispell_personal")
(setq ispell-silently-savep t)

;; ignore all-uppercase words
(defun flyspell-ignore-uppercase (beg end &rest rest)
  (while (and (< beg end)
              (let ((c (char-after beg)))
                (not (= c (downcase c)))))
    (setq beg (1+ beg)))
  (= beg end))
(add-hook 'flyspell-incorrect-hook 'flyspell-ignore-uppercase)

; (add-hook 'font-lock-mode-hook 'flyspell-prog-mode)

;;; w3m

(with-library "w3m"
  (require 'w3m-load)
  ;; 使用 w3m 作为默认的浏览器
  (setq browse-url-browser-function 'w3m-browse-url)
  (global-set-key (kbd "C-c b") 'w3m-browse-url)
  (setq w3m-fill-column 80)
  (setq w3m-key-binding 'info)
  ;; 用 S+RET 打开新链接时不直接跳到新页面，可以用 C-c C-n、C-c C-p 跳转
  (setq w3m-view-this-url-new-session-in-background t)

  (setq w3m-command-arguments-alist
        '( ;; 本地连接不需要代理
          ("^http://localhost" "-no-proxy")
          ("^http://127\\.0\\.0\\.1" "-no-proxy"))))

;;; Git

(robust-require vc-git
  (require 'git)
  (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
  (defalias 'gs 'git-status)
  (autoload 'git-blame-mode "git-blame"
    "Minor mode for incremental blame for Git." t))

;;; Dict
(require 'sdcv-mode)
(global-set-key (kbd "C-c d") 'sdcv-search)

;;;; wb-kbd.el

;;; 终端下 Keyboard 设置

;; 在 PuTTY 设置 Keyboard 为 SCO 可以识别 C/S-f2
(add-hook 'term-setup-hook
          (lambda ()
            ;; F1 ~ F12
            (define-key function-key-map (kbd "\e[N")   [f2])
            (define-key function-key-map (kbd "\e[l")   [C-f2])
            (define-key function-key-map (kbd "\e[Z")   [S-f2])
            (define-key function-key-map (kbd "\e\e[N") [M-f2])
            (define-key function-key-map (kbd "\e[Q")   [f5])
            (define-key function-key-map (kbd "\e[o")   [C-f5])
            (define-key function-key-map (kbd "\e[c")   [S-f5])
            (define-key function-key-map (kbd "\e\e[Q") [M-f5])
            (define-key function-key-map (kbd "\e[S")   [f7])
            (define-key function-key-map (kbd "\e[q")   [C-f7])
            (define-key function-key-map (kbd "\e[e")   [S-f7])
            (define-key function-key-map (kbd "\e\e[q") [M-f7])
            (define-key function-key-map (kbd "\e[V")   [f10])
            (define-key function-key-map (kbd "\e[t")   [C-f10])
            (define-key function-key-map (kbd "\e[h")   [S-f10])
            (define-key function-key-map (kbd "\e\eV")  [M-f10])
            (define-key function-key-map (kbd "\e[W")   [f11])
            (define-key function-key-map (kbd "\e[u")   [C-f11])
            (define-key function-key-map (kbd "\e[i")   [S-f11])
            (define-key function-key-map (kbd "\e\e[W") [M-f11])
            ;; PageUp 和 PageDown
            (define-key function-key-map (kbd "\e[I")   [prior])
            (define-key function-key-map (kbd "\e[G")   [next])))

;;; 全局键绑定

(defalias 'sbke 'save-buffers-kill-emacs)

;; C-x 3 缺省用 follow-mode 显示当前 buffer
;; (global-set-key (kbd "C-x 3") 'follow-delete-other-windows-and-split)

;; C-x n/p 实现向后/向前的 C-x o
(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-set-key (kbd "C-x n") 'next-multiframe-window)

;; 用 C-2 设置 mark，和 C-@ 键位相同，但不用按 Shift
(global-set-key [?\C-2] 'set-mark-command)

;; 相应的修改 pop-global-mark，使用 C-x C-2
(global-set-key (kbd "C-x C-2") 'pop-global-mark)

;; 下面的 C-x LETTER 都没有定义。在缺省情况下，Emacs 会自动转为对应的
;; C-x letter 版本，可以自定义一些自己喜欢的命令
(global-set-key "\C-xB" 'bury-buffer)
(global-set-key "\C-xE" 'apply-macro-to-region-lines)
(global-set-key "\C-xI" 'insert-buffer)

;; goto-line 默认绑定了两个按键 M-g g 和 M-g M-g，将其中一个绑定为按百
;; 分率跳转到某一行
(global-set-key (kbd "M-g g") 'wb-goto-line)

;; M-o 替换 C-x o，用于切换 window
(global-set-key (kbd "M-o") 'other-window)

;; C-c i 前缀用于插入一些文本

;; 定义 wb-insert-map 并绑定为 C-c i
(defvar wb-insert-map nil)
(setq wb-insert-map (make-sparse-keymap))
(global-set-key "\C-ci" wb-insert-map)

;; 绑定一些插入文本的函数
(global-set-key "\C-cid" 'wb-insert-date)
(global-set-key "\C-cit" 'wb-insert-time)

;;; 局部键绑定

(add-hook 'diff-mode-hook
          '(lambda ()
             (define-key diff-mode-shared-map "q" 'wb-quit-buffer)))
(define-key occur-mode-map "q" 'wb-quit-buffer)
(eval-after-load "grep"
  '(progn
     (define-key grep-mode-map  "q" 'wb-quit-buffer)))

;; 为 view-mode 加入 vim 的按键。
(setq view-mode-hook
      (lambda ()
        (define-key view-mode-map "h" 'backward-char)
        (define-key view-mode-map "l" 'forward-char)
        (define-key view-mode-map "j" 'next-line)
        (define-key view-mode-map "k" 'previous-line)))

;;;; .emacs tail

;; 最大化 Frame
(when *win32p*
  (wb-maximize-frame))

;; desktop 保存打开的文件列表
(robust-require desktop
  ;; 启用 desktop save mode
  (desktop-save-mode t)
  ;; 设置 desktop 相关文件的路径
  (setq desktop-base-file-name "~/.emacs.d/.emacs.desktop")
  (setq desktop-base-lock-name "~/.emacs.d/.emacs.desktop.lock")
  ;; 始终保存 desktop 文件，不管文件原来是否存在，也不询问用户
  (setq desktop-save t)
  ;; 不管 desktop 文件是否被 lock，都加载 desktop 文件
  (setq desktop-load-locked-desktop t))

;; session 是用来保存一些全局变量
(robust-require session
  (setq session-save-file (expand-file-name "~/.emacs.d/.session"))
  (setq session-save-file-coding-system 'utf-8-unix)
  ;; org-mark-ring 是一个循环结构。如果不除掉，使用 org-mode 后就关不了
  ;; emacs 了
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (add-hook 'after-init-hook 'session-initialize))

;; Local Variables:
;; coding: utf-8-unix
;; mode: outline-minor
;; outline-regexp: ";;;\\(;* [^     \n]\\|###autoload\\)"
;; End:
