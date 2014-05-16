;; -*- coding: utf-8-unix -*-

;; ELPA 包：设置 ELPA 并加载所有的包
(require 'package)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(setq package-user-dir (locate-user-emacs-file ".elpa"))

(setq package-enable-at-startup nil)
(package-initialize)

;; 需要通过 ELPA 安装的包
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 '(anything auctex auto-complete color-theme ido-vertical-mode magit markdown-mode
            org projectile smex undo-tree yasnippet))

;; 本地包：把 elisp-3rdparty 及其下面的所有子目录加到 load-path
;; 忽略 .git、.svn、RCS、CVS 等目录，以及包含 .nosearch 文件的目录
(setq wb-3rd-lisp-dir "~/.emacs.d/elisp-3rdparty/")
(if (and (fboundp 'normal-top-level-add-subdirs-to-load-path)
         (file-exists-p wb-3rd-lisp-dir))
    (let* ((default-directory wb-3rd-lisp-dir)
           (orig-load-path load-path))
      (setq load-path (list default-directory))
      (normal-top-level-add-subdirs-to-load-path)
      (setq load-path (append load-path orig-load-path))))

;; 加载 dotemacs 初始化文件
;; (let ((inner "wb-emacs-init"))
;;   (if init-file-debug
;;       (load inner)
;;     (condition-case error
;;         (load inner t t)
;;       (error
;;        (deh-display-dot-emacs-error error)))))

;; 通过 Org Babel 加载初始化文件
(org-babel-load-file (expand-file-name "~/.emacs.d/initialize.org"))
