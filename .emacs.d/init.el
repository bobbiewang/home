;; 把 elisp-3rdparty 及其下面的所有子目录加到 load-path
;; 忽略 .git、.svn、RCS、CVS 等目录，以及包含 .nosearch 文件的目录
(setq wb-3rd-lisp-dir "~/.emacs.d/elisp-3rdparty/")
(if (and (fboundp 'normal-top-level-add-subdirs-to-load-path)
         (file-exists-p wb-3rd-lisp-dir))
    (let* ((default-directory wb-3rd-lisp-dir)
           (orig-load-path load-path))
      (setq load-path (list default-directory))
      (normal-top-level-add-subdirs-to-load-path)
      (setq load-path (append load-path orig-load-path))))

;; 将 elisp-init 和 elisp-personal 加到 load-path
(setq load-path
  (cons "~/.emacs.d/elisp-init"
    (cons "~/.emacs.d/elisp-personal" load-path)))

;; 加载包管理系统
(require 'package)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(setq package-user-dir (locate-user-emacs-file ".elpa"))

(setq package-enable-at-startup nil)
(package-initialize)

;; 加载 dotemacs 辅助文件
(load "wb-de-helper.el")

;; 加载 dotemacs 初始化文件
(let ((inner "wb-emacs-init"))
  (if init-file-debug
      (load inner)
    (condition-case error
        (load inner t t)
      (error
       (deh-display-dot-emacs-error error)))))

(deh-initialization-time "Emacs has been initialized")
(deh-initialization-stat)
