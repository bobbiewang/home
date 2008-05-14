(setq load-path
  (cons "~/.emacs.d/elisp-init"
    (cons "~/.emacs.d/elisp-personal" load-path)))

(load "wb-de-helper.el")

(let ((inner "wb-emacs-init"))
  (if init-file-debug
      (load inner)
    (condition-case error
        (load inner)
      (error
       (deh-display-dot-emacs-error error)))))

(deh-current-load-time "Emacs has been initialized")
