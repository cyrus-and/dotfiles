(custom-set-variables
 ;; call the garbage collector less often (see `gcs-done')
 '(gc-cons-threshold (* 32 (expt 2 20))) ; 32 MB
 ;; avoid the symlink-related prompt
 '(vc-follow-symlinks t))

;; tangle and load the literate init file
(require 'ob-tangle)
(let ((input "~/.emacs.d/README.org")
      (output "~/.emacs.d/my-init.el"))
  (when (file-newer-than-file-p input output)
    (org-babel-tangle-file input output))
  (load-file output))
