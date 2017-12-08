(custom-set-variables
 ;; call the garbage collector less often
 '(gc-cons-threshold (* 32 (expt 2 20))) ; 32 MB
 ;; avoid the symlink-related prompt
 '(vc-follow-symlinks t))

;; load the literate init file
(org-babel-load-file "~/.emacs.d/README.org")
