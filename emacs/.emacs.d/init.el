;; avoid the symlink-related prompt
(custom-set-variables
 '(vc-follow-symlinks t))

;; load the literate init file
(org-babel-load-file "~/.emacs.d/README.org")
