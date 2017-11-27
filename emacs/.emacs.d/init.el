;; avoid the symlink-related prompt
(let ((vc-follow-symlinks t))
  (org-babel-load-file "~/.emacs.d/README.org"))
