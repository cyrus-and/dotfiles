;; -*- emacs-lisp -*-
;;
;; Cheatsheet:
;;
;; C-x C-q    wdired-change-to-wdired-mode
;; C-x d      dired (used to filter, e.g. C-x d *.c)
;; M-S-/      dabbrev-expand
;; C-x C-b    list-buffers
;; C-x 5 2    make-frame-command
;; M-z        zap-to-char
;; C-s C-w    "isearch next word"
;; F5         grep
;; F6         shell
;; F7         compile
;; F8         gdb
;; F9         package-list-packages
;; C-mouse-3  "show main menu"
;; M-s h r    highlight-regexp
;; C-x w r    unhighlight-regexp
;; % m        dired-mark-files-regexp
;; M-^        delete-indentation
;; M-|        shell-command-on-region
;; C-x -      shrink-window-if-larger-than-buffer
;; C-x z      repeat
;; M-_        toggle-comment-current-line (*)

;; (*) custom

;;;;;;;;;;;;;;;;;;;;
;; PACKAGES SETUP ;;
;;;;;;;;;;;;;;;;;;;;

;; initialize package system
(package-initialize)

;; add additional repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

;; refresh the package list the first time
(unless package-archive-contents
  (package-refresh-contents))

;; install custom packages the first time
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 ;; packages...
 '(zenburn-theme
   go-mode
   multi-web-mode
   markdown-mode
   json-mode
   auctex))

;;;;;;;;;;;;;;;;;;;;;
;; THEME (zenburn) ;;
;;;;;;;;;;;;;;;;;;;;;

(load-theme 'zenburn t)

;; custom theme tuning
(custom-set-faces
 '(menu ((t (:foreground "black" :background "grey75"))))
 '(mode-line ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil))))
 '(mode-line-highlight ((t (:box nil :inverse-video t))))
 '(lazy-highlight ((t (:foreground "black" :background "yellow" :weight normal))))
 '(isearch ((t (:foreground "black" :background "yellow"))))
 '(vertical-border ((t (:foreground "#4F4F4F"))))
 '(scroll-bar ((t (:foreground "#9FC59F" :background "#3F3F3F")))))

;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATIONS ;;
;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; no startup screen, use scratch
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")

 ;; window management ("*" buffers appear in the same window but "*compilation*"
 ;; is possibly reused)
 '(pop-up-windows nil)
 '(same-window-regexps '("*"))
 '(display-buffer-alist '(("\\*compilation\\*" .
                           (display-buffer-reuse-window . ((reusable-frames . t))))))

 ;; custom scroll mode
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-wheel-progressive-speed nil)

 ;; editing related options
 '(fill-column 80)

 ;; editing helpers
 '(truncate-lines t)
 '(column-number-mode t)
 '(indicate-buffer-boundaries 'left)
 '(show-paren-mode t)
 '(blink-cursor-mode nil)
 '(indicate-empty-lines t)

 ;; org-mode
 '(org-startup-indented t)
 '(org-startup-folded nil)

 ;; UI related options
 '(mouse-autoselect-window t)
 '(disabled-command-function nil)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tab-width 4)

 ;; custom frame title
 '(frame-title-format "%b")
 '(icon-title-format "%b")

 ;; line number mode
 '(global-linum-mode t)
 '(linum-format "%03d ")

 ;; minibuffer history
 '(savehist-mode t)
 '(history-length t)

 ;; code indentation (never use literal tab)
 '(indent-tabs-mode nil)
 '(c-basic-offset 4)
 '(c-offsets-alist
   '((substatement-open . 0)
     (brace-list-open . 0)
     (topmost-intro-cont . 0)))

 ;; backup
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs_backups"))))

;;;;;;;;;;;
;; HOOKS ;;
;;;;;;;;;;;

;; delete trailing spaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; visual line mode for compilation
(add-hook 'compilation-mode-hook
          (lambda () (visual-line-mode 1)))

;; visual line mode for org files
(add-hook 'org-mode-hook
          (lambda () (visual-line-mode 1)))

;;;;;;;;;;;;;;;
;; FUNCTIONS ;;
;;;;;;;;;;;;;;;

(defun reload-buffer ()
  (interactive)
  (revert-buffer t t))

(defun toggle-comment-current-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position))
  (next-line))

;;;;;;;;;;;;;;;
;; SHORTCUTS ;;
;;;;;;;;;;;;;;;

(global-set-key (kbd "C-<f5>") 'reload-buffer)
(global-set-key (kbd "C-<f6>") 'astyle-buffer)

(global-set-key (kbd "<f5>") 'rgrep)
(global-set-key (kbd "<f6>") 'eshell)
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f8>") 'gdb)
(global-set-key (kbd "<f9>") 'package-list-packages)

(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)

(global-set-key (kbd "C-M-<up>") 'enlarge-window)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)

(global-set-key (kbd "M-<tab>") 'list-buffers)

(global-set-key (kbd "M-_") 'toggle-comment-current-line)

;;;;;;;;;;;;;;;;;
;; THIRD PARTY ;;
;;;;;;;;;;;;;;;;;

;; multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\|<\\? \|<\\?=" "\\?>")
                  (js-mode "<script[^>]*>" "</script>")
                  (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "html"))
(multi-web-global-mode 1)
