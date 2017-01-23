;;;;;;;;;;
;; TODO ;;
;;;;;;;;;;

;; find a way to disable line truncation in the minibuffer ispell help

;; linum causes vertical-border flickering

;;;;;;;;;;;;;;;;;;;;
;; PACKAGES SETUP ;;
;;;;;;;;;;;;;;;;;;;;

;; initialize package system
(package-initialize)

;; add additional repositories
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; refresh the package list the first time
(unless package-archive-contents
  (package-refresh-contents))

;; install custom packages the first time
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 ;; packages...
 '(
   auctex
   cmake-mode
   exec-path-from-shell
   flatland-black-theme
   go-mode
   json-mode
   magit
   markdown-mode
   php-mode
   protobuf-mode
   yaml-mode
   ))

;;;;;;;;;;;
;; THEME ;;
;;;;;;;;;;;

(load-theme 'flatland-black t)

;; custom theme tuning
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#EEEEEE"))))
 '(font-lock-variable-name-face ((t (:foreground "#B8D977"))))
 '(fringe ((t (:foreground "#323232" :background "#000000"))))
 '(isearch ((t (:foreground "#000000" :background "#1278A8"))))
 '(lazy-highlight ((t (:foreground "#000000"))))
 '(linum ((t (:background "#000000" :inherit (default)))))
 '(mode-line ((t (:background "#323232"))))
 '(mode-line-highlight ((t (:box nil :inverse-video t))))
 '(region ((t (:foreground "#77BBDD" :background "#1278A8"))))
 '(vertical-border ((t (:foreground "#323232")))))

;; fix pure black background independently of what *black* is for the terminal
(if (not (display-graphic-p))
    (custom-set-faces
     '(default ((t (:background "color-16"))))
     '(lazy-highlight ((t (:foreground "color-16"))))
     '(isearch ((t (:foreground "color-16"))))
     '(fringe ((t (:background "color-16"))))
     '(linum ((t (:background "color-16"))))))

;; fix font sizeo on macOS
(if (eq system-type 'darwin)
    (custom-set-faces
     '(default ((t (:height 140))))))

;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATIONS ;;
;;;;;;;;;;;;;;;;;;;;

;; fix PATH on macOS
(exec-path-from-shell-initialize)

;; use custom window separator character in nox mode
(set-display-table-slot standard-display-table 'vertical-border #x2502)

;; enable S-arrows window focus switch
(windmove-default-keybindings)

;; avoid `node_modules` folder in searches
(eval-after-load "grep"
  '(add-to-list 'grep-find-ignored-directories "node_modules"))

;; variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs_backups"))))
 '(blink-cursor-mode nil)
 '(c-backslash-column 79)
 '(c-backslash-max-column 79)
 '(c-basic-offset 4)
 '(c-offsets-alist
   (quote
    ((substatement-open . 0)
     (brace-list-open . 0)
     (topmost-intro-cont . 0)
     (arglist-intro . +)
     (arglist-close . 0)
     (cpp-macro . 0)
     (innamespace . 0))))
 '(column-number-mode t)
 '(dired-listing-switches
   (if
       (eq system-type
           (quote darwin))
       "-al" "-al --group-directories-first"))
 '(disabled-command-function nil t)
 '(fill-column 80)
 '(history-length t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   (concat ";; Cheatsheet:
" ";;
" ";; C-x C-q    wdired-change-to-wdired-mode
" ";; C-x d      dired (used to filter, e.g. C-x d *.c)
" ";; M-S-/      dabbrev-expand
" ";; C-x C-b    list-buffers
" ";; M-z        zap-to-char
" ";; C-s C-w    'isearch next word'
" ";; F5         grep (*)
" ";; F6         shell (*)
" ";; F7         compile (*)
" ";; C-F7       recompile (*)
" ";; F8         gdb (*)
" ";; F9         package-list-packages (*)
" ";; M-s h r    highlight-regexp
" ";; C-x w r    unhighlight-regexp
" ";; % m        dired-mark-files-regexp
" ";; M-^        delete-indentation
" ";; M-|        shell-command-on-region
" ";; C-x -      shrink-window-if-larger-than-buffer
" ";; C-x z      repeat
" ";; M-_        toggle-comment-current-line (*)
" ";; C-x n n    narrow-to-region
" ";; C-x n w    widen
" ";; M-h        mark-paragraph
" ";; C-x C-SPC  pop-global-mark
" ";; C-x r SPC  point-to-register
" ";; C-x r j    jump-to-register
" ";; C-x SPC    rectangle-mark-mode
" ";;
" ";; (*) custom
"))
 '(ispell-dictionary "english")
 '(linum-format (if (display-graphic-p) " %d" "%d│"))
 '(mac-right-option-modifier (quote none))
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5))))
 '(package-selected-packages
   (quote
    (protobuf-mode cmake-mode yaml-mode solarized-theme rainbow-mode popup php-mode markdown-mode magit json-mode js2-mode go-mode flatland-black-theme exec-path-from-shell base16-theme auctex)))
 '(require-final-newline (quote ask))
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((ispell-silently-savep . t)
     (eval setq ispell-personal-dictionary
           (concat default-directory ".dictionary")))))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(vc-follow-symlinks t))

;;;;;;;;;;;
;; HOOKS ;;
;;;;;;;;;;;

;; linum mode for file-backed buffers only
(add-hook 'find-file-hook 'linum-mode)

;; delete trailing spaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; visual line mode for compilation
(add-hook 'compilation-mode-hook
          (lambda () (visual-line-mode 1)))

;; useful highlightings
(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-regexp "TODO\\|XXX" (quote hi-red-b))))

;; flyspell the whole buffer on entering the mode
(add-hook 'flyspell-mode-hook 'flyspell-buffer)

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

;; avoid suspend-frame (use C-x C-z instead)
(global-unset-key (kbd "C-z"))

(global-set-key (kbd "<f5>") 'rgrep)
(global-set-key (kbd "<f6>") 'eshell)
(global-set-key (kbd "<f7>") 'recompile)
(global-set-key (kbd "<f8>") 'gdb)
(global-set-key (kbd "<f9>") 'package-list-packages)

(global-set-key (kbd "C-<f5>") 'reload-buffer)
(global-set-key (kbd "C-<f7>") 'compile)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-_") 'toggle-comment-current-line)

(global-set-key (kbd "C-c o") 'ff-find-other-file)

;;;;;;;;;;;
;; MAGIT ;;
;;;;;;;;;;;

;; shortcut to main magit buffer
(global-set-key (kbd "<f12>") 'magit-status)

;; use magit for `git commit`
(global-git-commit-mode)

;;;;;;;;;;;;
;; OTHERS ;;
;;;;;;;;;;;;

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
