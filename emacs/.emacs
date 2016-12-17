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
 '(flatland-black-theme
   markdown-mode
   php-mode
   go-mode
   json-mode
   yaml-mode
   exec-path-from-shell
   magit
   auctex))

;;;;;;;;;;;
;; THEME ;;
;;;;;;;;;;;

(load-theme 'flatland-black t)

;; custom theme tuning
(custom-set-faces
 '(default ((t (:foreground "#EEEEEE"))))
 '(mode-line ((t (:background "#323232"))))
 '(mode-line-highlight ((t (:box nil :inverse-video t))))
 '(lazy-highlight ((t (:foreground "#000000"))))
 '(isearch ((t (:foreground "#000000" :background "#1278A8"))))
 '(region ((t (:foreground "#77BBDD" :background "#1278A8"))))
 '(fringe ((t (:foreground "#323232" :background "#000000"))))
 '(vertical-border ((t (:foreground "#323232"))))
 '(linum ((t (:background "#000000" :inherit (default)))))
 '(font-lock-variable-name-face ((t (:foreground "#B8D977")))))

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
 ;; file management
 '(vc-follow-symlinks t)

 ;; editing
 '(ispell-dictionary "english")
 '(fill-column 80)
 '(truncate-lines t)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(require-final-newline 'ask)

 ;; dired listing options
 '(dired-listing-switches
   (if (eq system-type 'darwin)
       "-al" "-al --group-directories-first"))

 ;; UI related options
 '(disabled-command-function nil)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(blink-cursor-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-wheel-progressive-speed nil)
 '(mac-right-option-modifier 'none)
 '(ring-bell-function 'ignore)

 ;; line number mode
 '(linum-format (if (display-graphic-p)
                    " %d" "%d\u2502"))

 ;; minibuffer history
 '(savehist-mode t)
 '(history-length t)

 ;; code indentation (never use literal tab)
 '(c-backslash-column 79)
 '(c-backslash-max-column 79)
 '(indent-tabs-mode nil)
 '(c-basic-offset 4)
 '(c-offsets-alist
   '((substatement-open . 0)
     (brace-list-open . 0)
     (topmost-intro-cont . 0)
     (arglist-intro . +)
     (arglist-close . 0)
     (cpp-macro . 0)
     (innamespace . 0)))

 ;; backup
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs_backups")))

 ;; add some common safe file/dir variables to avoid prompt
 '(safe-local-variable-values
   (quote
    ((ispell-silently-savep . t) ; auto-save personal dictionatu
     (eval setq ispell-personal-dictionary
           (concat default-directory ".dictionary"))))) ; define personal dictionary

 ;; no startup screen, use scratch
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   (concat
    ";; Cheatsheet:\n"
    ";;\n"
    ";; C-x C-q    wdired-change-to-wdired-mode\n"
    ";; C-x d      dired (used to filter, e.g. C-x d *.c)\n"
    ";; M-S-/      dabbrev-expand\n"
    ";; C-x C-b    list-buffers\n"
    ";; M-z        zap-to-char\n"
    ";; C-s C-w    'isearch next word'\n"
    ";; F5         grep (*)\n"
    ";; F6         shell (*)\n"
    ";; F7         compile (*)\n"
    ";; C-F7       recompile (*)\n"
    ";; F8         gdb (*)\n"
    ";; F9         package-list-packages (*)\n"
    ";; M-s h r    highlight-regexp\n"
    ";; C-x w r    unhighlight-regexp\n"
    ";; % m        dired-mark-files-regexp\n"
    ";; M-^        delete-indentation\n"
    ";; M-|        shell-command-on-region\n"
    ";; C-x -      shrink-window-if-larger-than-buffer\n"
    ";; C-x z      repeat\n"
    ";; M-_        toggle-comment-current-line (*)\n"
    ";; C-x n n    narrow-to-region\n"
    ";; C-x n w    widen\n"
    ";; M-h        mark-paragraph\n"
    ";; C-x C-SPC  pop-global-mark\n"
    ";; C-x r SPC  point-to-register\n"
    ";; C-x r j    jump-to-register\n"
    ";; C-x SPC    rectangle-mark-mode\n"
    ";;\n"
    ";; (*) custom\n")))

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
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f8>") 'gdb)
(global-set-key (kbd "<f9>") 'package-list-packages)

(global-set-key (kbd "C-<f5>") 'reload-buffer)
(global-set-key (kbd "C-<f7>") 'recompile)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-_") 'toggle-comment-current-line)

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
