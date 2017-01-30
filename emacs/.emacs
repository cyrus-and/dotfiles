;;;;;;;;;;;;;:;;
;; CHEATSHEET ;;
;;;;;;;;;;;;;;;;

(defconst cheatsheet
  '("% m        dired-mark-files-regexp"
    "C-s C-w    isearch-yank-word-or-char"
    "C-x -      shrink-window-if-larger-than-buffer"
    "C-x C-;    comment-line"
    "C-x C-SPC  pop-global-mark"
    "C-x C-b    list-buffers"
    "C-x C-q    wdired-change-to-wdired-mode"
    "C-x M-:    repeat-complex-command"
    "C-x SPC    rectangle-mark-mode"
    "C-x d      dired"
    "C-x n n    narrow-to-region"
    "C-x n w    widen"
    "C-x r SPC  point-to-register"
    "C-x r j    jump-to-register"
    "C-x w r    unhighlight-regexp"
    "C-x z      repeat"
    "M-S-/      dabbrev-expand"
    "M-^        delete-indentation"
    "M-h        mark-paragraph"
    "M-s h .    highlight-symbol-at-point"
    "M-s h r    highlight-regexp"
    "M-z        zap-to-char"
    "M-|        shell-command-on-region (replace with C-u)"
    ""
    "C-F5       reload-buffer"
    "F5         rgrep"
    "F6         shell"
    "F7         recompile-or-compile"
    "F8         package-list-packages"
    "F12        magit-status"))

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
 'package-install
 '(auctex
   cmake-mode
   exec-path-from-shell
   flatland-black-theme
   go-mode
   js2-mode
   json-mode
   magit
   markdown-mode
   php-mode
   protobuf-mode
   rainbow-mode
   yaml-mode))

;;;;;;;;;;;
;; THEME ;;
;;;;;;;;;;;

(load-theme 'flatland-black t)

;; custom theme tuning
(custom-set-faces
 '(default                      ((t (:foreground "#EEEEEE"))))
 '(mode-line                    ((t (:background "#323232"))))
 '(mode-line-highlight          ((t (:box nil :inverse-video t))))
 '(lazy-highlight               ((t (:foreground "#000000"))))
 '(isearch                      ((t (:foreground "#000000" :background "#1278A8"))))
 '(isearch-fail                 ((t (:background nil :inherit (hi-red-b)))))
 '(region                       ((t (:foreground "#77BBDD" :background "#1278A8"))))
 '(fringe                       ((t (:foreground "#323232" :background "#000000"))))
 '(vertical-border              ((t (:foreground "#323232"))))
 '(linum                        ((t (:background "#000000" :inherit (default)))))
 '(font-lock-variable-name-face ((t (:foreground "#B8D977")))))

;; fix pure black background independently of what *black* is for the terminal
(if (not (display-graphic-p))
    (custom-set-faces
     '(default        ((t (:background "color-16"))))
     '(lazy-highlight ((t (:foreground "color-16"))))
     '(isearch        ((t (:foreground "color-16"))))
     '(fringe         ((t (:background "color-16"))))
     '(linum          ((t (:background "color-16"))))))

;; fix font size on macOS retina
(if (eq system-type 'darwin)
    (custom-set-faces
     '(default ((t (:height 140))))))

;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATIONS ;;
;;;;;;;;;;;;;;;;;;;;

;; proper linum format
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((lines (count-lines (point-min) (point-max)))
         (width (length (number-to-string lines)))
         (base-format (format "%%0%dd" width))
         (padding-format (if (display-graphic-p) " %s" "%s "))
         (linum-format (format padding-format base-format)))
    ad-do-it))

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
 ;; call the garbage collector less often
 '(gc-cons-threshold (expt 2 24)) ; 16MB

 ;; customization interface output to a separate file
 '(custom-file "~/.emacs-customization")

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
 '(use-dialog-box nil)
 '(tab-width 4)
 '(blink-cursor-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-wheel-progressive-speed nil)
 '(mac-right-option-modifier 'none)
 '(ring-bell-function 'ignore)

 ;; *intuitive* scrolling
 '(scroll-conservatively 101)

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
 '(backup-directory-alist '(("." . "~/.emacs-backups")))

 ;; add some common safe file/dir variables to avoid prompt
 '(safe-local-variable-values
   '(;; auto-save personal dictionatu
     (ispell-silently-savep . t)
     ;; allows to define a personal local dictionary
     (eval setq ispell-personal-dictionary
           (concat default-directory ".dictionary"))))

 ;; no startup screen, use scratch
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   (mapconcat (lambda (line) (format ";; %s\n" line)) cheatsheet "")))

;;;;;;;;;;;
;; HOOKS ;;
;;;;;;;;;;;

;; linum mode for file-backed buffers only
;; (add-hook 'find-file-hook 'linum-mode) ; linum is fucking slow!

;; delete trailing spaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; visual line mode for compilation
(add-hook 'compilation-mode-hook
          (lambda () (visual-line-mode 1)))

;; useful highlightings
(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-regexp "TODO" 'hi-red-b)
            (highlight-regexp "XXX" 'hi-red-b)))

;; flyspell the whole buffer on entering the mode
(add-hook 'flyspell-mode-hook 'flyspell-buffer)

;;;;;;;;;;;;;;;
;; FUNCTIONS ;;
;;;;;;;;;;;;;;;

(defun recompile-or-compile ()
  (interactive)
  (if (fboundp 'recompile)
      (recompile)
    (call-interactively 'compile)))

(defun reload-buffer ()
  (interactive)
  (revert-buffer t t))

;;;;;;;;;;;;;;;
;; SHORTCUTS ;;
;;;;;;;;;;;;;;;

;; avoid suspend-frame (use C-x C-z instead)
(global-unset-key (kbd "C-z"))

;; disable keys to fix bad behaviors
(global-unset-key (kbd "<deletechar>"))
(global-unset-key (kbd "<home>"))
(global-unset-key (kbd "<next>"))
(global-unset-key (kbd "<prior>"))

;; ibuffer override
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; common tools
(global-set-key (kbd "C-<f5>") 'reload-buffer)
(global-set-key (kbd "C-<f7>") 'compile)
(global-set-key (kbd "<f5>") 'rgrep)
(global-set-key (kbd "<f6>") 'eshell)
(global-set-key (kbd "<f7>") 'recompile-or-compile)
(global-set-key (kbd "<f8>") 'package-list-packages)

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

;; winner-mode
(winner-mode 1)
