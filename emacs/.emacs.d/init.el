;;;;;;;;;;;;;;;;
;; CHEATSHEET ;;
;;;;;;;;;;;;;;;;

;; % m        dired-mark-files-regexp
;; C-s C-w    isearch-yank-word-or-char
;; C-x -      shrink-window-if-larger-than-buffer
;; C-x 8 RET  insert-char
;; C-x C-;    comment-line
;; C-x C-SPC  pop-global-mark
;; C-x C-b    list-buffers
;; C-x C-q    wdired-change-to-wdired-mode
;; C-x M-:    repeat-complex-command
;; C-x SPC    rectangle-mark-mode
;; C-x d      dired
;; C-x n n    narrow-to-region
;; C-x n w    widen
;; C-x r SPC  point-to-register
;; C-x r j    jump-to-register
;; C-x r w    window-configuration-to-register
;; C-x w r    unhighlight-regexp
;; C-x z      repeat
;; M-S-/      dabbrev-expand
;; M-^        delete-indentation
;; M-h        mark-paragraph
;; M-s h .    highlight-symbol-at-point
;; M-s h r    highlight-regexp
;; M-z        zap-to-char
;; M-|        shell-command-on-region (replace with C-u)

;; C-F5       reload-buffer
;; F5         rgrep
;; F6         shell
;; F7         recompile-or-compile
;; F8         package-list-packages
;; F12        magit-status

;;;;;;;;;;;;;;;;;;;;
;; PACKAGES SETUP ;;
;;;;;;;;;;;;;;;;;;;;

;; initialize the package system
(package-initialize)

;; add additional repositories
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; refresh the package list and install custom packages the first time
(unless package-archive-contents
  (package-refresh-contents)
  (mapc
   'package-install
   '(auctex
     cmake-mode
     exec-path-from-shell
     go-mode
     js2-mode
     json-mode
     magit
     markdown-mode
     php-mode
     protobuf-mode
     rainbow-mode
     yaml-mode)))

;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATIONS ;;
;;;;;;;;;;;;;;;;;;;;

;; theme
(load-theme 'my t)

;; cleaner rgrep output
(defadvice rgrep (after delete-grep-header activate)
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

;; fix PATH on macOS
(exec-path-from-shell-initialize)

;; use custom window separator character in nox mode
(set-display-table-slot standard-display-table 'vertical-border #x2502)

;; enable S-arrows window focus switch
(windmove-default-keybindings)

;; ignore `node_modules` folder in searches
(eval-after-load "grep"
  '(add-to-list 'grep-find-ignored-directories "node_modules"))

;; customization interface output to a separate file
(setq custom-file "~/.emacs.d/customization")
(load custom-file t)

;; variables
(custom-set-variables
 ;; make fail at the first error
 '(compile-command "make")

 ;; call the garbage collector less often
 '(gc-cons-threshold (expt 2 24)) ; 16MB

 ;; file management
 '(vc-follow-symlinks t)

 ;; editing
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
 '(use-dialog-box nil)
 '(tab-width 4)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(blink-cursor-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-wheel-progressive-speed nil)
 '(mac-right-option-modifier 'none)
 '(ring-bell-function 'ignore)
 '(font-lock-maximum-decoration 2)
 '(winner-mode t)

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
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))

 ;; add some common safe file/dir variables to avoid prompt
 '(safe-local-variable-values
   '(;; auto-save personal dictionatu
     (ispell-silently-savep . t)
     ;; allows to define a personal local dictionary
     (eval setq ispell-personal-dictionary
           (concat default-directory ".dictionary"))))

 ;; no startup screen, use scratch
 '(inhibit-startup-screen t)
 '(initial-scratch-message ""))

;;;;;;;;;;;
;; HOOKS ;;
;;;;;;;;;;;

;; delete trailing spaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; visual line mode for compilation
(add-hook 'compilation-mode-hook
          (lambda () (visual-line-mode 1)))

;; useful highlightings
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<TODO\\>" 0 font-lock-warning-face t)
               ("\\<XXX\\>" 0 font-lock-warning-face t)))))

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
(global-unset-key (kbd "<end>"))
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))

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

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

;;;;;;;;;;;;
;; OTHERS ;;
;;;;;;;;;;;;

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(custom-set-faces
 '(js2-object-property ((t (:inherit font-lock-builtin-face)))))
