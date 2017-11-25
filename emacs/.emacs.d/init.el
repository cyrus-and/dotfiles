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
;; C-x TAB    indent-rigidly
;; C-x d      dired
;; C-x n n    narrow-to-region
;; C-x n w    widen
;; C-x r SPC  point-to-register
;; C-x r i    insert-register
;; C-x r s    copy-to-register
;; C-x r w    window-configuration-to-register
;; C-x w r    unhighlight-regexp
;; C-x z      repeat
;; M-?        xref-find-references
;; M-S-/      dabbrev-expand
;; M-^        delete-indentation
;; M-h        mark-paragraph
;; M-s h .    highlight-symbol-at-point
;; M-s h r    highlight-regexp
;; M-z        zap-to-char
;; M-|        shell-command-on-region (replace with C-u)

;; C-F5       reload-buffer
;; F6         recompile-or-compile
;; C-F6       compile
;; F8         magit-status

;;;;;;;;;;;
;; THEME ;;
;;;;;;;;;;;

(load-theme 'my t)

;;;;;;;;;;;;;;;;;;;;
;; PACKAGES SETUP ;;
;;;;;;;;;;;;;;;;;;;;

;; initialize the package system
(package-initialize)

;; add additional repositories
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; install packages
(let ((packages
       '(auctex
         avy
         cmake-mode
         edit-indirect
         exec-path-from-shell
         go-mode
         js2-mode
         json-mode
         magit
         markdown-mode
         password-store
         php-mode
         rainbow-mode
         web-mode
         window-numbering
         yaml-mode)))
  (when (member nil (mapcar 'package-installed-p packages))
    (package-refresh-contents)
    (mapc 'package-install packages)))

;;;;;;;;;;;;;;;;;;
;; DISABLE KEYS ;;
;;;;;;;;;;;;;;;;;;

(define-minor-mode disable-bad-keys-mode
  "Disable bad keys."
  :init-value t
  :keymap (let ((map (make-sparse-keymap)))
            (dolist (key '("<deletechar>"
                           "<home>" "<end>" "<prior>" "<next>"
                           "<up>" "<down>" "<left>" "<right>"))
              (dolist (mod '("" "C-" "M-" "C-M-"))
                (let ((keystroke (kbd (format "%s%s" mod key))))
                  (define-key map keystroke 'ignore))))
            map))

(custom-set-variables
 '(disable-bad-keys-mode t))

;;;;;;;;;;;;
;; MAC OS ;;
;;;;;;;;;;;;

;; macOS specific initialization
(when (eq system-type 'darwin)
  ;; fix PATH
  (exec-path-from-shell-initialize)

  (custom-set-variables
   ;; allow to type fancy glyphs using the righ meta key
   '(mac-right-option-modifier 'none)))

;; TODO set font size to 15

;;;;;;;;;;;;;;
;; TERMINAL ;;
;;;;;;;;;;;;;;

;; use custom window separator character in nox mode
(set-display-table-slot standard-display-table 'vertical-border #x2502)

;;;;;;;;;
;; GUI ;;
;;;;;;;;;

;; avoid suspend-frame
(when (display-graphic-p)
  (global-unset-key (kbd "C-z")))

;;;;;;;;;;;;;;;;;;;;;;;;
;; EASY REVERT BUFFER ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun force-revert-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(global-set-key (kbd "C-<f5>") 'force-revert-buffer)

;;;;;;;;;;;;
;; ESHELL ;;
;;;;;;;;;;;;

(custom-set-variables
 '(eshell-prompt-regexp "^[$#] ")
 '(eshell-prompt-function
   (lambda ()
     (concat (abbreviate-file-name (eshell/pwd))
             "\n" (if (= (user-uid) 0) "#" "$") " "))))

(global-set-key (kbd "<f6>") 'eshell)

;;;;;;;;;;;;;;;;;
;; COMPILATION ;;
;;;;;;;;;;;;;;;;;

(defun recompile-or-compile ()
  "Recompile or prompt a new compilation."
  (interactive)
  (if (fboundp 'recompile)
      (recompile)
    (call-interactively 'compile)))

(custom-set-variables
 '(compile-command "make")
 '(compilation-scroll-output 'first-error)
 '(compilation-always-kill t))

;; visual line mode for compilation
(add-hook 'compilation-mode-hook 'visual-line-mode)

(global-set-key (kbd "C-<f7>") 'compile)
(global-set-key (kbd "<f7>") 'recompile-or-compile)

;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATION ;;
;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; disable the customization interface output
 '(custom-file "/dev/null"))

;;;;;;;;;;;;
;; BACKUP ;;
;;;;;;;;;;;;

(custom-set-variables
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups"))))

;;;;;;;;
;; UI ;;
;;;;;;;;

(custom-set-variables
 '(disabled-command-function nil)
 '(use-dialog-box nil)
 '(tab-width 4)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(blink-cursor-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(ring-bell-function 'ignore)
 '(font-lock-maximum-decoration 2)
 '(help-window-select t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(vc-follow-symlinks t)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(require-final-newline 'ask)
 '(electric-pair-mode t)
 '(truncate-lines t))

;;;;;;;;;;;
;; DIRED ;;
;;;;;;;;;;;

(custom-set-variables
 '(dired-listing-switches
   (if (eq system-type 'darwin)
       "-al" "-al --group-directories-first")))

;;;;;;;;;;;;;;;;
;; MINIBUFFER ;;
;;;;;;;;;;;;;;;;

(custom-set-variables
 '(savehist-mode t)
 '(history-length t))

;;;;;;;;;;;;;;;
;; SCROLLING ;;
;;;;;;;;;;;;;;;

;; scroll without jumps if the cursor moves out the window boundaries
(custom-set-variables
 '(scroll-conservatively 101)
 '(hscroll-step 1))

;;;;;;;;;;;;;;;;;;
;; HIGHLIGHTING ;;
;;;;;;;;;;;;;;;;;;

;; useful keyword highlighting
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<TODO\\>" 0 font-lock-warning-face t)
               ("\\<XXX\\>" 0 font-lock-warning-face t)))))

;;;;;;;;;;;
;; MOUSE ;;
;;;;;;;;;;;

(custom-set-variables
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-yank-at-point t))

(defun yank-primary ()
  "Yank the primary selection (the one selected with the mouse)."
  (interactive)
  (insert-for-yank (gui-get-primary-selection)))

;; yank primary selection with the keyboard
(global-set-key (kbd "S-<insert>") 'yank-primary)

;;;;;;;;;;;;
;; PYTHON ;;
;;;;;;;;;;;;

(custom-set-variables
 '(python-shell-interpreter "python3"))

;;;;;;;;;;;;;;;;
;; PERFORMACE ;;
;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; call the garbage collector less often
 '(gc-cons-threshold (expt 2 24))) ; 16MB

;;;;;;;;;;;;;;;;
;; FORMATTING ;;
;;;;;;;;;;;;;;;;

(custom-set-variables
 '(fill-column 80)
 '(c-backslash-column 79)
 '(c-backslash-max-column 79)
 '(indent-tabs-mode nil)
 '(c-basic-offset 4)
 '(c-offsets-alist
   '((substatement-open . 0)
     (brace-list-intro . +)
     (arglist-intro . +)
     (arglist-close . 0)
     (cpp-macro . 0)
     (innamespace . 0))))

;; delete trailing spaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;
;; IBUFFER ;;
;;;;;;;;;;;;;

;; list-buffer alternative
(defalias 'list-buffers 'ibuffer)

;;;;;;;;;;
;; GREP ;;
;;;;;;;;;;

;; ignore Node.js folder in searches
(eval-after-load "grep"
  '(add-to-list 'grep-find-ignored-directories "node_modules"))

;; cleaner rgrep output
(defadvice rgrep (after delete-grep-header activate)
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

;;;;;;;;;;;
;; MAGIT ;;
;;;;;;;;;;;

;; shortcut to main magit buffer
(global-set-key (kbd "<f8>") 'magit-status)

;; use magit for `git commit` and enable spell checking
(global-git-commit-mode)
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

;;;;;;;;;;;;;;
;; JS2-MODE ;;
;;;;;;;;;;;;;;

;; mode trigger
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;;;;;;;;;;;;
;; WEB-MODE ;;
;;;;;;;;;;;;;;

;; mode trigger
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;;;;;;;;;;;;;
;; ORG-MODE ;;
;;;;;;;;;;;;;;

(custom-set-variables
 '(org-startup-folded nil)
 '(org-cycle-separator-lines 1)
 '(org-blank-before-new-entry '((heading . t) (plain-list-item))))

;; visual indentation for org files
(add-hook 'org-mode-hook 'visual-line-mode)

;;;;;;;;;;
;; ZOOM ;;
;;;;;;;;;;

;; use local copy
(require 'zoom "~/dev/zoom/zoom.el")

(custom-set-variables
 '(zoom-mode t)
 '(zoom-size '(90 . 30))
 ;; resize temp buffers anyway
 '(temp-buffer-resize-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WINDOW-NUMBERING-MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(window-numbering-mode t))

;;;;;;;;;;;;;;;;;;;;;
;; SAVE-PLACE-MODE ;;
;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(save-place-mode t))

;;;;;;;;;
;; AVY ;;
;;;;;;;;;

;; override `goto-line'
(defalias 'goto-line 'avy-goto-line)

;; avy main shortcut
(global-set-key (kbd "C-ò") 'avy-goto-char-timer)

;;;;;;;;;
;; ERC ;;
;;;;;;;;;

;; automatic Freenode connection (need a graphical pinentry)
(defun irc ()
  (interactive)
  (let* ((credentials (split-string (password-store-get "Freenode")))
         (nick (nth 0 credentials))
         (password (nth 1 credentials)))
    (erc
     :server "irc.freenode.net"
     :port 6667
     :nick nick
     :password password)))
