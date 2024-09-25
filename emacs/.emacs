;; -*- lexical-binding: t -*- ;;

;; TODO coherent key mapping
;; TODO coherent documentation/comments

;;; PERFORMANCE

;; TODO reorganize?

;; call the garbage collector less often during the startup then restore the
;; initial value to avoid longer pauses during the interactive usage (note that
;; `custom-reevaluate-setting' only works as long as the value has not been
;; customized)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (custom-reevaluate-setting 'gc-cons-threshold)))

;; force the garbage collection to happen when the focus moves away from emacs
(add-hook 'focus-out-hook 'garbage-collect)

(defun display-startup-echo-area-message ()
  (message "Emacs started in %.2f seconds triggering the GC %d times taking %.2f seconds"
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done gc-elapsed))

;;; CONFIGURATION

;;;; CONSTANTS

;; some colors from from https://terminal.sexy

;; UI and base colors
(setq my/color-accent  "#ff6000")
(setq my/color-level-0 "#000000")
(setq my/color-level-1 "#1c1b1c")
(setq my/color-level-2 "#282A2E")
(setq my/color-level-3 "#373B41")
(setq my/color-level-4 "#e8dbb6")
(setq my/color-level-5 "#ffffff")

;; common colors
(setq my/color-red     "#A54242")
(setq my/color-green   "#8C9440")
(setq my/color-yellow  "#DE935F")
(setq my/color-blue    "#5F819D")
(setq my/color-magenta "#85678F")
(setq my/color-cyan    "#5E8D87")
(setq my/color-gray    "#707880")

;; font parameters
(setq my/font-family "Iosevka SS04")
(setq my/font-size   150)

;; other
(setq theme-divider-width 14)

;;;; COMMON FACES

;;;;; BASIC FACES

(custom-set-faces
 `(default        ((t (:foreground ,my/color-level-4 :background ,my/color-level-1 :family, my/font-family :height ,my/font-size))))
 `(link           ((t (:foreground ,my/color-accent))))
 `(link-visited   ((t (:inherit (link) :weight normal))))
 `(highlight      ((t (:foreground ,my/color-level-1 :background ,my/color-level-4))))
 `(isearch        ((t (:foreground ,my/color-level-1 :background ,my/color-accent))))
 `(lazy-highlight ((t (:foreground ,my/color-level-1 :background ,my/color-level-4))))
 `(match          ((t (:inherit (lazy-highlight)))))
 `(shadow         ((t (:foreground ,my/color-gray))))
 `(error          ((t (:foreground ,my/color-red))))
 `(warning        ((t (:foreground ,my/color-yellow))))
 `(success        ((t (:foreground ,my/color-green)))))

;;;;; HEADER/MODE LINE

(custom-set-faces
 `(mode-line           ((t (:foreground ,my/color-level-1 :background ,my/color-level-4))))
 `(mode-line-inactive  ((t (:inherit (mode-line)))))
 `(mode-line-highlight ((t (:background ,my/color-accent))))
 `(header-line         ((t (:inherit (mode-line))))))

;;;;; FONT LOCK

(custom-set-faces
 `(font-lock-function-name-face ((t (:inherit (bold) :foreground ,my/color-magenta))))
 `(font-lock-variable-name-face ((t (:foreground ,my/color-yellow))))
 `(font-lock-keyword-face       ((t (:inherit (bold) :foreground ,my/color-red))))
 `(font-lock-comment-face       ((t (:foreground ,my/color-gray))))
 `(font-lock-type-face          ((t (:foreground ,my/color-blue))))
 `(font-lock-constant-face      ((t (:foreground ,my/color-cyan))))
 `(font-lock-builtin-face       ((t (:foreground ,my/color-cyan))))
 `(font-lock-string-face        ((t (:foreground ,my/color-green))))
 `(font-lock-negation-char-face ((t (:inherit (default bold))))))

;;;;; OUTLINES

(custom-set-faces
 `(outline-1 ((t (:inherit (bold) :foreground ,my/color-blue))))
 `(outline-2 ((t (:inherit (bold) :foreground ,my/color-yellow))))
 `(outline-3 ((t (:inherit (bold) :foreground ,my/color-green))))
 `(outline-4 ((t (:inherit (bold) :foreground ,my/color-magenta))))
 `(outline-5 ((t (:inherit (bold) :foreground ,my/color-red))))
 `(outline-6 ((t (:inherit (bold) :foreground ,my/color-red))))
 `(outline-7 ((t (:inherit (bold) :foreground ,my/color-red))))
 `(outline-8 ((t (:inherit (bold) :foreground ,my/color-red)))))

;;;;; OTHERS

(custom-set-faces
 `(completions-common-part    ((t (:foreground ,my/color-level-1 :background ,my/color-accent))))
 `(cursor                     ((t (:background ,my/color-accent))))
 `(diff-refine-changed        ((t (:extend t))))
 `(fringe                     ((t (:inherit (shadow)))))
 `(isearch-fail               ((t (:inherit (error)))))
 `(minibuffer-prompt          ((t (:inherit (bold) :foreground ,my/color-accent))))
 `(pulse-highlight-start-face ((t (:background ,my/color-accent))))
 `(region                     ((t (:foreground ,my/color-level-1 :background ,my/color-level-5 :extend t))))
 `(secondary-selection        ((t (:foreground ,my/color-accent :background ,my/color-level-5 :extend t))))
 `(show-paren-match           ((t (:inherit (bold) :foreground ,my/color-accent))))
 `(show-paren-mismatch        ((t (:inherit (error) :inverse-video t)))))

;;;; OPTIONS

;;;;; BACKUPS

(custom-set-variables
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups"))))

;;;;; CURSOR

(custom-set-variables
 '(blink-cursor-mode nil)
 '(cursor-type '(bar . 3))
 '(cursor-in-non-selected-windows nil))

;;;;; CUSTOMIZATION INTERFACE

;; this is used to avoid messing with this very init file
(custom-set-variables
 '(custom-file "/tmp/unused.el"))

;;;;; DIRED

;; use the native ls implementation to be consistent across platforms
(with-eval-after-load 'dired
  (require 'ls-lisp))

(custom-set-variables
 '(ls-lisp-dirs-first t)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity '(uid gid)))

;;;;; ENVIRONMENT VARIABLES

;; add Nix executables to the path
(setenv "PATH" (format "%s:%s" (getenv "PATH") (expand-file-name "~/.nix-profile/bin/")))

;;;;; FFAP

(global-set-key (kbd "s-F") 'ffap-menu)

;;;;; IBUFFER

(defalias 'list-buffers 'ibuffer)

(custom-set-variables
 '(ibuffer-expert t))

;;;;; INDENTATION

(custom-set-variables
 '(c-backslash-column 79)
 '(c-backslash-max-column 79)
 '(c-basic-offset 4)
 '(c-offsets-alist
   '((substatement-open . 0)
     (brace-list-intro . +)
     (arglist-intro . +)
     (arglist-close . 0)
     (cpp-macro . 0)
     (inlambda . 0)
     (innamespace . 0)))
 '(fill-column 80))

;;;;; ISEARCH

(custom-set-variables
 '(isearch-allow-motion t)
 '(lazy-highlight-buffer t)
 '(lazy-highlight-cleanup nil))

;;;;; MACOS SPECIFIC

(when (eq system-type 'darwin)
  (custom-set-variables
   '(mac-right-option-modifier 'none))

  (global-set-key (kbd "<s-up>") 'beginning-of-buffer)
  (global-set-key (kbd "<s-down>") 'end-of-buffer))

;;;;; MINIBUFFER

(custom-set-variables
 '(history-length t)
 '(savehist-mode t))

;;;;; MODE LINE

;; XXX this requires projectile

(defun my/abbreviate-path (path)
  (let* ((path (abbreviate-file-name path)))
    (if (> (length path) (/ (window-total-width) 2))
        (replace-regexp-in-string (rx (and (group (not "/")) (* (not "/")))) "\\1" path)
      path)))

(custom-set-variables
 `(mode-line-format
   '(" "
     "%Z%*%@"
     "  "
     (:eval (when (projectile-project-p)
              (format "%s » " (propertize (projectile-project-name) 'face 'bold))))
     (:eval (when (buffer-file-name)
              (if (projectile-project-p)
                  ;; take the project-relative path and abbreviate it
                  (my/abbreviate-path (file-relative-name
                                       (file-truename default-directory)
                                       (projectile-project-root)))
                ;; abbreviate the current directory
                (my/abbreviate-path default-directory))))
     (:eval (propertize (or (uniquify-buffer-base-name) (buffer-name)) 'face 'bold))
     "  "
     "+%l:%c"
     (:eval (format " (%.0f%%%%)" (* (/ (float (line-number-at-pos)) (line-number-at-pos (point-max))) 100)))
     "  "
     mode-line-modes
     " " ; XXX one extra space is already there
     global-mode-string)))

;;;;; MOUSE

(custom-set-variables
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-yank-at-point t))

;;;;; SERVER

(require 'server)
(unless (server-running-p)
  (server-start))

;;;;; STARTUP

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-scratch-message ""))

;;;;; USER INTERFACE

(add-hook 'text-mode-hook 'visual-line-mode)
(add-to-list 'default-frame-alist '(undecorated . t))
(windmove-default-keybindings)

(custom-set-variables
 '(confirm-kill-emacs 'y-or-n-p)
 '(echo-keystrokes 0.001)
 '(frame-resize-pixelwise t)
 '(help-window-select t)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(menu-bar-mode nil)
 '(native-comp-async-report-warnings-errors nil)
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position 'always)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(use-dialog-box nil)
 '(vc-follow-symlinks t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(winner-mode t))

;;;;; WINDOW DIVIDERS

(custom-set-variables
 '(window-divider-mode t)
 '(window-divider-default-places t)
 '(window-divider-default-bottom-width theme-divider-width)
 '(window-divider-default-right-width theme-divider-width))

(custom-set-faces
 `(window-divider             ((t (:foreground ,my/color-level-3))))
 `(window-divider-first-pixel ((t (:foreground ,my/color-level-3))))
 `(window-divider-last-pixel  ((t (:foreground ,my/color-level-3))))
 `(internal-border            ((t (:background ,my/color-level-3)))))

;; TODO does not work in vanilla emacs for macOS...
;; (my/eval-after-load
;;   (add-to-list 'default-frame-alist `(internal-border-width . ,theme-divider-width)))

;;;;; WHITESPACE MANAGEMENT

(custom-set-variables
 '(indent-tabs-mode nil)
 '(require-final-newline 'ask))

(defun my/whitespace-cleanup--handler ()
  "Normalize whitespaces when `my/whitespace-cleanup-mode' is enabled."
  (when my/whitespace-cleanup-mode
    (whitespace-cleanup)))

(define-minor-mode my/whitespace-cleanup-mode
  "Normalize whitespaces on save."
  :init-value t
  :lighter " W"
  (my/whitespace-cleanup--handler))

(add-hook 'before-save-hook 'my/whitespace-cleanup--handler)

;; TODO (global-set-key (kbd "C-c d") 'my/trim-whitespace-mode)

;;;;; TRIM WHITESPACE MODE

;;;; PACKAGES

(defun my/install (package)
  "Install a package refreshing the packet list just once."
  (unless (package-installed-p package)
    (unless (bound-and-true-p my/install-refreshed)
      (package-refresh-contents)
      (setq my/install-refreshed t))
    (package-install package t)))

(defun my/upgrade-packages ()
  "Upgrade and clean up packages."
  (interactive)
  (with-current-buffer (package-list-packages t)
    (package-refresh-contents)
    (package-menu-mark-upgrades)
    (ignore-errors (package-menu-execute t))
    (package-autoremove)
    (kill-buffer)))

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; TODO (global-set-key (kbd "C-c u") 'my/upgrade-packages)

;;;;; ADAPTIVE WRAP

(my/install 'adaptive-wrap)

(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

;;;;; AVY

(my/install 'avy)

(global-set-key (kbd "s-.") 'avy-goto-char-timer)
(global-set-key (kbd "s-l") 'avy-goto-line)

;;;;; COMPANY

(my/install 'company)

(custom-set-variables
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(global-company-mode t))

(custom-set-faces
 `(company-scrollbar-bg       ((t (:background ,my/color-level-3))))
 `(company-scrollbar-fg       ((t (:background ,my/color-accent))))
 `(company-tooltip            ((t (:inherit (default)))))
 `(company-tooltip-annotation ((t (:inherit (shadow)))))
 `(company-tooltip-common     ((t (:inherit (completions-common-part)))))
 `(company-tooltip-selection  ((t (:inherit (highlight))))))

;;;;;; COMPANY-POSFRAME

(my/install 'company-posframe)

(custom-set-variables
 '(company-posframe-mode t)
 '(company-posframe-quickhelp-delay nil)
 '(company-posframe-show-indicator nil)
 '(company-posframe-show-metadata nil)
 '(company-posframe-show-params `(:internal-border-color ,my/color-level-2 :internal-border-width 5)))

;;;;; CONSULT

(my/install 'consult)

(global-set-key (kbd "C-x r j") 'consult-register)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "s--") 'consult-line)
(global-set-key (kbd "s-b") 'consult-project-buffer)
(global-set-key (kbd "s-i") 'consult-imenu)
(global-set-key (kbd "s-m") 'consult-outline)
(global-set-key (kbd "s-g") 'consult-ripgrep)
(global-set-key (kbd "s-j") 'consult-global-mark)

;;;;; DIFF-HL

(my/install 'diff-hl)

(custom-set-variables
 '(diff-hl-draw-borders nil)
 '(diff-hl-show-hunk-inline-popup-hide-hunk t)
 '(diff-hl-show-hunk-inline-popup-smart-lines nil)
 '(diff-hl-show-staged-changes nil)
 '(global-diff-hl-mode t))

(custom-set-faces
 '(diff-hl-change ((t (:inherit (warning) :inverse-video t))))
 '(diff-hl-insert ((t (:inherit (success) :inverse-video t))))
 '(diff-hl-delete ((t (:inherit (error) :inverse-video t)))))

;; enable annotations for dired buffers too
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; update the indicators also after a commit
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;;;;; MAGIT

(my/install 'magit)

(custom-set-variables
 '(magit-section-initial-visibility-alist '((stashes . hide) (unpushed . show)))
 '(with-editor-emacsclient-executable "emacsclient")) ; XXX fix warning with Nix

(custom-set-faces
 `(git-commit-overlong-summary ((t (:inherit (error) :inverse-video t))))
 `(magit-header-line ((t (:inherit (header-line))))))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
(global-set-key (kbd "C-c s") 'magit-status)

;;;;; MARGINALIA

(my/install 'marginalia)

(custom-set-variables
 '(marginalia-mode t))

;;;;; MINIONS

(my/install 'minions)

(custom-set-variables
 '(minions-mode t)
 '(minions-mode-line-lighter "···")
 '(minions-direct '(overwrite-mode)))

;;;;; OUTSHINE

(my/install 'outshine)

(with-eval-after-load
    'outshine
  (outshine-define-key outshine-mode-map (kbd "<backtab>") 'outshine-cycle-buffer t))

;;;;; PROJECTILE

(my/install 'projectile)

(setq my/projectile-window-configurations (make-hash-table :test 'equal))

(defun my/projectile-save-window-configuration ()
  "Save the current window configuration for the current project."
  (when (projectile-project-p)
    (puthash (projectile-project-root) (current-window-configuration) my/projectile-window-configurations)))

(defun my/projectile-restore-window-configuration ()
  "Restore the window configuration or start anew."
  (let ((configuration (gethash (projectile-project-root) my/projectile-window-configurations)))
    (if configuration
        (set-window-configuration configuration)
      (delete-other-windows))))

(defun my/projectile-open (filename)
  "Open a new project or switch to an existing one by selecting one of its files."
  (interactive "fFind file: ")
  (if (member (file-truename (projectile-project-root filename))
              (mapcar 'file-truename projectile-known-projects))
      (projectile-switch-project-by-name (projectile-project-root filename))
    (my/projectile-save-window-configuration)
    (find-file filename)
    (delete-other-windows)))

(add-hook 'projectile-before-switch-project-hook 'my/projectile-save-window-configuration)
(add-hook 'projectile-after-switch-project-hook 'my/projectile-restore-window-configuration)

(custom-set-variables
 '(projectile-mode t)
 '(projectile-switch-project-action 'projectile-dired))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "s-D") 'projectile-dired)
  (define-key projectile-mode-map (kbd "s-G") 'projectile-ripgrep)
  (define-key projectile-mode-map (kbd "s-K") 'projectile-remove-known-project)
  (define-key projectile-mode-map (kbd "s-d") 'projectile-find-dir)
  (define-key projectile-mode-map (kbd "s-f") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "s-k") 'projectile-kill-buffers)
  (define-key projectile-mode-map (kbd "s-o") 'my/projectile-open)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-switch-project)
  (define-key projectile-mode-map (kbd "s-s") 'projectile-run-vterm))

;;;;; RAINBOW-MODE

(my/install 'rainbow-mode)

;;;;; RIPGREP

(my/install 'ripgrep)

(defun my/ripgrep-fix ()
  "Enforce a cleaner ripgrep output by hiding the command."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (forward-line 3)
      (kill-whole-line))))

(custom-set-variables
 '(ripgrep-arguments '("--no-ignore" "--hidden" "--binary" "--glob !.git/")))

(add-hook 'ripgrep-search-finished-hook 'my/ripgrep-fix)

;;;;; VERTICO

(my/install 'vertico)

(custom-set-variables
 '(vertico-mode t))

;;;;;; ORDERLESS

(my/install 'orderless)

(custom-set-variables
 '(completion-category-defaults nil)
 '(completion-styles '(orderless))
 '(orderless-match-faces [completions-common-part]))

;;;;; VTERM

(my/install 'vterm)

(custom-set-variables
 '(vterm-kill-buffer-on-exit nil))

;; TODO (global-set-key (kbd "C-c a") 'vterm)

;;;;; WINUM

(my/install 'winum)

(custom-set-variables
 '(winum-mode t)
 '(winum-format (propertize " %s " 'face 'winum-face))
 '(winum-mode-line-position 0)
 '(winum-scope 'frame-local))

(custom-set-faces
 `(winum-face ((t (:foreground ,my/color-level-1 :background ,my/color-accent)))))

(setq winum-keymap (make-sparse-keymap))
(define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
(define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
(define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
(define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
(define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
(define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
(define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
(define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
(define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
(define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)

(defun my/winum-switch ()
  (interactive)
  (if (<= (count-windows) 2)
      (other-window 1)
    (let ((char (read-key (propertize
                           "Switch to window number? (C-g to abort; any key toggle)"
                           'face 'minibuffer-prompt))))
      (unless (= char ?\C-g)
        (if (<= ?0 char ?9)
            (winum-select-window-by-number (- char ?0))
          (select-window (get-mru-window nil t t)))))))

(global-set-key (kbd "C-x o") 'my/winum-switch)

;;;;; ZOOM

(my/install 'zoom)

(custom-set-variables
 '(zoom-mode t)
 '(zoom-size '(120 . 30))
 '(temp-buffer-resize-mode t))

;;; FILE VARIABLES

;; Local Variables:
;; eval: (rainbow-mode)
;; eval: (outshine-mode)
;; End:
