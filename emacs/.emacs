;; -*- lexical-binding: t -*-

;;; INTRO

;; call the garbage collector less often during the startup
(setq gc-cons-threshold most-positive-fixnum)

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
(setq my/color-blue    "#5F819D")
(setq my/color-cyan    "#5E8D87")
(setq my/color-yellow  "#DE935F")
(setq my/color-magenta "#85678F")
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

;;;;; COMPILATION

(custom-set-faces
 '(compilation-mode-line-exit ((t (:inherit (success)))))
 '(compilation-mode-line-run  ((t (:inherit (warning)))))
 '(compilation-mode-line-fail ((t (:inherit (error))))))

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

;;;;; TERMINAL

(custom-set-faces
 `(term-color-black   ((t (:foreground ,my/color-level-0))))
 `(term-color-white   ((t (:foreground ,my/color-level-5))))
 `(term-color-red     ((t (:foreground ,my/color-red))))
 `(term-color-green   ((t (:foreground ,my/color-green))))
 `(term-color-yellow  ((t (:foreground ,my/color-yellow))))
 `(term-color-blue    ((t (:foreground ,my/color-blue))))
 `(term-color-magenta ((t (:foreground ,my/color-magenta))))
 `(term-color-cyan    ((t (:foreground ,my/color-cyan)))))

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

;;;;; CODE ANNOTATIONS

(defun my/code-annotations ()
  (let ((regexp (rx bow (or "TODO" "XXX") eow))
        (face 'font-lock-warning-face))
    (font-lock-add-keywords nil `((,regexp 0 ,face prepend)))))

(add-hook 'text-mode-hook 'my/code-annotations)
(add-hook 'prog-mode-hook 'my/code-annotations)

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

;;;;; ERROR NAVIGATION

(keymap-global-set "s-," 'previous-error)
(keymap-global-set "s-." 'next-error)

;;;;; EXECUTABLES PATH

;; add user paths to `exec-path' and update PATH accordingly
(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
(add-to-list 'exec-path (expand-file-name "~/.bin"))
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (string-join exec-path ":"))

;;;;; FFAP

(keymap-global-set "s-j" 'ffap-menu)

;;;;; FIND TO DIRED

(custom-set-variables
 '(find-name-arg "-ipath")) ; use the whole path

(keymap-global-set "s-F" 'find-name-dired)

;;;;; FORCE REVERT BUFFER

(defun my/force-revert-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (lazy-highlight-cleanup t)
  (revert-buffer t t)
  (run-hooks 'window-buffer-change-functions)) ; XXX used to update the mode line

(keymap-global-set "s-<backspace>" 'my/force-revert-buffer)

;;;;; FRIENDLIER SCRATCH BUFFER

(defun my/scratch-buffer-save ()
  "Show the scracth save override message."
  (interactive)
  (message (substitute-command-keys "Use \\[write-file] to save.")))

(defun my/scratch-override-save (scratch)
  "Override `save-buffer' key in scratch buffer to help muscle memory."
  (with-current-buffer scratch
    (keymap-local-set "C-x C-s" 'my/scratch-buffer-save)
    scratch))

(advice-add 'get-scratch-buffer-create :filter-return 'my/scratch-override-save)

(defun my/scratch-toggle ()
  "Toggle the scratch buffer."
  (interactive)
  (if (eq (current-buffer) (get-scratch-buffer-create))
      (bury-buffer)
    (let ((scratch-window (get-buffer-window (get-scratch-buffer-create))))
      (if scratch-window
          (select-window scratch-window)
        (scratch-buffer)))))

(keymap-global-set "s-'" 'my/scratch-toggle)

;;;;; HI LOCK

(defface my/hi `((t (:box (:line-width 1 :color ,my/color-level-4 :style flat-button) :foreground ,my/color-level-1))) nil)
(defface my/hi-red `((t (:inherit (my/hi) :background ,my/color-red))) nil)
(defface my/hi-green `((t (:inherit (my/hi) :background ,my/color-green))) nil)
(defface my/hi-blue `((t (:inherit (my/hi) :background ,my/color-blue))) nil)
(defface my/hi-cyan `((t (:inherit (my/hi) :background ,my/color-cyan))) nil)
(defface my/hi-yellow `((t (:inherit (my/hi) :background ,my/color-yellow))) nil)
(defface my/hi-magenta `((t (:inherit (my/hi) :background ,my/color-magenta))) nil)

(custom-set-variables
 '(hi-lock-auto-select-face t)
 '(hi-lock-face-defaults '("my/hi-red" "my/hi-green" "my/hi-blue" "my/hi-cyan" "my/hi-yellow" "my/hi-magenta")))

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

;;;;; ISPELL

;; this is for hunspell if installed via Nix
(setenv "DICPATH" (expand-file-name "~/.nix-profile/share/hunspell/"))

;;;;; MACOS SPECIFIC

(when (eq system-type 'darwin)
  (custom-set-variables
   '(mac-right-option-modifier 'none))

  (keymap-global-set "s-<up>" 'beginning-of-buffer)
  (keymap-global-set "s-<down>" 'end-of-buffer))

;;;;; MINIBUFFER

(custom-set-variables
 '(history-length t)
 '(savehist-mode t))

;;;;; MODE LINE

(defun my/mode-line-abbreviate-path (path)
  (replace-regexp-in-string (rx (and (group (not "/")) (* (not "/")))) "\\1" path))

(defun my/mode-line-update-variables (&rest args)
  (require 'projectile)

  (setq-local
   my/mode-line-directory
   (when (or (buffer-file-name) (derived-mode-p 'dired-mode))
     (let ((directory (file-truename default-directory)))
       (when (projectile-project-p)
         (setq directory (file-relative-name directory (projectile-project-root))))
       (when (derived-mode-p 'dired-mode)
         (setq directory (file-name-directory (directory-file-name directory))))
       (when (equal directory "./")
         (setq directory nil))
       (when directory
         (replace-regexp-in-string "%" "%%" directory)))))

  (setq-local
   my/mode-line-buffer
   (if (and (derived-mode-p 'dired-mode)
            (projectile-project-p)
            (file-equal-p (projectile-project-root) default-directory))
       "." (replace-regexp-in-string "%" "%%" (or (uniquify-buffer-base-name) (buffer-name)))))

  (setq-local
   my/mode-line-projectile-project-name
   (and (projectile-project-p) (projectile-project-name)))

  nil)

(add-hook 'window-buffer-change-functions 'my/mode-line-update-variables)

(defface my/mode-line-number `((t (:foreground ,my/color-level-1 :background ,my/color-accent))) nil)
(defface my/mode-line-marker `((t (:foreground ,my/color-accent :weight bold))) nil)
(defface my/mode-line-buffer `((t (:weight bold :box (:line-width (1 . 3) :color ,my/color-level-4)))) nil)
(defface my/mode-line-buffer-modified `((t (:inherit (my/mode-line-buffer) :foreground ,my/color-accent))) nil)

(custom-set-variables
 `(mode-line-format
   '((:eval (unless (bound-and-true-p my/mode-line-buffer)
              (my/mode-line-update-variables)))
     (:eval `(:propertize (" " ,(winum-get-number-string) " ") face my/mode-line-number))
     "  %["
     (:eval (if (and (not buffer-read-only) (buffer-modified-p))
                `(:propertize ,my/mode-line-buffer face my/mode-line-buffer-modified)
              `(:propertize ,my/mode-line-buffer face my/mode-line-buffer)))
     "%] "
     (:eval (when my/mode-line-projectile-project-name
              `(" " (:propertize "@" face my/mode-line-marker)
                ,my/mode-line-projectile-project-name)))
     (:eval (when my/mode-line-directory
              `(" " (:propertize ":" face my/mode-line-marker)
                ,my/mode-line-directory)))
     (:eval (when (buffer-file-name)
              (let* ((system buffer-file-coding-system)
                     (coding (coding-system-mnemonic system))
                     (eol (coding-system-eol-type-mnemonic system))
                     (marker (concat (unless (= coding ?-) (string coding))
                                     (unless (equal eol ":")))))
                (unless (string-empty-p marker)
                  `(" " (:propertize "$" face my/mode-line-marker) ,marker)))))
     (:eval (when (buffer-file-name)
              '(" " (:propertize "+" face my/mode-line-marker)
                "%l"
                (:propertize "/" face my/mode-line-marker)
                (:eval (format "%d" (line-number-at-pos (point-max))))
                (:propertize ":" face my/mode-line-marker)
                "%c"))))))

;;;;; MOUSE

(custom-set-variables
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-yank-at-point t))

;;;;; PERFORMANCE

(add-hook 'focus-out-hook 'garbage-collect)

;;;;; PYTHON

(custom-set-variables
 '(python-indent-guess-indent-offset-verbose nil))

;;;;; SERVER

(require 'server)

(unless (server-running-p)
  (server-start))

;;;;; STARTUP

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-scratch-message ""))

(defun display-startup-echo-area-message ()
  (message "Emacs started in %.2f seconds triggering the GC %d times taking %.2f seconds"
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done gc-elapsed))

;;;;; USER INTERFACE

(custom-set-variables
 '(confirm-kill-emacs 'y-or-n-p)
 '(disabled-command-function nil)
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

;; use nice ellipses for collapsed portions of text, e.g., in outlines
(set-display-table-slot standard-display-table 'selective-display (string-to-vector "…"))

(add-hook 'text-mode-hook 'visual-line-mode)

(add-to-list 'default-frame-alist '(undecorated . t))

(windmove-default-keybindings)

;;;;; WINDOW DIVIDERS

(custom-set-variables
 '(window-divider-default-bottom-width theme-divider-width)
 '(window-divider-default-places t)
 '(window-divider-default-right-width theme-divider-width)
 '(window-divider-mode t))

(custom-set-faces
 `(window-divider             ((t (:foreground ,my/color-level-3))))
 `(window-divider-first-pixel ((t (:foreground ,my/color-level-3))))
 `(window-divider-last-pixel  ((t (:foreground ,my/color-level-3))))
 `(internal-border            ((t (:background ,my/color-level-3)))))

(add-to-list 'default-frame-alist `(internal-border-width . ,theme-divider-width))

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

(keymap-global-set "s-w" 'whitespace-mode)
(keymap-global-set "s-W" 'my/whitespace-cleanup-mode)

;;;;; XREF

;; mostly used with dumb-jump
(custom-set-variables
 '(xref-show-definitions-function 'xref-show-definitions-completing-read)
 '(xref-show-xrefs-function 'xref-show-definitions-completing-read))

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

(keymap-global-set "s-u" 'my/upgrade-packages)

;;;;; ADAPTIVE WRAP

(my/install 'adaptive-wrap)

(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

;;;;; AVY

(my/install 'avy)

(custom-set-variables
 '(avy-timeout-seconds 3))

(keymap-global-set "s-<return>" 'avy-goto-char-timer)

;;;;; CONSULT

(my/install 'consult)

(defun my/consult-line-dwim ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun my/consult-ripgrep-dwim ()
  (interactive)
  (consult-ripgrep
   (or (and (fboundp 'projectile-project-root) (projectile-project-root))
       default-directory)
   (thing-at-point 'symbol)))

(keymap-global-set "C-x r j" 'consult-register)
(keymap-global-set "M-y" 'consult-yank-pop)
(keymap-global-set "s--" 'consult-focus-lines)
(keymap-global-set "s-G" 'my/consult-ripgrep-dwim)
(keymap-global-set "s-I" 'consult-outline)
(keymap-global-set "s-S" 'my/consult-line-dwim)
(keymap-global-set "s-b" 'consult-project-buffer)
(keymap-global-set "s-g" 'consult-ripgrep)
(keymap-global-set "s-i" 'consult-imenu)
(keymap-global-set "s-l" 'consult-goto-line)
(keymap-global-set "s-s" 'consult-line)

;; add ripgrep arguments (see RIPGREP)
(with-eval-after-load 'consult
  (let ((default (custom-reevaluate-setting 'consult-ripgrep-args)))
    (custom-set-variables
     `(consult-ripgrep-args '(,default "--hidden" "--binary" "--glob !.git/")))))

;;;;; CORFU

(my/install 'corfu)

(custom-set-variables
 '(corfu-auto t)
 '(corfu-auto-delay 0.1)
 '(corfu-auto-prefix 2)
 '(corfu-bar-width 5)
 '(corfu-quit-at-boundary nil)
 '(corfu-quit-no-match t)
 '(corfu-separator ?\s)
 '(global-corfu-mode t))

(custom-set-faces
 `(corfu-bar ((t (:background ,my/color-accent))))
 `(corfu-current ((t (:inherit (highlight))))))

;;;;;; CAPE

(my/install 'cape)

(add-to-list 'completion-at-point-functions 'cape-dabbrev)
(add-to-list 'completion-at-point-functions 'cape-file)

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

;;;;; DOCKERFILE-MODE

(my/install 'dockerfile-mode)

;;;;; DUMB-JUMP

(my/install 'dumb-jump)

(custom-set-variables
 '(dumb-jump-force-searcher 'rg)) ; XXX this is apparently needed

(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

;; XXX disable js-mode native `jsfind-symbol'
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))

;;;;; EXPAND REGION

(my/install 'expand-region)

(keymap-global-set "S-s-<up>" 'er/expand-region)
(keymap-global-set "S-s-<down>" 'er/contract-region)

;;;;; GO-MODE

(my/install 'go-mode)

;;;;; MAGIT

(my/install 'magit)

(custom-set-variables
 '(magit-buffer-name-format "*%M*")
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(magit-section-initial-visibility-alist '((untracked . show) (stashes . hide) (unpushed . show))))

(custom-set-faces
 `(git-commit-overlong-summary ((t (:inherit (error) :inverse-video t))))
 `(magit-header-line ((t (:inherit (header-line))))))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
(keymap-global-set "s-m" 'magit-status)

;;;;; MARGINALIA

(my/install 'marginalia)

(custom-set-variables
 '(marginalia-mode t))

;;;;; MARKDOWN-MODE

(my/install 'markdown-mode)

(custom-set-variables
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-hide-urls t)
 '(markdown-url-compose-char "…"))

(custom-set-faces
 `(markdown-code-face           ((t (:background ,my/color-level-2 :extend t))))
 `(markdown-header-face-1       ((t (:inherit (outline-1)))))
 `(markdown-header-face-2       ((t (:inherit (outline-2)))))
 `(markdown-header-face-3       ((t (:inherit (outline-3)))))
 `(markdown-header-face-4       ((t (:inherit (outline-4)))))
 `(markdown-header-face-5       ((t (:inherit (outline-5)))))
 `(markdown-header-face-6       ((t (:inherit (outline-6)))))
 `(markdown-inline-code-face    ((t (:inherit (shadow)))))
 `(markdown-metadata-value-face ((t (:inherit (default))))))

;;;;;; EDIT-INDIRECT

;; allow to edit code blocks natively
(my/install 'edit-indirect)

;;;;; ORDERLESS

;; used by vertico and corfu

(my/install 'orderless)

(custom-set-variables
 '(completion-category-defaults nil)
 '(completion-styles '(orderless))
 '(orderless-match-faces [completions-common-part]))

;;;;; OUTSHINE

(my/install 'outshine)

;;;;; PROJECTILE

(my/install 'projectile)

(setq my/projectile-window-configurations (make-hash-table :test 'equal))

(defun my/projectile-save-window-configuration ()
  "Save the current window configuration for the current project."
  (when (projectile-project-p)
    ;; XXX it's apparently needed to manually update the list...
    (projectile-add-known-project (projectile-project-root))
    (puthash (projectile-project-root)
             (cons (current-window-configuration) (point-marker))
             my/projectile-window-configurations)))

(defun my/projectile-restore-window-configuration ()
  "Restore the window configuration or start anew."
  (let ((configuration (gethash (projectile-project-root) my/projectile-window-configurations)))
    (if configuration
        (progn
          (set-window-configuration (car configuration))
          (goto-char (cdr configuration)))
      (delete-other-windows))))

(defun my/projectile-open (filename)
  "Switch to a project by selecting one of its files or open a new file using a new window configuration."
  (interactive "fFind file: ")
  (if (and (projectile-project-p filename)
           (gethash (projectile-project-root filename) my/projectile-window-configurations))
      (progn (projectile-switch-project-by-name filename)
             (find-file filename))
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
  (define-key projectile-mode-map (kbd "s-K") 'projectile-remove-known-project)
  (define-key projectile-mode-map (kbd "s-P") 'my/projectile-open)
  (define-key projectile-mode-map (kbd "s-d") 'projectile-find-dir)
  (define-key projectile-mode-map (kbd "s-f") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "s-k") 'projectile-kill-buffers)
  (define-key projectile-mode-map (kbd "s-o") 'projectile-ripgrep)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-switch-project)
  (define-key projectile-mode-map (kbd "s-t") 'projectile-run-vterm))

;;;;; RAINBOW-MODE

(my/install 'rainbow-mode)

;;;;; RIPGREP

(my/install 'ripgrep)

(custom-set-variables
 '(ripgrep-arguments '("--hidden" "--binary" "--glob !.git/")))

(defun my/ripgrep-start-fix (process)
  "Enforce a cleaner ripgrep output by hiding the command."
  (when (derived-mode-p 'ripgrep-search-mode)
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (forward-line 3)
        (kill-whole-line)))))

(add-hook 'compilation-start-hook 'my/ripgrep-start-fix)

;;;;; VERTICO

(my/install 'vertico)

(custom-set-variables
 '(vertico-mode t))

;; vertico-repeat
(add-to-list 'savehist-additional-variables 'vertico-repeat-history)
(add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
(keymap-global-set "s-r" 'vertico-repeat)

;;;;; VTERM

;; XXX the system command libtool to be named glibtool in order to compile on macOS

(my/install 'vterm)

(custom-set-variables
 '(vterm-kill-buffer-on-exit nil))

(keymap-global-set "s-!" 'vterm)

;;;;; WINUM

(my/install 'winum)

(custom-set-variables
 '(winum-auto-setup-mode-line nil)
 '(winum-mode t)
 '(winum-scope 'frame-local))

(defun my/winum-select-window-by-number (n)
  "Select a window by its number or switch back to the most recently used one."
  (lambda ()
    (interactive)
    (if (= n (winum-get-number))
        (my/select-mru-window)
      (winum-select-window-by-number n))))

(defun my/select-mru-window ()
  "Select the most recently used window."
  (interactive)
  (let ((mru-window (get-mru-window nil t t)))
    (when mru-window
      (select-window mru-window))))

(keymap-global-set "s-1" (my/winum-select-window-by-number 1))
(keymap-global-set "s-2" (my/winum-select-window-by-number 2))
(keymap-global-set "s-3" (my/winum-select-window-by-number 3))
(keymap-global-set "s-4" (my/winum-select-window-by-number 4))
(keymap-global-set "s-5" (my/winum-select-window-by-number 5))
(keymap-global-set "s-6" (my/winum-select-window-by-number 6))
(keymap-global-set "s-7" (my/winum-select-window-by-number 7))
(keymap-global-set "s-8" (my/winum-select-window-by-number 8))
(keymap-global-set "s-9" (my/winum-select-window-by-number 9))
(keymap-global-set "s-0" (my/winum-select-window-by-number 10))
(keymap-global-set "s-\\" 'my/select-mru-window)

;;;;; YAML-MODE

(my/install 'yaml-mode)

;;;;; ZOOM

(my/install 'zoom)

(custom-set-variables
 '(zoom-mode t)
 '(zoom-size '(120 . 30))
 '(temp-buffer-resize-mode t))

;;; ADDITIONAL INITIALIZATION

(let ((local "~/.emacs.d/local.el"))
  (when (file-exists-p local)
    (load-file local)))

;;; OUTRO

;; restore the original garbage collector value
(custom-reevaluate-setting 'gc-cons-threshold)

;;; FILE VARIABLES

;; Local Variables:
;; eval: (rainbow-mode)
;; eval: (outshine-mode)
;; End:
