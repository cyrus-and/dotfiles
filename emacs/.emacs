;;; META

;; This is a standalone Emacs initialization file. Best viewed with
;; `outshine-mode'.

;; Apart from a few exceptions, default keybindings are left unchanged, rather
;; some shortcuts in the `C-c <char>' format have been added, list them with
;; `C-c ?'.

;;; CHEATSHEET

;; % m           `dired-mark-files-regexp'
;; C-s C-w       `isearch-yank-word-or-char' (M-e to edit)
;; C-u C-SPC     `pop-mark'
;; C-x -         `shrink-window-if-larger-than-buffer'
;; C-x 8 RET     `insert-char'
;; C-x C-;       `comment-line'
;; C-x C-SPC     `pop-global-mark'
;; C-x C-b       `list-buffers'
;; C-x C-j       `dired-jump'
;; C-x C-q       `wdired-change-to-wdired-mode'
;; C-x M-:       `repeat-complex-command'
;; C-x SPC       `rectangle-mark-mode'
;; C-x TAB       `indent-rigidly'`
;; C-x d         `dired' (useful for applying filters, e.g., *.c)
;; C-x n n       `narrow-to-region'
;; C-x n w       `widen'
;; C-x r SPC     `point-to-register'
;; C-x r i       `insert-register'
;; C-x r s       `copy-to-register'
;; C-x r w       `window-configuration-to-register'
;; C-x t 0       `tab-close'
;; C-x t 1       `tab-close-other'
;; C-x t 2       `tab-new'
;; C-x t RET     `tab-bar-select-tab-by-name'
;; C-x t r       `tab-rename'
;; C-x w r       `unhighlight-regexp'
;; C-x z         `repeat'
;; M-/           `dabbrev-expand'
;; M-^           `delete-indentation'
;; M-g M-n       `next-error'
;; M-g M-p       `previous-error'
;; M-h           `mark-paragraph'
;; M-s h .       `highlight-symbol-at-point'
;; M-s h r       `highlight-regexp'
;; M-s o         `occur' (specify the context with C-u; edit with e)
;; M-x ffap RET  `find-file-at-point'
;; M-z           `zap-to-char'
;; M-|           `shell-command-on-region' (replace the region with C-u)

;;; PERFORMANCE

;;;; GARBAGE COLLECTION

;; call the garbage collector less often during the startup then restore the
;; initial value to avoid longer pauses during the interactive usage (note that
;; `custom-reevaluate-setting' only works as long as the value has not been
;; customized)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (custom-reevaluate-setting 'gc-cons-threshold)))

;; force the garbage collection to happen when the focus moves away from emacs
(add-hook 'focus-out-hook 'garbage-collect)

;;;; COALESCE VARIABLE CUSTOMIZATION

;; each `custom-set-variables' call introduce some overhead so this override
;; collects all the customizations in a list which is then applied just once
;; after the initialization
(setq my/variables nil)

(defun my/custom-set-variables (&rest args)
  (setq my/variables (nconc my/variables args)))

(defun my/apply-customizations ()
  (advice-remove 'custom-set-variables 'my/custom-set-variables)
  (apply 'custom-set-variables my/variables)
  (makunbound 'my/variables))

;; apply the override during the initialization then restore and call
;; `custom-set-variables' just once
(advice-add 'custom-set-variables :override 'my/custom-set-variables)
(add-hook 'after-init-hook 'my/apply-customizations)

;;; PACKAGES

;; prevent the customization interface from altering this file
(setq custom-file "/dev/null")

;; utility to install a package refreshing the packet list just once
(defun my/install (package)
  (unless (package-installed-p package)
    (unless (bound-and-true-p my/install-refreshed)
      (package-refresh-contents)
      (setq my/install-refreshed t))
    (package-install package)))

;; XXX temporary hackish solution for https://debbugs.gnu.org/34341
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; add MELPA archive
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(global-set-key (kbd "C-c p") 'package-list-packages)

;;;; UPGRADE UTILITY

(defun my/upgrade ()
  "Upgrade and clean up packages."
  (interactive)
  (with-current-buffer (package-list-packages t)
    (package-refresh-contents)
    (package-menu-mark-upgrades)
    (ignore-errors (package-menu-execute t))
    (package-autoremove)
    (kill-buffer)))

(global-set-key (kbd "C-c P") 'my/upgrade)

;;;; UTILITIES

;; utility to defer slow operation so to not directly impact init time
(defmacro my/defer (body)
  `(run-with-idle-timer
    0.5 nil (lambda () ,body)))

;;; THEME

;; required for styling buttons
(require 'cus-edit)

;;;; CONSTANTS

;; Some colors from from https://terminal.sexy

;; UI and base colors
(setq theme-color-accent   "#ff6000")
(setq theme-color-level-1  "#1D1F21")
(setq theme-color-level-2  "#373B41")
(setq theme-color-level-3  "#C5C8C6")

;; common colors
(setq theme-red     "#A54242")
(setq theme-green   "#8C9440")
(setq theme-yellow  "#DE935F")
(setq theme-blue    "#5F819D")
(setq theme-magenta "#85678F")
(setq theme-cyan    "#5E8D87")
(setq theme-gray    "#707880")

;; theme parameters
(setq theme-divider-width   15)
(setq theme-font-linux      "Terminus")
(setq theme-font-macos      "Iosevka SS04")
(setq theme-font-size-linux 14)
(setq theme-font-size-macos 16)

;;;; THEME VARIABLES

;; window dividers
(add-to-list 'default-frame-alist `(internal-border-width . ,theme-divider-width))
(custom-set-variables
 '(window-divider-mode t)
 '(window-divider-default-places t)
 '(window-divider-default-bottom-width theme-divider-width)
 '(window-divider-default-right-width theme-divider-width))

;; widget and custom button coherence
(custom-set-variables
 '(custom-raised-buttons t) ; for terminal mode
 '(widget-mouse-face 'custom-button-mouse))

;; use no widgets marks
(custom-set-variables
 '(widget-push-button-prefix " ")
 '(widget-push-button-suffix " ")
 '(widget-link-prefix " ")
 '(widget-link-suffix " "))

;;;; THEME FACES

;; basic faces
(custom-set-faces
 `(default        ((t (:foreground ,theme-color-level-3 :background ,theme-color-level-1))))
 `(shadow         ((t (:foreground ,theme-color-level-2))))
 `(link           ((t (:foreground ,theme-color-accent))))
 `(link-visited   ((t (:inherit (link) :weight normal))))
 `(highlight      ((t (:foreground ,theme-color-level-1 :background ,theme-color-accent))))
 `(isearch        ((t (:foreground ,theme-color-level-1 :background ,theme-color-accent))))
 `(lazy-highlight ((t (:foreground ,theme-color-level-1 :background ,theme-color-level-3))))
 `(error          ((t (:foreground ,theme-red))))
 `(warning        ((t (:foreground ,theme-yellow))))
 `(success        ((t (:foreground ,theme-green)))))

;; header/mode line
(custom-set-faces
 `(mode-line           ((t (:foreground ,theme-color-level-1 :background ,theme-color-level-3))))
 `(mode-line-inactive  ((t (:inherit (mode-line)))))
 `(mode-line-highlight ((t (:background ,theme-color-accent))))
 `(header-line         ((t (:inherit (mode-line))))))

;; window dividers
(custom-set-faces
 `(window-divider             ((t (:foreground ,theme-color-level-2))))
 `(window-divider-first-pixel ((t (:foreground ,theme-color-level-2))))
 `(window-divider-last-pixel  ((t (:foreground ,theme-color-level-2))))
 `(internal-border            ((t (:background ,theme-color-level-2)))))

;; font lock
(custom-set-faces
 `(font-lock-function-name-face ((t (:inherit (bold) :foreground ,theme-magenta))))
 `(font-lock-variable-name-face ((t (:foreground ,theme-yellow))))
 `(font-lock-keyword-face       ((t (:inherit (bold) :foreground ,theme-red))))
 `(font-lock-comment-face       ((t (:foreground ,theme-gray))))
 `(font-lock-type-face          ((t (:foreground ,theme-blue))))
 `(font-lock-constant-face      ((t (:foreground ,theme-cyan))))
 `(font-lock-builtin-face       ((t (:foreground ,theme-cyan))))
 `(font-lock-string-face        ((t (:foreground ,theme-green))))
 `(font-lock-negation-char-face ((t (:inherit (default bold))))))

;; highlighting lock
(custom-set-faces
 `(hi-black-b  ((t (:inherit (bold) :foreground ,theme-color-level-1 :background ,theme-gray))))
 `(hi-black-hb ((t (:inherit (bold) :foreground ,theme-color-level-3 :background ,theme-gray))))
 `(hi-blue     ((t (:foreground ,theme-color-level-1 :background ,theme-blue))))
 `(hi-blue-b   ((t (:inherit (hi-blue bold) :inverse-video t))))
 `(hi-green    ((t (:foreground ,theme-color-level-1 :background ,theme-green))))
 `(hi-green-b  ((t (:inherit (hi-green bold) :inverse-video t))))
 `(hi-pink     ((t (:foreground ,theme-color-level-1 :background ,theme-magenta))))
 `(hi-red-b    ((t (:inherit (bold) :foreground ,theme-red))))
 `(hi-yellow   ((t (:foreground ,theme-color-level-1 :background ,theme-yellow)))))

;; compilation
(custom-set-faces
 '(compilation-mode-line-exit ((t (:inherit (success)))))
 '(compilation-mode-line-run  ((t (:inherit (warning)))))
 '(compilation-mode-line-fail ((t (:inherit (error))))))

;; widgets
(custom-set-faces
 `(custom-button         ((t (:foreground ,theme-color-level-3 :background ,theme-color-level-2 :box (:line-width 2 :color nil :style released-button)))))
 `(custom-button-pressed ((t (:inherit (custom-button-mouse) :box (:line-width 2 :color nil :style pressed-button)))))
 `(custom-button-mouse   ((t (:inherit (custom-button) :foreground ,theme-color-accent))))
 `(widget-button         ((t (:inherit (custom-button)))))
 `(widget-button-pressed ((t (:inherit (custom-button-pressed)))))
 `(widget-field          ((t (:foreground ,theme-color-level-3 :background ,theme-color-level-2 :extend t)))))

;; outlines
(custom-set-faces
 `(outline-1 ((t (:inherit (bold) :extend t :height 1.4 :foreground ,theme-blue))))
 `(outline-2 ((t (:inherit (bold) :extend t :height 1.2 :foreground ,theme-yellow))))
 `(outline-3 ((t (:inherit (bold) :extend t :height 1.2 :foreground ,theme-green))))
 `(outline-4 ((t (:inherit (bold) :extend t :height 1.2 :foreground ,theme-magenta))))
 `(outline-5 ((t (:inherit (bold) :extend t :height 1.2 :foreground ,theme-red))))
 `(outline-6 ((t (:inherit (bold) :extend t :height 1.0 :foreground ,theme-red))))
 `(outline-7 ((t (:inherit (bold) :extend t :height 1.0 :foreground ,theme-red))))
 `(outline-8 ((t (:inherit (bold) :extend t :height 1.0 :foreground ,theme-red)))))

;; others
(custom-set-faces
 `(cursor                       ((t (:background ,theme-color-level-3))))
 `(fringe                       ((t (:inherit (default)))))
 `(minibuffer-prompt            ((t (:foreground ,theme-color-accent :weight bold))))
 `(region                       ((t (:foreground ,theme-color-level-1 :background ,theme-color-level-2 :extend t))))
 `(secondary-selection          ((t (:foreground ,theme-color-level-3 :background ,theme-color-level-2 :extend t))))
 `(isearch-fail                 ((t (:inherit (error)))))
 `(completions-common-part      ((t (:inherit (shadow)))))
 `(completions-first-difference ((t (:foreground ,theme-color-accent))))
 `(pulse-highlight-start-face   ((t (:background ,theme-color-accent))))
 `(show-paren-match             ((t (:inherit (bold) :foreground ,theme-color-accent))))
 `(show-paren-mismatch          ((t (:inherit (error) :inverse-video t)))))

;;; CONFIGURATIONS

;;;; ADAPTIVE WRAP

;; nicer `visual-line-mode'

(my/install 'adaptive-wrap)

(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

;;;; BACKUPS

(custom-set-variables
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups"))))

;;;; BASIC EDITING

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
     (inlambda . 0)
     (innamespace . 0))))

;;;; CODE ANNOTATIONS

;; highlight custom keyword in all basic modes
(setq my/todo-regexp (rx bow (or "TODO" "XXX") eow))
(setq my/todo-face 'font-lock-warning-face)

(defun my/todo-fontlock-hook ()
  (font-lock-add-keywords nil `((,my/todo-regexp 0 ,my/todo-face prepend)) t))

(add-hook 'text-mode-hook 'my/todo-fontlock-hook)
(add-hook 'prog-mode-hook 'my/todo-fontlock-hook)

;; provide an utility to list the annotations of the current buffer
(defun my/todo-occur ()
  (interactive)
  (occur my/todo-regexp))

(global-set-key (kbd "C-c t") 'my/todo-occur)

;;;; COMB

(my/install 'comb)

;; customize the keybindings
(with-eval-after-load 'comb
  (define-key comb-keymap (kbd "RET") 'comb-approve-next)
  (define-key comb-keymap (kbd "DEL") 'comb-reject-next)
  (define-key comb-keymap (kbd "SPC") 'comb-undecide-next))

;;;; COMPANY

(my/install 'company)
(my/install 'company-posframe)

(custom-set-variables
 '(global-company-mode t)
 '(company-idle-delay 0.1)
 '(company-show-numbers t)
 '(company-minimum-prefix-length 2)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil))

(custom-set-variables
 `(company-posframe-mode ,(not (eq system-type 'darwin))) ; https://github.com/tumashu/posframe/issues/30
 '(company-posframe-quickhelp-delay nil)
 '(company-posframe-show-indicator nil)
 '(company-posframe-show-metadata nil))

(custom-set-faces
 `(company-tooltip            ((t (:foreground ,theme-color-level-3 :background ,theme-color-level-2 ))))
 `(company-tooltip-common     ((t (:inherit (lazy-highlight)))))
 `(company-tooltip-selection  ((t (:foreground ,theme-color-level-1 :background ,theme-color-accent))))
 `(company-tooltip-mouse      ((t (:inherit (company-tooltip-selection)))))
 `(company-tooltip-annotation ((t (:foreground ,theme-color-level-3))))
 `(company-scrollbar-bg       ((t (:background ,theme-color-level-2))))
 `(company-scrollbar-fg       ((t (:background ,theme-color-level-3)))))

;;;; COMPILATION

(custom-set-variables
 '(compile-command "make")
 '(compilation-always-kill t)
 '(compilation-disable-input t)
 '(compilation-scroll-output 'first-error))

(add-hook 'compilation-mode-hook 'visual-line-mode)

;;;;; AUTO QUIT

(defun my/compile-auto-kill (buffer status)
  "Run after compilation and kill the window if needed."
  (let ((window (get-buffer-window buffer)))
    (when (and (bound-and-true-p my/compile-should-kill)
               ;; only for *compilation* buffers (do not kill grep and similar)
               (equal (buffer-name buffer) "*compilation*")
               ;; a window to kill must exist
               window
               ;; status must be success
               (equal status "finished\n")
               ;; there must not be any additional information
               (with-current-buffer buffer
                 (zerop (+ compilation-num-errors-found
                           compilation-num-warnings-found
                           compilation-num-infos-found))))
      (run-at-time 1 nil 'quit-window nil window))))

(defun my/compile-before (&rest ignore)
  "Determine if the next compilation should auto quit the compilation window."
  (let* ((buffer (get-buffer "*compilation*"))
         (window (get-buffer-window buffer)))
    (setq my/compile-should-kill (not (and buffer window)))))

(advice-add 'compile :before 'my/compile-before)
(advice-add 'recompile :before 'my/compile-before)

(add-hook 'compilation-finish-functions 'my/compile-auto-kill)

;;;;; SMART COMPILE

(defun my/smart-compile (arg)
  "Recompile or prompt a new compilation.

If prefix ARG is given, simply call `compile'."
  (interactive "P")
  ;; if a prefix argument is present prompt the compilation command
  ;; otherwise try to fetch the command from the buffer-local variable
  (if arg
      ;; do not propagate the prefix argument
      (let ((current-prefix-arg))
        (call-interactively 'compile))
    ;; reload safe variables silently
    (let ((enable-local-variables :safe))
      (hack-local-variables))
    ;; compile using the local value of `compile-command' or seek for the
    ;; default compilation buffer
    (if (local-variable-p 'compile-command)
        (recompile)
      (let ((buffer (get-buffer "*compilation*")))
        ;; recompile the compilation buffer or prompt the compilation command
        (if buffer
            (with-current-buffer buffer
              (recompile))
          (call-interactively 'compile))))))

(global-set-key (kbd "C-c c") 'my/smart-compile)

;;;; CURSOR

(custom-set-variables
 '(blink-cursor-mode nil)
 '(cursor-in-non-selected-windows nil))

;;;; DIFF-HL

(my/install 'diff-hl)

(custom-set-variables
 '(global-diff-hl-mode t)
 '(diff-hl-show-staged-changes nil)
 '(diff-hl-draw-borders nil))

(custom-set-faces
 '(diff-hl-change ((t (:inherit (warning) :inverse-video t))))
 '(diff-hl-insert ((t (:inherit (success) :inverse-video t))))
 '(diff-hl-delete ((t (:inherit (error) :inverse-video t)))))

;; enable annotations for dired buffers too
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; update the indicators also after a commit
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;;;; DIRED

;; use the native ls implementation to be consistent even on macOS and allow to
;; omit hidden files
(with-eval-after-load 'dired
  (require 'ls-lisp)
  (require 'dired-x))

(custom-set-variables
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity '(uid gid))
 '(ls-lisp-dirs-first t)
 '(dired-omit-files "^\\."))

;;;; EASY REVERT BUFFER

;; utility to abruptly discard changes in the current buffer

(defun my/force-revert-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(global-set-key (kbd "C-c R") 'my/force-revert-buffer)

;;;; ERC

(my/install 'password-store)

;; disable hard fill module
(with-eval-after-load 'erc
  (delete 'fill erc-modules)
  (erc-update-modules))

;; timestamp always visible
(custom-set-variables
 '(erc-hide-list '("JOIN" "PART" "QUIT"))
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-timestamp-format "[%H:%M] ")
 '(erc-timestamp-only-if-changed-flag nil))

;; make track mode less noisy and add the indicator at the end of the mode line
;; to not interfere with minions
(custom-set-variables
 '(erc-track-exclude-types '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE"))
 '(erc-track-position-in-mode-line t))

;; autojoin
(custom-set-variables
 '(erc-autojoin-channels-alist '(("freenode.net$" . ("#emacs")))))

(custom-set-faces
 `(erc-prompt-face    ((t (:inherit (minibuffer-prompt)))))
 `(erc-timestamp-face ((t (:inherit (shadow)))))
 `(erc-notice-face    ((t (:inherit (shadow bold))))))

;; disable automatic point recentering so that the prompt stays still and enable
;; visual line wrapping
(add-hook 'erc-mode-hook 'my/erc-setup)
(defun my/erc-setup ()
  (visual-line-mode)
  (set (make-local-variable 'scroll-conservatively) 101))

;; automatic freenode.net connection (a pinentry program must be available)
(defun my/irc ()
  (interactive)
  (let* ((credentials (split-string (password-store-get "Freenode")))
         (nick (nth 0 credentials))
         (password (nth 1 credentials)))
    (erc
     :server "irc.freenode.net"
     :port 6667
     :nick nick
     :password password)))

(global-set-key (kbd "C-c i") 'my/irc)

;;;; ERROR NAVIGATION

(global-set-key (kbd "<M-S-up>") 'previous-error)
(global-set-key (kbd "<M-S-down>") 'next-error)

;;;; FIND

;; find in whole path
(custom-set-variables
 '(find-name-arg "-path"))

(global-set-key (kbd "C-c f") 'find-name-dired)

;;;; GREP

;; exclude Node.js folders
(with-eval-after-load 'grep
  (add-to-list 'grep-find-ignored-directories "node_modules"))

;;;;; HIDE THE GREP COMMAND

;; use a cleaner grep output by hiding the command
(add-hook 'grep-setup-hook 'my/grep-fix)
(defun my/grep-fix ()
  (save-excursion
    (let ((inhibit-read-only t))
      (forward-line 3) ; kill 4th line
      (kill-whole-line))))

;;;;; SMART GREP

;; call rgrep in the same conditions if there is already an rgrep buffer alive
(defun my/rgrep ()
  (interactive)
  (if (and (boundp 'grep-last-buffer)
           (buffer-live-p grep-last-buffer))
      (rgrep
       (grep-read-regexp)
       (car grep-files-history)
       (with-current-buffer grep-last-buffer default-directory))
    (call-interactively 'rgrep)))

(global-set-key (kbd "C-c g") 'my/rgrep)

;;;; IBUFFER

;; avoid asking confirmation
(custom-set-variables
 '(ibuffer-expert t))

;; override `list-buffers'
(defalias 'list-buffers 'ibuffer)

;;;; IMENU-LIST

(my/install 'imenu-list)

(custom-set-faces
 `(imenu-list-entry-face-0 ((t (:inherit (outline-1) :height 0.75))))
 `(imenu-list-entry-face-1 ((t (:inherit (outline-2) :height 0.75))))
 `(imenu-list-entry-face-2 ((t (:inherit (outline-3) :height 0.75))))
 `(imenu-list-entry-face-3 ((t (:inherit (outline-4) :height 0.75)))))

(global-set-key (kbd "C-c l") 'imenu-list-smart-toggle)

;;;;; Use fixed size

;; this helps with Zoom

(defun my/fix-imenu-size ()
  (with-selected-window (get-buffer-window "*Ilist*")
    (setq window-size-fixed t)
    (window-resize (selected-window) (- 30 (window-total-width)) t t)))

(add-hook 'imenu-list-update-hook 'my/fix-imenu-size)

;;;; INITIALIZATION

(custom-set-variables
 '(initial-scratch-message "")
 '(inhibit-startup-screen t))

;; show some performance stats
(defun display-startup-echo-area-message ()
  (message "Emacs started in %.2f seconds triggering the GC %d times taking %.2f seconds"
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done gc-elapsed))

;;;; ISEARCH

;; inhibit search/replace on invisible text
(custom-set-variables
 '(isearch-allow-scroll t)
 '(search-invisible nil))

;;;; IVY + COUNSEL + SWIPER

(my/install 'ivy)
(my/install 'counsel) ; this will also install swiper

(custom-set-variables
 '(counsel-mode t)
 '(ivy-mode t)
 '(ivy-minibuffer-faces '(ivy-minibuffer-match-face-1 ivy-minibuffer-match-face-2)))

(custom-set-faces
 `(ivy-highlight-face             ((t ())))
 `(ivy-current-match              ((t (:background ,theme-color-accent :foreground ,theme-color-level-1))))
 `(ivy-minibuffer-match-face-1    ((t ())))
 `(ivy-minibuffer-match-face-2    ((t (:inherit (lazy-highlight)))))
 `(swiper-line-face               ((t (:inherit (ivy-current-match)))))
 `(swiper-background-match-face-1 ((t (:inherit (lazy-highlight)))))
 `(swiper-background-match-face-2 ((t (:inherit (swiper-background-match-face-1)))))
 `(swiper-background-match-face-3 ((t (:inherit (swiper-background-match-face-1)))))
 `(swiper-background-match-face-4 ((t (:inherit (swiper-background-match-face-1)))))
 `(swiper-match-face-1            ((t (:inherit (isearch)))))
 `(swiper-match-face-2            ((t (:inherit (swiper-match-face-1)))))
 `(swiper-match-face-3            ((t (:inherit (swiper-match-face-1)))))
 `(swiper-match-face-4            ((t (:inherit (swiper-match-face-1))))))

;; prefer shorter matches
(with-eval-after-load 'ivy
  (add-to-list 'ivy-sort-matches-functions-alist '(t . ivy--shorter-matches-first)))

;; override isearch
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper-backward)

(global-set-key (kbd "C-c j") 'counsel-file-jump)

;;;; JAVASCRIPT

(my/install 'js2-mode)

(custom-set-variables
 '(js2-include-node-externs t)
 '(js2-skip-preprocessor-directives t))

(custom-set-faces
 `(js2-function-param         ((t (:inherit (font-lock-variable-name-face)))))
 `(js2-function-call          ((t (:inherit (font-lock-function-name-face)))))
 `(js2-object-property        ((t (:inherit (font-lock-variable-name-face)))))
 `(js2-object-property-access ((t (:inherit (default))))))

;; associate by file name and shebang
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;; LINUX SPECIFIC

(when (eq system-type 'gnu/linux)
  ;; create a GTK configuration file that matches the theme color to avoid
  ;; glitches but only if needed
  (let ((gtkrc "~/.emacs.d/gtkrc"))
    (when (file-newer-than-file-p load-file-name gtkrc)
      (mkdir "~/.emacs.d" t)
      (with-temp-file gtkrc
        (insert (format "style 'default' { bg[NORMAL] = '%s' }\n" theme-background))
        (insert "class 'GtkWidget' style 'default'\n"))))

  ;; setup base GUI to avoid glitches but only if needed
  (let ((xdefaults (format "~/.Xdefaults-%s" (system-name))))
    (when (file-newer-than-file-p load-file-name xdefaults)
      (with-temp-file xdefaults
        (insert (format "emacs.font: %s-%d\n" theme-font-linux theme-font-size-linux))
        (insert "emacs.menuBar: off\n")
        (insert "emacs.toolBar: off\n")
        (insert "emacs.verticalScrollBars: off\n")
        (insert (format "emacs.background: %s\n" theme-background)))
      ;; XXX workaround for emacs 27 that does not read .Xdefaults-hostname
      (mkdir "~/.Xdefaults" t)
      (copy-file xdefaults (format "~/.Xdefaults/%s" (system-name)) t))))

;;;; MACOS SPECIFIC

(when (eq system-type 'darwin)
  ;; use the right meta key natively so to allow typing fancy glyphs
  (custom-set-variables
   '(mac-right-option-modifier 'none))

  ;; tune the window decorations
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  ;; use the lightweight fullscreen mode (XXX for some reason this only works
  ;; the second time)
  (custom-set-variables
   '(ns-use-native-fullscreen nil)
   '(ns-use-fullscreen-animation nil))

  ;; fetch environment variables from shell (namely, those in ~/.profile since
  ;; it is not sourced by macOS but only by bash)
  (my/install 'exec-path-from-shell)
  (custom-set-variables
   '(exec-path-from-shell-warn-duration-millis most-positive-fixnum))
  (my/defer (exec-path-from-shell-copy-envs
             '("PATH" "PASSWORD_STORE_DIR" "NPM_CONFIG_PREFIX" "GEM_HOME" "PIP_USER")))

  ;; setup base GUI to avoid glitches but only if needed
  (let ((plist "~/Library/Preferences/org.gnu.Emacs.plist"))
    (when (file-newer-than-file-p load-file-name plist)
      (shell-command-to-string "defaults write org.gnu.Emacs ToolBar -bool false")
      (shell-command-to-string (format "defaults write org.gnu.Emacs Font \"%s\"-%d"
                                       theme-font-macos theme-font-size-macos))))

  ;; force GPG to use a GUI pinentry program (fetch pinentry-mac from brew) to
  ;; avoid problems
  (let ((gpg-agent-conf "~/.gnupg/gpg-agent.conf"))
    (when (file-newer-than-file-p load-file-name gpg-agent-conf)
      (mkdir "~/.gnupg" t)
      (with-temp-file gpg-agent-conf
        (insert "pinentry-program /usr/local/bin/pinentry-mac\n")))))

;;;; MAGIT

(my/install 'magit)

;; enable spell checking in commit editing buffers
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

;; set up default status sections visibility
(custom-set-variables
 '(magit-section-initial-visibility-alist
   '((stashes . show) (unpushed . show))))

;; make clearer the commit message overflow
(custom-set-faces
 `(git-commit-overlong-summary ((t (:inherit (error) :inverse-video t)))))

(global-set-key (kbd "C-c s") 'magit-status)

;;;; MARKDOWN

(my/install 'markdown-mode)
(my/install 'edit-indirect)

(custom-set-variables
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-hide-urls t)
 '(markdown-url-compose-char "…")
 '(markdown-asymmetric-header t))

(custom-set-faces
 `(markdown-code-face           ((t ())))
 `(markdown-inline-code-face    ((t (:foreground ,theme-gray))))
 `(markdown-metadata-value-face ((t (:inherit (default)))))
 `(markdown-header-face-1       ((t (:inherit (outline-1)))))
 `(markdown-header-face-2       ((t (:inherit (outline-2)))))
 `(markdown-header-face-3       ((t (:inherit (outline-3)))))
 `(markdown-header-face-4       ((t (:inherit (outline-4)))))
 `(markdown-header-face-5       ((t (:inherit (outline-5)))))
 `(markdown-header-face-6       ((t (:inherit (outline-6))))))

;; use nice ellipses (this also works for org mode)
(set-display-table-slot standard-display-table 'selective-display (string-to-vector "…"))

;;;; MINIBUFFER

;; infinite minibuffer history and case insensitive completion
(custom-set-variables
 '(savehist-mode t)
 '(history-length t)
 '(read-buffer-completion-ignore-case t))

;;;; MINIONS

(my/install 'minions)

(custom-set-variables
 '(minions-mode t)
 '(minions-mode-line-lighter "···")
 '(minions-direct '(overwrite-mode)))

;;;; MODE LINE

(custom-set-variables
 '(column-number-mode t)
 '(winum-mode-line-position 0)
 `(mode-line-format
   '(" "
     (:propertize "%Z%*%@" face (:foreground ,theme-color-level-2))
     "  "
     (:propertize
      (:eval (when (buffer-file-name)
               (let ((path (abbreviate-file-name default-directory)))
                 (if (> (length path) (/ (window-total-width) 2))
                     (replace-regexp-in-string "\\(/[^/]\\)[^/]*" "\\1" path)
                   path))))
      face shadow)
     (:propertize "%b" face bold)
     "  "
     (:propertize "+%l:%c" face (:foreground ,theme-color-level-2))
     "  "
     mode-line-modes
     " " ; XXX one extra space is already there
     global-mode-string)))

;;;; MOUSE

(custom-set-variables
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-yank-at-point t))

;;;; OPENWITH

;; this is especially useful to open PDF files directly from markdown

(my/install 'openwith)

(custom-set-variables
 '(openwith-mode t)
 `(openwith-associations '(("\\.pdf\\'" ,(if (eq system-type 'darwin) "open" "xdg-open") (file))))
 '(large-file-warning-threshold nil))

;;;; OTHER PACKAGES

(my/install 'dockerfile-mode)
(my/install 'go-mode)
(my/install 'rainbow-mode)
(my/install 'rust-mode)
(my/install 'yaml-mode)

;;;; OUTSHINE

;; this is basically only used for this file

(my/install 'outshine)

(custom-set-variables
 '(outshine-fontify-whole-heading-line t))

;; enable shift tab everywhere to be coherent with org and markdown modes
(with-eval-after-load 'outshine
  (outshine-define-key outshine-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer t))

;;;; PHP

(my/install 'php-mode)

(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

;;;; PROJECTILE

(my/install 'projectile)

(custom-set-variables
 '(projectile-mode t))

;; define the global entrypoint key
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

;;;; PYTHON

(custom-set-variables
 '(python-shell-interpreter "python3"))

;;;; SAVE PLACE

(custom-set-variables
 '(save-place-mode t))

;;;; SHELL

;; avoid showing the shell buffer output immediately for async commands and
;; allow more than one of them
(custom-set-variables
 '(async-shell-command-buffer 'new-buffer)
 '(async-shell-command-display-buffer nil))

(global-set-key (kbd "C-c a") 'shell)

;;;; TRAMP

;; avoid verbose tramp messages and show errors only
(custom-set-variables
 '(tramp-verbose 1))

;;;; USER INTERFACE

(windmove-default-keybindings)

(custom-set-variables
 '(disabled-command-function nil)
 '(echo-keystrokes 0.1)
 '(frame-resize-pixelwise t)
 '(help-window-select t)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(truncate-lines t)
 '(use-dialog-box nil)
 '(vc-follow-symlinks t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

;; visual line for text modes
(add-hook 'text-mode-hook 'visual-line-mode)

;; avoid suspend-frame in GUI mode
(advice-add 'iconify-or-deiconify-frame :before-until 'display-graphic-p)

;; disable menu in terminals anyway
(unless (display-graphic-p)
  (custom-set-variables
   '(menu-bar-mode nil)))

;;;; WHITESPACE MANAGEMENT

(custom-set-variables
 '(require-final-newline 'ask))

;;;;; TRIM WHITESPACE MODE

(defun my/trim-whitespace--handler ()
  "Delete trailing whitespaces if `my/trim-whitespace-mode' is enabled."
  (when my/trim-whitespace-mode
    (delete-trailing-whitespace)))

(define-minor-mode my/trim-whitespace-mode
  "Delete trailing whitespaces on save."
  :init-value t
  :lighter " W"
  (my/trim-whitespace--handler))

(add-hook 'before-save-hook 'my/trim-whitespace--handler)

(global-set-key (kbd "C-c d") 'my/trim-whitespace-mode)

;;;;; NORMALIZE WHITESPACE FOR DIRED BUFFERS

(defun my/dired-normalize-whitespace-marked-files ()
  "Normalize the whitespace in the currently marked dired files."
  (interactive)
  (let ((delete-trailing-lines t))
    (dolist (file (dired-get-marked-files))
      (with-current-buffer (find-file-literally file)
	    (goto-char (point-max))
	    (insert "\n")
        (delete-trailing-whitespace)
        (save-buffer)
        (kill-buffer))))
  (message "Done!"))

;;;; WINNER

(custom-set-variables
 '(winner-mode t))

;;;; WINUM

(my/install 'winum)

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

(custom-set-variables
 '(winum-mode t)
 '(winum-format (propertize " %s " 'face 'winum-face))
 '(winum-scope 'frame-local))

(custom-set-faces
 `(winum-face ((t (:foreground ,theme-color-level-1 :background ,theme-color-accent)))))

;;;; WOMAN

;; fill the whole frame on creation (refresh with `R`)
(custom-set-variables
 '(woman-fill-frame t))

(custom-set-faces
 `(woman-bold   ((t (:inherit (bold)))))
 `(woman-italic ((t (:inherit (italic) :foreground ,theme-green)))))

;;;; WRITEROOM

(my/install 'writeroom-mode)

(custom-set-variables
 '(writeroom-bottom-divider-width 0)
 '(writeroom-fullscreen-effect 'fullboth))

;;;; ZOOM

(my/install 'zoom)

;; use a bigger target size and resize temp buffers anyway
(custom-set-variables
 '(zoom-mode t)
 '(zoom-size '(120 . 30))
 '(temp-buffer-resize-mode t))

(global-set-key (kbd "C-c z") 'zoom-mode)

;;; FILE VARIABLES

;; Local Variables:
;; eval: (rainbow-mode)
;; eval: (outshine-mode)
;; End:
