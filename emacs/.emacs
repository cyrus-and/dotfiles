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

(global-set-key (kbd "C-c u") 'my/upgrade)

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
(setq theme-color-accent  "#ff6000")
(setq theme-color-low     "#000000")
(setq theme-color-high    "#ffffff")
(setq theme-color-level-1 "#1D1F21")
(setq theme-color-level-2 "#282A2E")
(setq theme-color-level-3 "#373B41")
(setq theme-color-level-4 "#C5C8C6")

;; common colors
(setq theme-color-red     "#A54242")
(setq theme-color-green   "#8C9440")
(setq theme-color-yellow  "#DE935F")
(setq theme-color-blue    "#5F819D")
(setq theme-color-magenta "#85678F")
(setq theme-color-cyan    "#5E8D87")
(setq theme-color-gray    "#707880")

;; theme parameters
(setq theme-divider-width   15)
(setq theme-font-linux      "Terminus")
(setq theme-font-macos      "Iosevka SS04")
(setq theme-font-size-linux 14)
(setq theme-font-size-macos 14)

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
 `(default        ((t (:foreground ,theme-color-level-4 :background ,theme-color-level-1))))
 `(link           ((t (:foreground ,theme-color-accent))))
 `(link-visited   ((t (:inherit (link) :weight normal))))
 `(highlight      ((t (:foreground ,theme-color-level-1 :background ,theme-color-level-4))))
 `(isearch        ((t (:foreground ,theme-color-level-1 :background ,theme-color-accent))))
 `(lazy-highlight ((t (:foreground ,theme-color-level-1 :background ,theme-color-level-4))))
 `(match          ((t (:inherit (lazy-highlight)))))
 `(shadow         ((t (:foreground ,theme-color-gray))))
 `(error          ((t (:foreground ,theme-color-red))))
 `(warning        ((t (:foreground ,theme-color-yellow))))
 `(success        ((t (:foreground ,theme-color-green)))))

;; header/mode line
(custom-set-faces
 `(mode-line           ((t (:foreground ,theme-color-level-1 :background ,theme-color-level-4))))
 `(mode-line-inactive  ((t (:inherit (mode-line)))))
 `(mode-line-highlight ((t (:background ,theme-color-accent))))
 `(header-line         ((t (:inherit (mode-line))))))

;; window dividers
(custom-set-faces
 `(window-divider             ((t (:foreground ,theme-color-level-3))))
 `(window-divider-first-pixel ((t (:foreground ,theme-color-level-3))))
 `(window-divider-last-pixel  ((t (:foreground ,theme-color-level-3))))
 `(internal-border            ((t (:background ,theme-color-level-3)))))

;; font lock
(custom-set-faces
 `(font-lock-function-name-face ((t (:inherit (bold) :foreground ,theme-color-magenta))))
 `(font-lock-variable-name-face ((t (:foreground ,theme-color-yellow))))
 `(font-lock-keyword-face       ((t (:inherit (bold) :foreground ,theme-color-red))))
 `(font-lock-comment-face       ((t (:foreground ,theme-color-gray))))
 `(font-lock-type-face          ((t (:foreground ,theme-color-blue))))
 `(font-lock-constant-face      ((t (:foreground ,theme-color-cyan))))
 `(font-lock-builtin-face       ((t (:foreground ,theme-color-cyan))))
 `(font-lock-string-face        ((t (:foreground ,theme-color-green))))
 `(font-lock-negation-char-face ((t (:inherit (default bold))))))

;; highlighting lock
(custom-set-faces
 `(hi-black-b  ((t (:inherit (bold) :foreground ,theme-color-level-1 :background ,theme-color-gray))))
 `(hi-black-hb ((t (:inherit (bold) :foreground ,theme-color-level-4 :background ,theme-color-gray))))
 `(hi-blue     ((t (:foreground ,theme-color-level-1 :background ,theme-color-blue))))
 `(hi-blue-b   ((t (:inherit (hi-blue bold) :inverse-video t))))
 `(hi-green    ((t (:foreground ,theme-color-level-1 :background ,theme-color-green))))
 `(hi-green-b  ((t (:inherit (hi-green bold) :inverse-video t))))
 `(hi-pink     ((t (:foreground ,theme-color-level-1 :background ,theme-color-magenta))))
 `(hi-red-b    ((t (:inherit (bold) :foreground ,theme-color-red))))
 `(hi-yellow   ((t (:foreground ,theme-color-level-1 :background ,theme-color-yellow)))))

;; compilation
(custom-set-faces
 '(compilation-mode-line-exit ((t (:inherit (success)))))
 '(compilation-mode-line-run  ((t (:inherit (warning)))))
 '(compilation-mode-line-fail ((t (:inherit (error))))))

;; widgets
(custom-set-faces
 `(custom-button         ((t (:foreground ,theme-color-level-1 :background ,theme-color-level-4 :box (:line-width 2 :color nil :style released-button)))))
 `(custom-button-pressed ((t (:inherit (custom-button-mouse) :box (:line-width 2 :color nil :style pressed-button)))))
 `(custom-button-mouse   ((t (:inherit (custom-button) :foreground ,theme-color-accent))))
 `(widget-button         ((t (:inherit (custom-button)))))
 `(widget-button-pressed ((t (:inherit (custom-button-pressed)))))
 `(widget-field          ((t (:foreground ,theme-color-level-4 :background ,theme-color-level-2 :extend t)))))

;; outlines
(custom-set-faces
 `(outline-1 ((t (:inherit (bold) :height 1.0 :extend t :foreground ,theme-color-blue))))
 `(outline-2 ((t (:inherit (bold) :height 1.0 :extend t :foreground ,theme-color-yellow))))
 `(outline-3 ((t (:inherit (bold) :height 1.0 :extend t :foreground ,theme-color-green))))
 `(outline-4 ((t (:inherit (bold) :height 1.0 :extend t :foreground ,theme-color-magenta))))
 `(outline-5 ((t (:inherit (bold) :height 1.0 :extend t :foreground ,theme-color-red))))
 `(outline-6 ((t (:inherit (bold) :height 1.0 :extend t :foreground ,theme-color-red))))
 `(outline-7 ((t (:inherit (bold) :height 1.0 :extend t :foreground ,theme-color-red))))
 `(outline-8 ((t (:inherit (bold) :height 1.0 :extend t :foreground ,theme-color-red)))))

;; others
(custom-set-faces
 `(completions-common-part      ((t (:foreground ,theme-color-level-1 :background ,theme-color-accent))))
 `(cursor                       ((t (:background ,theme-color-accent))))
 `(diff-refine-changed          ((t (:extend t))))
 `(fringe                       ((t (:inherit (shadow)))))
 `(isearch-fail                 ((t (:inherit (error)))))
 `(minibuffer-prompt            ((t (:inherit (bold) :foreground ,theme-color-accent))))
 `(pulse-highlight-start-face   ((t (:background ,theme-color-accent))))
 `(region                       ((t (:foreground ,theme-color-level-1 :background ,theme-color-high :extend t))))
 `(secondary-selection          ((t (:foreground ,theme-color-accent :background ,theme-color-high :extend t))))
 `(show-paren-match             ((t (:inherit (bold) :foreground ,theme-color-accent))))
 `(show-paren-mismatch          ((t (:inherit (error) :inverse-video t)))))

;;; CONFIGURATIONS

;;;; ADAPTIVE WRAP

;; nicer `visual-line-mode'

(my/install 'adaptive-wrap)

(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

;;;; AVY

(my/install 'avy)

(global-set-key (kbd "s-.") 'avy-goto-char-timer)
(global-set-key (kbd "s-l") 'avy-goto-line)

;;;; BACKUPS

(custom-set-variables
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups"))))

;;;; BASIC EDITING

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
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(json-encoding-default-indentation "    "))

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
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(global-company-mode t))

(custom-set-variables
 '(company-posframe-mode t)
 '(company-posframe-quickhelp-delay nil)
 '(company-posframe-show-indicator nil)
 '(company-posframe-show-metadata nil))

(setq company-posframe-show-params `(:internal-border-color ,theme-color-level-2 :internal-border-width 5))

(custom-set-faces
 `(company-tooltip            ((t (:inherit (default)))))
 `(company-tooltip-common     ((t (:inherit (completions-common-part)))))
 `(company-tooltip-selection  ((t (:inherit (highlight)))))
 `(company-tooltip-annotation ((t (:inherit (shadow)))))
 `(company-scrollbar-bg       ((t (:background ,theme-color-level-3))))
 `(company-scrollbar-fg       ((t (:background ,theme-color-accent)))))

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

;;;; CONSULT

(my/install 'consult)

(global-set-key (kbd "C-x r j") 'consult-register)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "s--") 'consult-line)
(global-set-key (kbd "s-b") 'consult-project-buffer)
(global-set-key (kbd "s-i") 'consult-imenu)
(global-set-key (kbd "s-m") 'consult-outline)
(global-set-key (kbd "s-g") 'consult-ripgrep)

;;;; CURSOR

(custom-set-variables
 '(blink-cursor-mode t)
 '(cursor-type '(bar . 3))
 '(cursor-in-non-selected-windows nil))

;;;; DIFF-HL

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

;;;; DIRED

;; use the native ls implementation to be consistent even on macOS and allow to
;; omit hidden files
(with-eval-after-load 'dired
  (require 'ls-lisp)
  (require 'dired-x))

;; refresh the dired buffer at every visit
(add-hook 'dired-mode-hook 'auto-revert-mode)

(custom-set-variables
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity '(uid gid))
 '(ls-lisp-dirs-first t)
 '(dired-omit-files "^\\."))

;;;; DUMB JUMP

(my/install 'dumb-jump)

;; allow to use dumb-jump with xref (e.g., M-.)
(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

;; TODO is this really needed?
(custom-set-variables
 '(dumb-jump-prefer-searcher 'rg))

;;;; EASY REVERT BUFFER

;; utility to abruptly discard changes in the current buffer

(defun my/force-revert-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (lazy-highlight-cleanup t)
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
 '(erc-autojoin-channels-alist '(("libera.chat$" . ("#emacs")))))

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
  (let* ((credentials (split-string (password-store-get "Libera.Chat")))
         (nick (nth 0 credentials))
         (password (nth 1 credentials)))
    (erc-tls
     :server "irc.libera.chat"
     :port 6697
     :nick nick
     :password password)))

(global-set-key (kbd "C-c i") 'my/irc)

;;;; CONVENIENT ERROR NAVIGATION

(global-set-key (kbd "s-[") 'previous-error)
(global-set-key (kbd "s-]") 'next-error)

;;;; FFAP

(global-set-key (kbd "s-F") 'ffap-menu)

;;;; FIND

;; find in whole path
(custom-set-variables
 '(find-name-arg "-path"))

(global-set-key (kbd "C-c f") 'find-name-dired)

;;;; GO MODE

(custom-set-variables
 '(go-ts-mode-indent-offset 4))

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
 '(isearch-allow-motion t)
 '(lazy-highlight-cleanup nil)
 '(lazy-highlight-buffer t)
 '(lazy-highlight-max-at-a-time nil)
 '(search-invisible nil))

;; occur using the last isearch query (superseded by ctrlf's `M-s o')
(global-set-key (kbd "C-c o") 'isearch-occur)

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
(add-to-list 'auto-mode-alist '("\\.m?js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;; JSON

(custom-set-variables
 '(json-ts-mode-indent-offset 4))

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
  ;; coherently remap macOS scrolling keys
  (global-set-key (kbd "<s-up>") 'beginning-of-buffer)
  (global-set-key (kbd "<s-down>") 'end-of-buffer)

  ;; use the right meta key natively so to allow typing fancy glyphs
  (custom-set-variables
   '(mac-right-option-modifier 'none))

  ;; tune the window decorations and appearance
  (add-to-list 'default-frame-alist '(undecorated . t))

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
   '((stashes . hide) (unpushed . show))))

;; make clearer the commit message overflow
(custom-set-faces
 `(git-commit-overlong-summary ((t (:inherit (error) :inverse-video t)))))
 `(magit-header-line ((t (:inherit (header-line)))))

(global-set-key (kbd "C-c s") 'magit-status)

;;;; MARKDOWN

(my/install 'markdown-mode)
(my/install 'edit-indirect)

(custom-set-variables
 '(markdown-asymmetric-header t)
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-fontify-whole-heading-line t)
 '(markdown-hide-urls t)
 '(markdown-nested-imenu-heading-index nil)
 '(markdown-url-compose-char "…"))

(custom-set-faces
 `(markdown-code-face           ((t (:background ,theme-color-level-2 :extend t))))
 `(markdown-inline-code-face    ((t (:inherit (shadow)))))
 `(markdown-metadata-value-face ((t (:inherit (default)))))
 `(markdown-header-face-1       ((t (:inherit (outline-1)))))
 `(markdown-header-face-2       ((t (:inherit (outline-2)))))
 `(markdown-header-face-3       ((t (:inherit (outline-3)))))
 `(markdown-header-face-4       ((t (:inherit (outline-4)))))
 `(markdown-header-face-5       ((t (:inherit (outline-5)))))
 `(markdown-header-face-6       ((t (:inherit (outline-6))))))

;; use nice ellipses (this also works for org mode)
(set-display-table-slot standard-display-table 'selective-display (string-to-vector "…"))

(add-hook 'markdown-mode-hook 'flyspell-mode)

;;;; MINIBUFFER

;; infinite minibuffer history and case insensitive completion
(custom-set-variables
 '(completion-ignore-case t)
 '(history-length t)
 '(savehist-mode t)
 '(read-buffer-completion-ignore-case t))

;;;; MINIONS

(my/install 'minions)

(custom-set-variables
 '(minions-mode t)
 '(minions-mode-line-lighter "···")
 '(minions-direct '(overwrite-mode)))

;;;; MODE LINE

(defun my/abbreviate-path (path)
  (let* ((path (abbreviate-file-name path)))
    (if (> (length path) (/ (window-total-width) 2))
        (replace-regexp-in-string (rx (and (group (not "/")) (* (not "/")))) "\\1" path)
      path)))

(custom-set-variables
 '(winum-mode-line-position 0)
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

;;;; MOUSE

(custom-set-variables
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-yank-at-point t))

;;;; NATIVE COMP

(custom-set-variables
 '(native-comp-async-report-warnings-errors nil))

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

;; save and restore the window configurations on switch

(require 'cl-lib)

(setq my/projectile-window-configurations (make-hash-table :test 'equal))

(defun my/projectile-save-window-configuration ()
  ;; save the current window configuration for this project before switching away
  (when (projectile-project-p)
    (puthash (projectile-project-root) (current-window-configuration) my/projectile-window-configurations)))

(defun my/projectile-restore-window-configuration ()
  ;; restore the window configuration for this project after switching to it
  (let ((configuration (gethash (projectile-project-root) my/projectile-window-configurations)))
    ;; restore the configuration or start anew (in that case the current buffer
    ;; is the one created by the my/projectile-switch-project-action function)
    (if configuration
        (set-window-configuration configuration)
      (delete-other-windows)))
  ;; kill all the open unused file-associated buffers
  (mapc 'kill-buffer
        (cl-set-difference
         (seq-filter 'buffer-file-name (projectile-project-buffers))
         (mapcar 'window-buffer (window-list)))))

(defun my/projectile-open (filename)
  ;; open a new project by selecting one of its files
  (interactive "fFind file: ")
  (my/projectile-save-window-configuration)
  (find-file filename)
  (delete-other-windows))

(add-hook 'projectile-before-switch-project-hook 'my/projectile-save-window-configuration)
(add-hook 'projectile-after-switch-project-hook 'my/projectile-restore-window-configuration)

(custom-set-variables
 '(projectile-mode t)
 '(projectile-switch-project-action 'projectile-dired)) ; always do dired, windows configurations will be possibly restored after

;; define the global entrypoint key and some shortcuts
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-o") 'my/projectile-open)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-switch-project)
  (define-key projectile-mode-map (kbd "s-k") 'projectile-kill-buffers)
  (define-key projectile-mode-map (kbd "s-f") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "s-d") 'projectile-find-dir)
  (define-key projectile-mode-map (kbd "s-t") 'projectile-dired)
  (define-key projectile-mode-map (kbd "s-S-g") 'projectile-ripgrep)
  (define-key projectile-mode-map (kbd "s-s") 'projectile-run-vterm))

;;;; PYTHON

(custom-set-variables
 '(python-shell-interpreter "python3"))

(defun my/fix-python-tab-width-nonsense ()
  (setq tab-width (default-value 'tab-width)))

(add-hook 'python-mode-hook 'my/fix-python-tab-width-nonsense)

;;;; RECENTF

(custom-set-variables
 '(recentf-mode t))

(global-set-key (kbd "s-r") 'recentf-open)

;;;; RIPGREP

(my/install 'ripgrep)

(custom-set-variables
 '(ripgrep-arguments '("--glob !.git")))

;;;;; HIDE THE RIPGREP COMMAND

;; use a cleaner ripgrep output by hiding the command
(add-hook 'ripgrep-search-finished-hook 'my/ripgrep-fix)
(defun my/ripgrep-fix ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (forward-line 3) ; kill 4th line
      (kill-whole-line))))

;;;; SAVE PLACE

(custom-set-variables
 '(save-place-mode t))

;;;; SERVER

(server-start)

;;;; SPELL CHECK

(global-set-key (kbd "C-c k") 'ispell-buffer)

;;;; SHELL

;; avoid showing the shell buffer output immediately for async commands and
;; allow more than one of them
(custom-set-variables
 '(async-shell-command-buffer 'new-buffer)
 '(async-shell-command-display-buffer nil))

;;;; TAB BAR

(defun my/tab-bar-tab-name-format (tab i)
  (propertize
   (format " %d • %s " i (alist-get 'name tab))
   'face (funcall tab-bar-tab-face-function tab)))

(defun my/tab-bar-tab-name-function ()
  (if (projectile-project-p)
      (format "%s » %s" (projectile-project-name) (buffer-name))
    (buffer-name)))

(custom-set-variables
 '(tab-bar-auto-width-max nil)
 '(tab-bar-close-button-show nil)
 '(tab-bar-format '((lambda () "C-x t") tab-bar-format-tabs)) ; XXX to avoid trailing space...
 '(tab-bar-mode t)
 '(tab-bar-select-tab-modifiers '(super))
 '(tab-bar-separator "  ")
 '(tab-bar-show 1)
 '(tab-bar-tab-name-format-function 'my/tab-bar-tab-name-format)
 '(tab-bar-tab-name-function 'my/tab-bar-tab-name-function))

(custom-set-faces
 `(tab-bar ((t (:background ,theme-color-level-3))))
 `(tab-bar-tab ((t (:foreground ,theme-color-accent :background ,theme-color-level-1))))
 `(tab-bar-tab-inactive ((t (:inherit (tab-bar-tab) :foreground ,theme-color-level-4)))))

(global-set-key (kbd "M-s-<left>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "M-s-<right>") 'tab-bar-switch-to-next-tab)

;;;; TERM

(custom-set-faces
 `(term-color-red     ((t (:foreground ,theme-color-red))))
 `(term-color-green   ((t (:foreground ,theme-color-green))))
 `(term-color-yellow  ((t (:foreground ,theme-color-yellow))))
 `(term-color-blue    ((t (:foreground ,theme-color-blue))))
 `(term-color-magenta ((t (:foreground ,theme-color-magenta))))
 `(term-color-cyan    ((t (:foreground ,theme-color-cyan)))))

;;;; TRAMP

;; avoid verbose tramp messages and show errors only
(custom-set-variables
 '(tramp-verbose 1))

;;;; TREESIT

(my/install 'treesit-auto)

(require 'treesit-auto)

(custom-set-variables
 '(global-treesit-auto-mode t)
 '(treesit-auto-install t))

;; adjustments for modes
(custom-set-variables
 '(c-ts-mode-indent-offset 4))

;;;; USER INTERFACE

(windmove-default-keybindings)

(custom-set-variables
 '(confirm-kill-emacs 'y-or-n-p)
 '(disabled-command-function nil)
 '(echo-keystrokes 0.1)
 '(frame-resize-pixelwise t)
 '(help-window-select t)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(menu-bar-mode nil)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position 'always)
 '(tab-width 4)
 '(tool-bar-mode nil)
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

;;;; VERTICO

(my/install 'vertico)
(my/install 'orderless)

(custom-set-variables
 '(completion-category-defaults nil)
 '(completion-styles '(orderless))
 '(orderless-match-faces [completions-common-part])
 '(vertico-mode t))

;;;; VTERM

;; XXX this requires CMake to be installed so that the first time it compiles the module
(my/install 'vterm)

(custom-set-variables
 '(vterm-kill-buffer-on-exit nil))

(global-set-key (kbd "C-c a") 'vterm)

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

;;;; WOMAN

;; fill the whole frame on creation (refresh with `R`)
(custom-set-variables
 '(woman-fill-frame t))

(custom-set-faces
 `(woman-bold   ((t (:inherit (bold)))))
 `(woman-italic ((t (:inherit (italic) :foreground ,theme-color-green)))))

(global-set-key (kbd "C-c m") 'woman)

;;;; WRITEROOM

(my/install 'writeroom-mode)

(custom-set-variables
 `(writeroom-bottom-divider-width ,theme-divider-width)
 '(writeroom-fullscreen-effect 'fullboth))

(global-set-key (kbd "C-c w") 'writeroom-mode)

;;;; ZOOM

(my/install 'zoom)

;; use a bigger target size and resize temp buffers anyway
(custom-set-variables
 '(zoom-mode t)
 '(zoom-size '(120 . 30))
 '(temp-buffer-resize-mode t))

(global-set-key (kbd "C-c z") 'zoom-mode)

;;; ADDITIONAL INITIALIZATION

;; use this as an entrypoint for additional local configurations
(let ((local "~/.emacs.d/local.el"))
  (when (file-exists-p local)
    (load-file local)))

;;; FILE VARIABLES

;; Local Variables:
;; eval: (rainbow-mode)
;; eval: (outshine-mode)
;; End:
