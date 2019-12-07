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
(setq gc-cons-threshold (* 32 (expt 2 20))) ; 32 MB
(add-hook 'after-init-hook (lambda () (custom-reevaluate-setting 'gc-cons-threshold)))

;; force the garbage collection to happen when the focus moves away from emacs
(add-hook 'focus-out-hook 'garbage-collect)

;;;; UTILITIES

;; utility to defer slow operation so to not directly impact init time
(defmacro my/defer (body)
  `(run-with-idle-timer
    0.5 nil (lambda () ,body)))

;;; GLOBALS

;; UI and base colors
(setq theme-background "#000000")
(setq theme-foreground "#aaaaaa")
(setq theme-accent     "#ff6000")
(setq theme-bright     "#ffffff")
(setq theme-faint      "#666666")
(setq theme-dark       "#222222")
(setq theme-very-dark  "#0c0c0c")

;; common colors (darker variant from http://terminal.sexy/)
(setq theme-red     "#A54242")
(setq theme-green   "#8C9440")
(setq theme-yellow  "#DE935F")
(setq theme-blue    "#5F819D")
(setq theme-magenta "#85678F")
(setq theme-cyan    "#5E8D87")

;; theme parameters
(setq theme-divider-width   6)
(setq theme-font            "Iosevka SS04")
(setq theme-font-size-linux 14)
(setq theme-font-size-macos 16)

;;; THEME

;; required for styling buttons
(require 'cus-edit)

(custom-set-variables
 ;; window dividers
 '(window-divider-mode t)
 '(window-divider-default-places t)
 '(window-divider-default-bottom-width theme-divider-width)
 '(window-divider-default-right-width theme-divider-width)
 ;; widget and custom button coherence
 '(custom-raised-buttons t) ; for terminal mode
 '(widget-mouse-face 'custom-button-mouse)
 ;; use no widgets marks
 '(widget-push-button-prefix " ")
 '(widget-push-button-suffix " ")
 '(widget-link-prefix " ")
 '(widget-link-suffix " "))

(custom-set-faces
 ;; basic faces
 `(default                      ((t (:foreground ,theme-foreground :background ,theme-background))))
 `(shadow                       ((t (:foreground ,theme-faint))))
 `(link                         ((t (:foreground ,theme-accent :underline (:color foreground-color :style line)))))
 `(link-visited                 ((t (:inherit (link) :weight normal))))
 `(highlight                    ((t (:background ,theme-dark))))
 `(match                        ((t (:foreground ,theme-accent :weight bold))))
 `(isearch                      ((t (:foreground ,theme-background :background ,theme-accent))))
 `(lazy-highlight               ((t (:foreground ,theme-background :background ,theme-bright))))
 `(error                        ((t (:foreground ,theme-red))))
 `(warning                      ((t (:foreground ,theme-yellow))))
 `(success                      ((t (:foreground ,theme-green))))
 ;; header/mode line
 `(mode-line                    ((t (:foreground ,theme-accent :background ,theme-dark :box (:line-width 5 :color ,theme-dark :style nil)))))
 `(mode-line-inactive           ((t (:inherit (mode-line) :foreground ,theme-bright))))
 `(mode-line-highlight          ((t (:inverse-video t :box nil))))
 `(header-line                  ((t (:inherit (mode-line) :foreground ,theme-foreground))))
 ;; window dividers
 `(window-divider               ((t (:foreground ,theme-faint))))
 `(window-divider-first-pixel   ((t (:foreground ,theme-faint))))
 `(window-divider-last-pixel    ((t (:foreground ,theme-faint))))
 `(window-divider-last-pixel    ((t (:foreground ,theme-faint))))
 ;; font lock
 `(font-lock-function-name-face ((t (:inherit (bold) :foreground ,theme-magenta))))
 `(font-lock-variable-name-face ((t (:foreground ,theme-yellow))))
 `(font-lock-keyword-face       ((t (:inherit (bold) :foreground ,theme-red))))
 `(font-lock-comment-face       ((t (:foreground ,theme-faint))))
 `(font-lock-type-face          ((t (:foreground ,theme-blue))))
 `(font-lock-constant-face      ((t (:foreground ,theme-cyan))))
 `(font-lock-builtin-face       ((t (:foreground ,theme-cyan))))
 `(font-lock-string-face        ((t (:foreground ,theme-green))))
 `(font-lock-negation-char-face ((t (:inherit (bold) :inherit (default)))))
 ;; highlighting lock ssds
 `(hi-black-b                   ((t (:inherit (bold) :background ,theme-very-dark))))
 `(hi-black-hb                  ((t (:inherit (bold) :background ,theme-dark))))
 `(hi-blue                      ((t (:foreground ,theme-background :background ,theme-blue))))
 `(hi-blue-b                    ((t (:inherit (hi-blue bold) :inverse-video t))))
 `(hi-green                     ((t (:foreground ,theme-background :background ,theme-green))))
 `(hi-green-b                   ((t (:inherit (hi-green bold) :inverse-video t))))
 `(hi-pink                      ((t (:foreground ,theme-background :background ,theme-magenta))))
 `(hi-red-b                     ((t (:inherit (bold) :foreground ,theme-red))))
 `(hi-yellow                    ((t (:foreground ,theme-background :background ,theme-yellow))))
 ;; compilation
 '(compilation-mode-line-exit   ((t (:inherit (success)))))
 '(compilation-mode-line-run    ((t (:inherit (warning)))))
 '(compilation-mode-line-fail   ((t (:inherit (error)))))
 ;; widgets
 `(custom-button                ((t (:box (:line-width 2 :color nil :style released-button) :foreground ,theme-background :background ,theme-faint))))
 `(custom-button-pressed        ((t (:inherit (custom-button-mouse) :box (:line-width 2 :color nil :style pressed-button)))))
 `(custom-button-mouse          ((t (:inherit (custom-button) :background ,theme-foreground))))
 `(widget-button                ((t (:inherit (custom-button)))))
 `(widget-button-pressed        ((t (:inherit (custom-button-pressed)))))
 `(widget-field                 ((t (:foreground ,theme-foreground :background ,theme-dark))))
 ;; outlines
 `(outline-1                    ((t (:inherit (bold) :extend t :height 1.4 :background ,theme-very-dark :foreground ,theme-blue))))
 `(outline-2                    ((t (:inherit (bold) :extend t :height 1.2 :background ,theme-very-dark :foreground ,theme-yellow))))
 `(outline-3                    ((t (:inherit (bold) :extend t :height 1.2 :background ,theme-very-dark :foreground ,theme-green))))
 `(outline-4                    ((t (:inherit (bold) :extend t :height 1.2 :background ,theme-very-dark :foreground ,theme-magenta))))
 `(outline-5                    ((t (:inherit (bold) :extend t :height 1.2 :background ,theme-very-dark :foreground ,theme-red))))
 `(outline-6                    ((t (:inherit (bold) :extend t :height 1.0 :background ,theme-very-dark :foreground ,theme-red))))
 `(outline-7                    ((t (:inherit (bold) :extend t :height 1.0 :background ,theme-very-dark :foreground ,theme-red))))
 `(outline-8                    ((t (:inherit (bold) :extend t :height 1.0 :background ,theme-very-dark :foreground ,theme-red))))
 ;; others
 `(cursor                       ((t (:background ,theme-bright))))
 `(fringe                       ((t (:foreground ,theme-dark))))
 `(minibuffer-prompt            ((t (:foreground ,theme-accent :weight bold))))
 `(region                       ((t (:foreground ,theme-background :background ,theme-faint))))
 `(secondary-selection          ((t (:foreground ,theme-background :background ,theme-foreground))))
 `(isearch-fail                 ((t (:inherit (error)))))
 `(completions-common-part      ((t (:inherit (shadow)))))
 `(completions-first-difference ((t (:foreground ,theme-accent))))
 `(pulse-highlight-start-face   ((t (:background ,theme-accent))))
 `(show-paren-match             ((t (:inherit (bold) :foreground ,theme-accent))))
 `(show-paren-mismatch          ((t (:inherit (error) :inverse-video t)))))

;;; PACKAGES

;; XXX temporary hackish solution for https://debbugs.gnu.org/34341
(if (version< emacs-version "26.3")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; install a package refreshing the packet list just once
(defun my/install (package)
  (unless (package-installed-p package)
    (unless (bound-and-true-p my/install-refreshed)
      (package-refresh-contents)
      (setq my/install-refreshed t))
    (package-install package)))

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(global-set-key (kbd "C-c p") 'package-list-packages)

;;; CONFIGURATIONS

;;;; ADAPTIVE WRAP

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

(setq my/todo-regexp (rx bow (or "TODO" "XXX") eow))
(setq my/todo-face 'font-lock-warning-face)

(defun my/todo-fontlock-hook ()
  (font-lock-add-keywords nil `((,my/todo-regexp 0 ,my/todo-face prepend)) t))

(defun my/todo-occur ()
  (interactive)
  (occur my/todo-regexp))

(add-hook 'text-mode-hook 'my/todo-fontlock-hook)
(add-hook 'prog-mode-hook 'my/todo-fontlock-hook)

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
 `(company-posframe-mode ,(not (eq system-type 'darwin))) ; https://github.com/tumashu/posframe/issues/30
 '(company-idle-delay 0.1)
 '(company-show-numbers t)
 '(company-minimum-prefix-length 2)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil))

(custom-set-faces
 `(company-tooltip                  ((t (:background ,theme-faint :foreground ,theme-background))))
 `(company-tooltip-common           ((t (:foreground ,theme-accent :background ,theme-very-dark))))
 `(company-tooltip-search           ((t (:inherit (isearch)))))
 `(company-tooltip-search-selection ((t (:inherit (lazy-highlight)))))
 `(company-tooltip-selection        ((t (:background ,theme-accent))))
 `(company-tooltip-mouse            ((t (:inherit (company-tooltip-selection)))))
 `(company-tooltip-annotation       ((t (:foreground ,theme-dark))))
 `(company-template-field           ((t (:background ,theme-dark))))
 `(company-preview-common           ((t (:background ,theme-very-dark :foreground ,theme-accent))))
 `(company-scrollbar-bg             ((t (:background ,theme-dark))))
 `(company-scrollbar-fg             ((t (:background ,theme-accent)))))

;; start completion with backspace too
(add-to-list 'company-begin-commands 'delete-backward-char)
(add-to-list 'company-begin-commands 'backward-delete-char-untabify)

;;;; COMPILATION

(custom-set-variables
 '(compile-command "make")
 '(compilation-always-kill t)
 '(compilation-disable-input t))

;; automatically kill the compilation window on success after a short delay, but
;; only if successful
(add-to-list 'compilation-finish-functions 'my/compile-auto-quit)
(defun my/compile-auto-quit (buffer status)
  (let ((window (get-buffer-window buffer)))
    (when (and
           (bound-and-true-p my/compile-should-auto-quit)
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

;; inhibit the auto-kill behavior if the compilation window is already present
;; when the re/compilation is started
(advice-add 'compile :before 'my/compile-before)
(advice-add 'recompile :before 'my/compile-before)
(defun my/compile-before (&rest ignore)
  (let* ((buffer (get-buffer "*compilation*"))
         (window (get-buffer-window buffer)))
    (setq my/compile-should-auto-quit (not (and buffer window)))))

;; add a smart recompilation helper
(defun my/smart-compile ()
  "Recompile or prompt a new compilation."
  (interactive)
  ;; reload safe variables silently
  (let ((enable-local-variables :safe))
    (hack-local-variables))
  ;; smart compile
  (if (local-variable-p 'compile-command)
      (compile compile-command)
    (let ((buffer (get-buffer "*compilation*")))
      (if buffer
          (with-current-buffer buffer
            (recompile))
        (call-interactively 'compile)))))

(add-hook 'compilation-mode-hook 'visual-line-mode)

(global-set-key (kbd "C-c c") 'my/smart-compile)
(global-set-key (kbd "C-c C") 'compile)

;;;; CURSOR

(custom-set-variables
 '(blink-cursor-mode t)
 '(blink-cursor-delay 2.0)
 '(blink-cursor-blinks 0)
 '(cursor-type 'hollow)
 '(cursor-in-non-selected-windows nil))

;;;; DIFF-HL

(my/install 'diff-hl)

(custom-set-variables
 '(global-diff-hl-mode t)
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

(custom-set-variables
 ;; timestamp always visible
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-timestamp-format "[%H:%M] ")
 '(erc-timestamp-only-if-changed-flag nil)
 ;; make track mode less noisy and add the indicator at the end of the modeline
 ;; to not interfere with minions
 '(erc-track-exclude-types '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE"))
 '(erc-track-position-in-mode-line t)
 ;; autojoin
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

(global-set-key (kbd "C-M-<prior>") 'previous-error)
(global-set-key (kbd "C-M-<next>") 'next-error)

;;;; FIND

;; find in whole path
(custom-set-variables
 '(find-name-arg "-path"))

(global-set-key (kbd "C-c f") 'find-name-dired)

;;;; GGTAGS

(my/install 'ggtags)

;; automatically enable ggtags globally for every C-derived programming mode
(add-hook 'c-mode-common-hook 'ggtags-mode)

;;;; GREP

;; exclude Node.js folders
(with-eval-after-load 'grep
  (add-to-list 'grep-find-ignored-directories "node_modules"))

;; use a cleaner grep output by hiding the command
(add-hook 'grep-setup-hook 'my/grep-fix)
(defun my/grep-fix ()
  (save-excursion
    (let ((inhibit-read-only t))
      (forward-line 3) ; kill 4th line
      (kill-whole-line))))

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

;;;; IDO

;; use IDO only for buffers and avoid the completion buffer

(custom-set-variables
 '(ido-mode 'buffers)
 '(ido-completion-buffer nil)
 '(ido-default-buffer-method 'selected-window))

(custom-set-faces
 `(ido-only-match  ((t (:foreground ,theme-accent))))
 `(ido-first-match ((t (:inherit (ido-only-match))))))

;;;; IMENU-LIST

(my/install 'imenu-list)

(custom-set-faces
 `(imenu-list-entry-face-0 ((t (:inherit (outline-1)))))
 `(imenu-list-entry-face-1 ((t (:inherit (outline-2)))))
 `(imenu-list-entry-face-2 ((t (:inherit (outline-3)))))
 `(imenu-list-entry-face-3 ((t (:inherit (outline-4))))))

(global-set-key (kbd "C-c l") 'imenu-list-smart-toggle)

;;;; INHIBIT CUSTOMIZATION INTERFACE

;; discard persistent changes via the customization interface
(custom-set-variables
 '(custom-file "/dev/null"))

;;;; INITIALIZATION

(custom-set-variables
 '(initial-scratch-message "")
 '(inhibit-startup-screen t))

;; show some performance stats
(defun display-startup-echo-area-message ()
  (message "Emacs started in %.2f seconds triggering the GC %d times taking %.2f seconds"
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done gc-elapsed))

;;;; INSTALL OTHER PACKAGES

(my/install 'dockerfile-mode)
(my/install 'go-mode)
(my/install 'rainbow-mode)
(my/install 'rust-mode)
(my/install 'yaml-mode)

;;;; ISEARCH

;; inhibit search/replace on invisible text
(custom-set-variables
 '(isearch-allow-scroll t)
 '(search-invisible nil))

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
        (insert (format "emacs.font: %s-%d\n" theme-font theme-font-size-linux))
        (insert "emacs.menuBar: off\n")
        (insert "emacs.toolBar: off\n")
        (insert "emacs.verticalScrollBars: off\n")
        (insert (format "emacs.background: %s\n" theme-background)))
      ;; workaround for emacs 27 that does not read .Xdefaults-hostname
      (mkdir "~/.Xdefaults" t)
      (copy-file xdefaults (format "~/.Xdefaults/%s" (system-name)) t))))

;;;; LSP

(my/install 'lsp-mode)
(my/install 'lsp-ui)
(my/install 'company-lsp)
(my/install 'yasnippet)

;; add modes manually
(add-hook 'c++-mode-hook 'lsp-deferred)

;; remove the default company clang backend to avoid interferences
(custom-set-variables
 '(company-backends (remove 'company-clang company-backends)))

;;;; MACOS SPECIFIC

(when (eq system-type 'darwin)
  ;; fetch environment variables from shell (namely, those in ~/.profile since
  ;; it is not sourced by macOS but only by bash)
  (my/install 'exec-path-from-shell)
  (my/defer (exec-path-from-shell-copy-envs
             '("PATH" "PASSWORD_STORE_DIR" "NPM_CONFIG_PREFIX" "GEM_HOME" "PIP_USER")))

  ;; use the right meta key natively so to allow typing fancy glyphs
  (custom-set-variables
   '(mac-right-option-modifier 'none))

  ;; disable scrolling inertia
  (setq ns-use-mwheel-momentum nil)

  ;; setup base GUI to avoid glitches but only if needed
  (let ((plist "~/Library/Preferences/org.gnu.Emacs.plist"))
    (when (file-newer-than-file-p load-file-name plist)
      (shell-command-to-string "defaults write org.gnu.Emacs ToolBar -bool false")
      (shell-command-to-string (format "defaults write org.gnu.Emacs Font \"%s\"-%d"
                                       theme-font theme-font-size-macos))))

  ;; force GPG to use a GUI pinentry program (fetch pinentry-mac from brew) to
  ;; avoid problems
  (let ((gpg-agent-conf "~/.gnupg/gpg-agent.conf"))
    (when (file-newer-than-file-p load-file-name gpg-agent-conf)
      (mkdir "~/.gnupg" t)
      (with-temp-file gpg-agent-conf
        (insert "pinentry-program /usr/local/bin/pinentry-mac\n")))))

;;;; MAGIT

(my/install 'magit)

;; always use magit for commit editing and enable spell checking
(add-to-list 'auto-mode-alist (cons git-commit-filename-regexp 'git-commit-setup))
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
 `(markdown-code-face           ((t (:background ,theme-very-dark :extend t))))
 `(markdown-pre-face            ((t (:inherit (markdown-code-face)))))
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

;; infinite minibuffer history
(custom-set-variables
 '(savehist-mode t)
 '(history-length t))

;;;; MINIONS

(my/install 'minions)

(custom-set-variables
 '(minions-mode t)
 '(minions-mode-line-lighter "···")
 '(minions-direct '(overwrite-mode)))

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
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(disabled-command-function nil)
 '(echo-keystrokes 0.1)
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
 '(winum-format (propertize " %s " 'face 'winum-face))
 '(winum-scope 'frame-local))

(custom-set-faces
 `(winum-face ((t (:box (:color ,theme-faint) :foreground ,theme-background :background ,theme-faint)))))

;; this needs to be explicitly called in order to properly work at startup
(winum-mode)

;;;; WOMAN

;; fill the whole frame on creation (refresh with `R`)
(custom-set-variables
 '(woman-fill-frame t))

(custom-set-faces
 `(woman-bold   ((t (:inherit (bold) :foreground ,theme-bright))))
 `(woman-italic ((t (:inherit (italic) :foreground ,theme-green)))))

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
