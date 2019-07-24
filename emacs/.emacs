;;; CHEAT SHEET

;; % m           `dired-mark-files-regexp'
;; C-s C-w       `isearch-yank-word-or-char' (M-e to edit)
;; C-u C-SPC     `pop-mark'
;; C-x -         `shrink-window-if-larger-than-buffer'
;; C-x 8 RET     `insert-char'
;; C-x C-;       `comment-line'
;; C-x C-SPC     `pop-global-mark'
;; C-x C-b       `list-buffers'
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

;; utility to defer slow operation so to not directly impact init time
(defmacro my/defer (body)
  `(run-with-idle-timer
    0.5 nil (lambda () ,body)))

;; call the garbage collector less often (especially during the startup)
(custom-set-variables
 '(gc-cons-threshold (* 32 (expt 2 20)))) ; 32 MB

;;; GLOBALS

;; theme base colors, adapted from https://terminal.sexy
(setq theme-background "#000000")
(setq theme-foreground "#bfbfbf")
(setq theme-accent     "#ff6000")
(setq theme-bright     "#ffffff")
(setq theme-faint      "#666666")
(setq theme-dark       "#141414")
(setq theme-very-dark  "#0c0c0c")

;; emacs common colors
(setq theme-red    "#cc6666")
(setq theme-green  "#b5bd68")
(setq theme-yellow "#f0c674")
(setq theme-blue   "#81a2be")
(setq theme-pink   "#b294bb")

;; font lock palette
(setq theme-palette-1 "#5f819d")
(setq theme-palette-2 "#f0c674")
(setq theme-palette-3 "#a54242")
(setq theme-palette-4 "#666d65")
(setq theme-palette-5 "#de935f")
(setq theme-palette-6 "#85678f")
(setq theme-palette-7 "#81a2be")
(setq theme-palette-8 "#b5bd68")

;; theme parameters
(setq theme-divider-width   6)
(setq theme-font            "Iosevka")
(setq theme-font-size-linux 14)
(setq theme-font-size-macos 17)

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
 ;; font lock
 `(font-lock-function-name-face ((t (:foreground ,theme-palette-1))))
 `(font-lock-variable-name-face ((t (:foreground ,theme-palette-2))))
 `(font-lock-keyword-face       ((t (:foreground ,theme-palette-3))))
 `(font-lock-comment-face       ((t (:foreground ,theme-palette-4))))
 `(font-lock-type-face          ((t (:foreground ,theme-palette-5))))
 `(font-lock-constant-face      ((t (:foreground ,theme-palette-6))))
 `(font-lock-builtin-face       ((t (:foreground ,theme-palette-7))))
 `(font-lock-string-face        ((t (:foreground ,theme-palette-8))))
 `(font-lock-negation-char-face ((t (:inherit (default)))))
 ;; highlighting
 `(hi-black-b                   ((t (:inherit (bold)))))
 `(hi-black-hb                  ((t (:inherit (bold)))))
 `(hi-blue                      ((t (:foreground ,theme-background :background ,theme-blue))))
 `(hi-blue-b                    ((t (:inherit (hi-blue bold) :inverse-video t))))
 `(hi-green                     ((t (:foreground ,theme-background :background ,theme-green))))
 `(hi-green-b                   ((t (:inherit (hi-green bold) :inverse-video t))))
 `(hi-pink                      ((t (:foreground ,theme-background :background ,theme-pink))))
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
 `(widget-field                 ((t (:foreground ,theme-background :background ,theme-faint))))
 ;; others
 `(cursor                       ((t (:background ,theme-bright))))
 `(fringe                       ((t (:foreground ,theme-faint))))
 `(minibuffer-prompt            ((t (:foreground ,theme-accent :weight bold))))
 `(region                       ((t (:foreground ,theme-accent :background ,theme-faint))))
 `(secondary-selection          ((t (:foreground ,theme-accent :background ,theme-dark))))
 `(isearch-fail                 ((t (:inherit (error)))))
 `(completions-common-part      ((t (:inherit (shadow)))))
 `(completions-first-difference ((t (:foreground ,theme-accent))))
 `(pulse-highlight-start-face   ((t (:background ,theme-accent))))
 `(show-paren-match             ((t (:inherit (bold) :foreground ,theme-accent))))
 `(show-paren-mismatch          ((t (:inherit (error) :inverse-video t)))))

;;; PACKAGES

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

;;;; ANNOTATIONS

(defun my/annotations-hook ()
  (let ((regexp (rx bow (or "TODO" "XXX") eow))
        (face 'font-lock-warning-face))
    (font-lock-add-keywords nil `((,regexp 0 ,face prepend)) t)))

(add-hook 'text-mode-hook 'my/annotations-hook)
(add-hook 'prog-mode-hook 'my/annotations-hook)

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
     (innamespace . 0))))

;;;; COMB

(my/install 'comb)

;; customize the keybindings
(eval-after-load 'comb
  '(progn (define-key comb-keymap (kbd "RET") 'comb-approve-next)
          (define-key comb-keymap (kbd "DEL") 'comb-reject-next)
          (define-key comb-keymap (kbd "SPC") 'comb-undecide-next)))

;;;; COMPILATION

(custom-set-variables
 '(compile-command "make")
 '(compilation-scroll-output 'first-error)
 '(compilation-always-kill t)
 '(compilation-disable-input t))

;; automatically kill the compilation window on success after a short delay, but
;; only if successful
(add-to-list 'compilation-finish-functions 'my/compile-auto-quit)
(defun my/compile-auto-quit (buffer status)
  (let ((window (get-buffer-window buffer)))
    (when (and (equal (buffer-name buffer) "*compilation*") ; do not kill grep and similar
               my/compile-should-auto-quit ;; only for *compilation** buffers
               window ; a window to kill must exist
               (equal status "finished\n"))
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

(global-set-key (kbd "C-c c") 'my/smart-compile)
(global-set-key (kbd "C-c C") 'compile)

;;;; DIFF-HL

(my/install 'diff-hl)

(custom-set-variables
 '(global-diff-hl-mode t)
 '(diff-hl-draw-borders nil))

(custom-set-faces
 '(diff-hl-change ((t (:inherit (warning) :inverse-video t))))
 '(diff-hl-insert ((t (:inherit (success) :inverse-video t))))
 '(diff-hl-delete ((t (:inherit (error) :inverse-video t)))))

;;;; DIRED

;; use the native ls implementation to be consistent even on macOS
(eval-after-load 'dired
  '(require 'ls-lisp))

(custom-set-variables
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity '(uid gid))
 '(ls-lisp-dirs-first t))

;;;; EASY REVERT BUFFER

(defun my/force-revert-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(global-set-key (kbd "C-c R") 'my/force-revert-buffer)

;;;; ERC

(my/install 'password-store)

;; disable hard fill module
(eval-after-load 'erc
  '(progn
     (delete 'fill erc-modules)
     (erc-update-modules)))

(custom-set-variables
 ;; timestamp always visible
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-timestamp-format "[%H:%M] ")
 '(erc-timestamp-only-if-changed-flag nil)
 ;; make track mode less noisy
 '(erc-track-exclude-types '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE"))
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

(global-set-key (kbd "<M-up>") 'previous-error)
(global-set-key (kbd "<M-down>") 'next-error)

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
(eval-after-load 'grep
  '(add-to-list 'grep-find-ignored-directories "node_modules"))

;; use a cleaner grep output by hiding the command
(add-hook 'grep-setup-hook 'my/grep-fix)
(defun my/grep-fix ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-line 4)
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
  (message "Emacs started in %s triggering the garbage collector %d times"
           (emacs-init-time) gcs-done))

;;;; INSTALL OTHER PACKAGES

(my/install 'yaml-mode)
(my/install 'rainbow-mode)
(my/install 'dockerfile-mode)

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
 `(js2-object-property ((t (:inherit (font-lock-builtin-face))))))

;; associate by file name and shebang
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;; LINUX

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
  (let ((xdefaults (format "~/.Xdefaults-%s" system-name)))
    (when (file-newer-than-file-p load-file-name xdefaults)
      (with-temp-file xdefaults
        (insert (format "emacs.font: %s-%d\n" theme-font theme-font-size-linux))
        (insert "emacs.menuBar: off\n")
        (insert "emacs.toolBar: off\n")
        (insert "emacs.verticalScrollBars: off\n")
        (insert (format "emacs.background: %s\n" theme-background))))))

;;;; MACOS

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
      (shell-command-to-string (format "defaults write org.gnu.Emacs Font %s-%d"
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

;; git commit editing
(global-git-commit-mode)
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(custom-set-variables
 '(magit-section-initial-visibility-alist '((stashes . show) (unpushed . show))))

(global-set-key (kbd "C-c s") 'magit-status)

;;;; MARKDOWN

(my/install 'markdown-mode)
(my/install 'edit-indirect)

(custom-set-variables
 '(markdown-fontify-code-blocks-natively t))

(custom-set-faces
 `(markdown-code-face ((t (:background ,theme-very-dark))))
 `(markdown-header-face-1 ((t (:inherit (outline-1 bold) :height 1.4 :background ,theme-very-dark))))
 `(markdown-header-face-2 ((t (:inherit (outline-2 bold) :height 1.4 :background ,theme-very-dark))))
 `(markdown-header-face-3 ((t (:inherit (outline-3 bold) :height 1.4 :background ,theme-very-dark))))
 `(markdown-header-face-4 ((t (:inherit (outline-4 bold) :height 1.4 :background ,theme-very-dark))))
 `(markdown-header-face-5 ((t (:inherit (outline-5 bold) :height 1.4 :background ,theme-very-dark))))
 `(markdown-header-face-6 ((t (:inherit (outline-6 bold) :height 1.4 :background ,theme-very-dark)))))

;; use nice ellipses (this also works for org mode)
(set-display-table-slot standard-display-table 'selective-display (string-to-vector "\u2026"))

;;;; MINIBUFFER

;; infinite minibuffer history
(custom-set-variables
 '(savehist-mode t)
 '(history-length t))

;;;; MOUSE

(custom-set-variables
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-yank-at-point t))

;;;; OUTLINE

;; utility shortcuts that works with all the outline-based modes
(global-set-key (kbd "M-1") 'outline-hide-body)
(global-set-key (kbd "M-2") 'outline-show-all)

;;;; OUTSHINE

;; this is basically only used for this file
(my/install 'outshine)

(custom-set-variables
 '(outshine-fontify-whole-heading-line t))

(custom-set-faces
 `(outshine-level-1 ((t (:inherit (outline-1 bold) :height 1.4 :background ,theme-very-dark))))
 `(outshine-level-2 ((t (:inherit (outline-2 bold) :height 1.4 :background ,theme-very-dark))))
 `(outshine-level-3 ((t (:inherit (outline-3 bold) :height 1.4 :background ,theme-very-dark))))
 `(outshine-level-4 ((t (:inherit (outline-4 bold) :height 1.4 :background ,theme-very-dark))))
 `(outshine-level-5 ((t (:inherit (outline-5 bold) :height 1.4 :background ,theme-very-dark))))
 `(outshine-level-6 ((t (:inherit (outline-6 bold) :height 1.4 :background ,theme-very-dark))))
 `(outshine-level-7 ((t (:inherit (outline-7 bold) :height 1.4 :background ,theme-very-dark))))
 `(outshine-level-8 ((t (:inherit (outline-8 bold) :height 1.4 :background ,theme-very-dark)))))

;;;; PHP

(my/install 'php-mode)

(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

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

;;;; WOMAN

;; Fill the whole frame on creation (refresh with `R`)
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

;;; FILE VARIABLES

;; Local Variables:
;; eval: (rainbow-mode)
;; eval: (outshine-mode)
;; End:
