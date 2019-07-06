;;; PERFORMANCE

;; call the garbage collector less often (see `gcs-done')
(custom-set-variables
 '(gc-cons-threshold (* 32 (expt 2 20)))) ; 32 MB

;;; UTILITIES

(defun my/install (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

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
(setq theme-red        "#cc6666")
(setq theme-green      "#b5bd68")
(setq theme-yellow     "#f0c674")
(setq theme-blue       "#81a2be")
(setq theme-pink       "#b294bb")

;; font lock palette
(setq theme-palette-1  "#5f819d")
(setq theme-palette-2  "#f0c674")
(setq theme-palette-3  "#a54242")
(setq theme-palette-4  "#666d65")
(setq theme-palette-5  "#de935f")
(setq theme-palette-6  "#85678f")
(setq theme-palette-7  "#81a2be")
(setq theme-palette-8  "#b5bd68")

;; theme parameters
(setq theme-divider-width 6)

;;; PACKAGES

(custom-set-variables
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

(global-set-key (kbd "C-c p") 'package-list-packages)

(package-initialize)

;;; THEME

(custom-set-variables
 '(window-divider-mode t)
 '(window-divider-default-places t)
 '(window-divider-default-bottom-width theme-divider-width)
 '(window-divider-default-right-width theme-divider-width))

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
 ;; highlightings
 `(hi-black-b                   ((t (:inherit (bold)))))
 `(hi-black-hb                  ((t (:inherit (bold)))))
 `(hi-blue                      ((t (:foreground ,theme-background :background ,theme-blue))))
 `(hi-blue-b                    ((t (:inherit (hi-blue bold) :inverse-video t))))
 `(hi-green                     ((t (:foreground ,theme-background :background ,theme-green))))
 `(hi-green-b                   ((t (:inherit (hi-green bold) :inverse-video t))))
 `(hi-pink                      ((t (:foreground ,theme-background :background ,theme-pink))))
 `(hi-red-b                     ((t (:inherit (bold) :foreground ,theme-red))))
 `(hi-yellow                    ((t (:foreground ,theme-background :background ,theme-yellow))))
 ;; widgets
 `(custom-button                ((t (:box (:line-width 2 :color nil :style released-button) :foreground ,theme-foreground :background ,theme-faint))))
 `(custom-button-pressed        ((t (:inherit (custom-button-mouse) :box (:line-width 2 :color nil :style released-button) :foreground ,theme-accent))))
 `(custom-button-mouse          ((t (:inherit (highlight))))) ; for coherence with widget-button
 `(widget-field                 ((t (:foreground ,theme-foreground :background ,theme-faint))))
 `(widget-button                ((t (:inherit (custom-button)))))
 `(widget-button-pressed        ((t (:inherit (custom-button-pressed)))))
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

;;; BACKUPS

(custom-set-variables
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups"))))

;;; BASIC EDITING

(custom-set-variables
 '(require-final-newline 'ask)
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

;;; COMPILATION

(custom-set-variables
 '(compile-command "make")
 '(compilation-scroll-output 'first-error)
 '(compilation-always-kill t)
 '(compilation-disable-input t))

;; TODO move in theme since this is not an external package
(custom-set-faces
 '(compilation-mode-line-exit ((t (:inherit (success)))))
 '(compilation-mode-line-run  ((t (:inherit (warning)))))
 '(compilation-mode-line-fail ((t (:inherit (error))))))

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

;; inhibit the auto-kill beahavior if the compilation window is already present
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

;;; DIRED

;; TODO check this is actually working

(require 'ls-lisp)

(custom-set-variables
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity '(uid gid)))

;;; DIFF-HL

(my/install 'diff-hl)

(custom-set-variables
 '(diff-hl-draw-borders nil)
 '(global-diff-hl-mode t))

(custom-set-faces
 '(diff-hl-change ((t (:inherit (warning) :inverse-video t))))
 '(diff-hl-insert ((t (:inherit (success) :inverse-video t))))
 '(diff-hl-delete ((t (:inherit (error) :inverse-video t)))))

;;; ERC

(my/install 'password-store)

(custom-set-variables
 ;; disable hard fill
 '(erc-modules '(completion autojoin button irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))
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

;; disable automatic point recentering so that the prompt stays still
(add-hook 'erc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively) 101)))

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

;;; WHITESPACE MANAGMENT

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

;;; INHIBIT CUSTOMIZATION INTERFACE

(custom-set-variables
 '(custom-file "/dev/null"))

;;; EASY REVERT BUFFER

(defun my/force-revert-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(global-set-key (kbd "C-c R") 'my/force-revert-buffer)

;;; ERROR NAVIGATION

(global-set-key (kbd "<M-up>") 'previous-error)
(global-set-key (kbd "<M-down>") 'next-error)

;;; FIND

;; find in whole path
(custom-set-variables
 '(find-name-arg "-path"))

(global-set-key (kbd "C-c f") 'find-name-dired)

;;; GREP

;; exclude Node.js folders
(eval-after-load 'grep
  '(add-to-list 'grep-find-ignored-directories "node_modules"))

;; use a cleaner ~grep~ output by hiding the command
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

;;; GGTAGS

(my/install 'ggtags)

;; automatically enable ggtags globally for every C-derived programming mode
(add-hook 'c-mode-common-hook (lambda () (ggtags-mode 1)))

;;; GRAPHINCAL INTERFACE

;; avoid suspend-frame in GUI mode
(advice-add 'iconify-or-deiconify-frame :before-until 'display-graphic-p)

;; create a gtkrc file that matches the theme color to avoid glitches (it works
;; from the second time on)
(let ((gtkrc "~/.emacs.d/gtkrc"))
  (when (file-newer-than-file-p load-file-name gtkrc)
    (mkdir "~/.emacs.d" t)
    (with-temp-file gtkrc
      (insert (format "style \"default\" { bg[NORMAL] = \"%s\" }\n" theme-background))
      (insert "class \"GtkWidget\" style \"default\"\n"))))

;; TODO do the same for Xresources?

;;; MACOS

(my/install 'exec-path-from-shell)

(when (eq system-type 'darwin)
  ;; use the correct $PATH environment variable
  (exec-path-from-shell-initialize)

  ;; use the right meta key natively so to allow typing fancy glyphs
  (custom-set-variables
   '(mac-right-option-modifier 'none))

  ;; use a bigger font size to compensate the retina screen
  (custom-set-faces
   '(default ((t (:family "Iosevka" :height 170)))))

  ;; disable scrolling inertia
  (setq ns-use-mwheel-momentum nil))

;;; IBUFFER

(custom-set-variables
 '(ibuffer-expert t))

(defalias 'list-buffers 'ibuffer)

;;; INITIALIZATION

(custom-set-variables
 '(initial-scratch-message "")
 '(initial-buffer-choice t))

;; show some performace stats
(defun display-startup-echo-area-message ()
  (message "Emacs started in %s triggering the garbage collector %d times"
           (emacs-init-time) gcs-done))

;;; ISEARCH

;; inhibit search/replace on invisible text
(custom-set-variables
 '(search-invisible nil))

;;; JAVASCRIPT

(my/install 'js2-mode)

(custom-set-variables
 '(js2-include-node-externs t)
 '(js2-skip-preprocessor-directives t))

(custom-set-faces
  `(js2-object-property ((t (:inherit (font-lock-builtin-face))))))

;; associate by file name and shebang
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;; MAGIT

(my/install 'magit)

;; git commit utilities
(global-git-commit-mode)
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(custom-set-variables
 '(magit-section-initial-visibility-alist '((stashes . show) (unpushed . show))))

(global-set-key (kbd "C-c s") 'magit-status)

;;; MARKDOWN

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

(set-display-table-slot standard-display-table
                        'selective-display
                        (string-to-vector "\u2026"))

;; avoid some visibilities
(advice-add 'markdown-cycle :before 'my/markdown-cycle-advice)
(defun my/markdown-cycle-advice (arg)
  ;; skip "CONTENTS" visibility global status
  (when (eq markdown-cycle-global-status 2)
    (setq markdown-cycle-global-status 3))
  ;; skip "SUBTREE" visibility subtree status
  (when (eq markdown-cycle-subtree-status 'children)
    (setq markdown-cycle-subtree-status 'subtree))
  ;; scroll the current heading to top if globally invoked
  (when arg
    (ignore-errors (markdown-back-to-heading))
    (recenter 0)))

;;; MINIBUFFER

;; infinite minibuffer history
(custom-set-variables
 '(savehist-mode t)
 '(history-length t))

;;; MOUSE

(custom-set-variables
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-yank-at-point t))

;;; PHP

(my/install 'php-mode)

(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;;; SAVE PLACE

(custom-set-variables
 '(save-place-mode t))

;;; SHELL

;; avoid showing the shell buffer output immediately for async commands and
;; allow more than one of them
(custom-set-variables
 '(async-shell-command-buffer 'new-buffer)
 '(async-shell-command-display-buffer nil))

(global-set-key (kbd "C-c a") 'shell)

;;; USER INTERFACE

(windmove-default-keybindings)

(custom-set-variables
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(disabled-command-function nil)
 '(echo-keystrokes 0.1)
 '(font-lock-maximum-decoration 2)
 '(help-window-select t)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(isearch-allow-scroll t)
 '(menu-bar-mode nil)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(use-dialog-box nil)
 '(vc-follow-symlinks t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

;; visual line for text modes
(add-hook 'text-mode-hook 'visual-line-mode)

;;; WINNER

(custom-set-variables
 '(winner-mode t))

;; COMB

(my/install 'comb)

;; customize the keybindings
(eval-after-load 'comb
  '(progn (define-key comb-keymap (kbd "RET") 'comb-approve-next)
          (define-key comb-keymap (kbd "DEL") 'comb-reject-next)
          (define-key comb-keymap (kbd "SPC") 'comb-undecide-next)))

;;; ZOOM

(my/install 'zoom)

;; use a bigger target size and resize temp buffers anyway
(custom-set-variables
 '(zoom-mode t)
 '(zoom-size '(120 . 30))
 '(temp-buffer-resize-mode t))

;;; OTHER PACKAGES

(my/install 'yaml-mode)
(my/install 'rainbow-mode)
(my/install 'dockerfile-mode)

;;; FILE VARIABLES

;; Local Variables:
;; eval: (rainbow-mode)
;; End:
