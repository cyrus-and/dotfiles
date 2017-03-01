(deftheme my "Simple flat dark theme")
(provide-theme 'my)

;; palette adapted from https://terminal.sexy

(let (;; theme base colors
      (theme-background "#000000")
      (theme-foreground "#c5c8c6")
      (theme-accent     "#ff6000")
      (theme-bright     "#ffffff")
      (theme-faint      "#373b41")
      (theme-dark       "#111213")

      ;; emacs common colors
      (theme-red        "#cc6666")
      (theme-green      "#b5bd68")
      (theme-yellow     "#f0c674")
      (theme-blue       "#81a2be")
      (theme-pink       "#b294bb")

      ;; font lock palette
      (theme-palette-1  "#5F819D")
      (theme-palette-2  "#f0c674")
      (theme-palette-3  "#A54242")
      (theme-palette-4  "#666d65")
      (theme-palette-5  "#DE935F")
      (theme-palette-6  "#85678F")
      (theme-palette-7  "#81A2BE")
      (theme-palette-8  "#b5bd68"))

  (custom-theme-set-faces
   'my

   ;; basic faces
   `(default        ((t (:foreground ,theme-foreground :background ,theme-background))))
   `(shadow         ((t (:foreground ,theme-faint))))
   `(link           ((t (:foreground ,theme-accent :weight bold :underline (:color foreground-color :style line)))))
   `(link-visited   ((t (:inherit (link) :weight normal))))
   `(highlight      ((t (:inherit (link) :inverse-video t :underline nil))))
   `(match          ((t (:foreground ,theme-background :background ,theme-accent))))
   `(isearch        ((t (:inherit (match)))))
   `(lazy-highlight ((t (:foreground ,theme-background :background ,theme-bright))))
   `(error          ((t (:foreground ,theme-red))))
   `(warning        ((t (:foreground ,theme-yellow))))
   `(success        ((t (:foreground ,theme-green))))

   ;; mode line
   `(mode-line           ((t (:foreground ,theme-bright :background ,theme-dark))))
   `(mode-line-inactive  ((t (:inherit (mode-line) :foreground ,theme-faint))))
   `(mode-line-highlight ((t (:inverse-video t))))

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
   `(hi-black-b  ((t (:inherit (bold)))))
   `(hi-black-hb ((t (:inherit (bold)))))
   `(hi-blue     ((t (:foreground ,theme-background :background ,theme-blue))))
   `(hi-blue-b   ((t (:inherit (hi-blue bold) :inverse-video t))))
   `(hi-green    ((t (:foreground ,theme-background :background ,theme-green))))
   `(hi-green-b  ((t (:inherit (hi-green bold) :inverse-video t))))
   `(hi-pink     ((t (:foreground ,theme-background :background ,theme-pink))))
   `(hi-red-b    ((t (:inherit (bold) :foreground ,theme-red))))
   `(hi-yellow   ((t (:foreground ,theme-background :background ,theme-yellow))))

   ;; others
   `(vertical-border              ((t (:foreground ,theme-dark))))
   `(cursor                       ((t (:background ,theme-bright))))
   `(fringe                       ((t (:inherit (shadow)))))
   `(minibuffer-prompt            ((t (:foreground ,theme-accent :weight bold))))
   `(region                       ((t (:foreground ,theme-accent :background ,theme-faint))))
   `(secondary-selection          ((t (:foreground ,theme-accent :background ,theme-dark))))
   `(isearch-fail                 ((t (:inherit (error)))))
   `(completions-common-part      ((t (:inherit (shadow)))))
   `(completions-first-difference ((t (:foreground ,theme-accent))))

   ;; fix: compilation
   `(compilation-mode-line-exit ((t (:inherit (success)))))
   `(compilation-mode-line-run  ((t (:inherit (warning)))))
   `(compilation-mode-line-fail ((t (:inherit (error)))))

   ;; fix: show-paren
   `(show-paren-match    ((t (:background ,theme-faint))))
   `(show-paren-mismatch ((t (:inherit (error) :inverse-video t))))))
