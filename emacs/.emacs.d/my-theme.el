(deftheme my "Simple flat dark theme")
(provide-theme 'my)

(let (;; theme base colors
      (theme-background "#000000")
      (theme-foreground "#eeeeee")
      (theme-accent     "#ff6000")
      (theme-bright     "#ffffff")
      (theme-faint      "#333333")
      (theme-dark       "#111111")
      ;; emacs base colors
      (theme-red        "#ff4200")
      (theme-green      "#00ff00")
      (theme-yellow     "#ffd100")
      (theme-blue       "#0074ff")
      (theme-pink       "#ffbfef")
      ;; font lock palette
      (theme-palette-1  "#5F819D")
      (theme-palette-2  "#aaffaa")
      (theme-palette-3  "#A54242")
      (theme-palette-4  "#666666")
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

   ;; consistency fixes
   `(compilation-mode-line-exit ((t (:inherit (success)))))
   `(compilation-mode-line-run  ((t (:inherit (warning)))))
   `(compilation-mode-line-fail ((t (:inherit (error)))))))
