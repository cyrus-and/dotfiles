(defun zoom ()
  (interactive)
  ;; check if should actually re-layout
  (unless (or (not zoom-mode) ; TODO Allow resize alone
              (window-minibuffer-p)
              (one-window-p))
    ;; temporarily disables this mode during resize to avoid infinite recursion
    (let ((zoom-mode nil)
          ;; values are computed to be large enough to always impact the third
          ;; window in a 3-split scenario; in this way windows are automatically
          ;; balanced bu `window-resize` (the 1 is to surpass such threshold)
          (min-width (- (* 2 (ceiling (/ (frame-width) 3.0))) window-min-width -1))
          (min-height (- (* 2 (ceiling (/ (frame-height) 3.0))) window-min-height -1)))
      ;; start from a balanced layout
      (balance-windows)
      ;; then resize the focused window
      (window-resize nil (max (- min-width (window-total-width)) 0) t)
      (window-resize nil (max (- min-height (window-total-height)) 0) nil)
      ;; scroll al the way to the left border (if the window is wide enough to
      ;; contain it) otherwise scroll to center the point
      (scroll-right (window-hscroll))
      (if (> (current-column) (- (window-total-width) hscroll-margin))
          (scroll-left (- (current-column) (/ (window-total-width) 2)))))))

(defun zoom--hook-handler (&rest ignore)
  (zoom))

(defun zoom--enable ()
  (add-hook 'window-configuration-change-hook 'zoom--hook-handler)
  (advice-add 'select-window :after 'zoom--hook-handler)
  (zoom))

(defun zoom--disable ()
  (remove-hook 'window-configuration-change-hook 'zoom--hook-handler)
  (advice-remove 'select-window 'zoom--hook-handler)
  (balance-windows)
  )

;;;###autoload
(define-minor-mode zoom-mode
  "Fixed and automatic balanced window
layout with priority to the focused window."

  :global t
  :lighter " Z"
  (if zoom-mode
      (zoom--enable)
    (zoom--disable)))

(provide 'zoom-mode)
