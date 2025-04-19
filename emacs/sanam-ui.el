
;;; ----- Appearance -----

(defun dw/set-terminal-title (title)
  (send-string-to-terminal (format "\e]0;%s\a" title)))

(defun dw/clear-background-color (&optional frame)
  (interactive)
  (or frame (setq frame (selected-frame)))
  "unsets the background color in terminal mode"
  (unless (display-graphic-p frame)
    ;; Set the terminal to a transparent version of the background color
    (send-string-to-terminal
     (format "\033]11;[90]%s\033\\"
         (face-attribute 'default :background)))
    (set-face-background 'default "unspecified-bg" frame)))

;; Clear the background color for transparent terminals
(unless (display-graphic-p)
  (add-hook 'after-make-frame-functions 'dw/clear-background-color)
  (add-hook 'window-setup-hook 'dw/clear-background-color)
  (add-hook 'ef-themes-post-load-hook 'dw/clear-background-color))

(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :font "JetBrains Mono"
                      :weight 'normal
                      :height 180)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font "JetBrains Mono"
                      :weight 'normal
                      :height 180)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "Fira Code Retina"
                      :height 160
                      :weight 'normal)

  ;; Make frames transparent
  (set-frame-parameter (selected-frame) 'alpha-background 93)
  (add-to-list 'default-frame-alist '(alpha-background . 93))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))


;; Make vertical window separators look nicer in terminal Emacs
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

;; Clean up the mode line
(setq-default mode-line-format
              '("%e" "  "
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                mode-line-format-right-align
                "  "
                (project-mode-line project-mode-line-format)
                " "
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                "  ")
              project-mode-line t
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '(" %l:%c"))

(use-package emacs-solo-rainbow-delimiters
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/rainbow-delimiters ()
    "Apply simple rainbow coloring to parentheses, brackets, and braces in the current buffer.
Opening and closing delimiters will have matching colors."
    (interactive)
    (let ((colors '(font-lock-keyword-face
                    font-lock-type-face
                    font-lock-function-name-face
                    font-lock-variable-name-face
                    font-lock-constant-face
                    font-lock-builtin-face
                    font-lock-string-face
                    )))
      (font-lock-add-keywords
       nil
       `((,(rx (or "(" ")" "[" "]" "{" "}"))
          (0 (let* ((char (char-after (match-beginning 0)))
                    (depth (save-excursion
                             ;; Move to the correct position based on opening/closing delimiter
                             (if (member char '(?\) ?\] ?\}))
                                 (progn
                                   (backward-char) ;; Move to the opening delimiter
                                   (car (syntax-ppss)))
                               (car (syntax-ppss)))))
                    (face (nth (mod depth ,(length colors)) ',colors)))
               (list 'face face)))))))
    (font-lock-flush)
    (font-lock-ensure))

  (add-hook 'prog-mode-hook #'emacs-solo/rainbow-delimiters))
