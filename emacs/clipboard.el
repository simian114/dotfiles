;; clipboard
(setq select-enable-clipboard t)
(setq select-enable-primary t)

(defun copy-to-clipboard (text)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "pbcopy")))
(defun paste-from-clipboard ()
  (shell-command-to-string "pbpaste"))
(setq interprogram-cut-function 'copy-to-clipboard)
(setq interprogram-paste-function 'paste-from-clipboard)(defun copy-to-clipboard (text)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "pbcopy")))
(defun paste-from-clipboard ()
  (shell-command-to-string "pbpaste"))
(setq interprogram-cut-function 'copy-to-clipboard)
(setq interprogram-paste-function 'paste-from-clipboard)

