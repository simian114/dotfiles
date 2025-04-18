;; magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setq auth-sources '("~/.authinfo"))

(use-package forge
  :ensure t
  :after magit)

