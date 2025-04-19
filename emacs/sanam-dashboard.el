;; Dashboard configuration for the Emacs start page
;; use-package with package.el:

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice
        (lambda ()
          (dashboard-refresh-buffer)
          (get-buffer "*dashboard*"))))

(setq dashboard-items '((recents   . 5)
                        (bookmarks . 5)
                        (projects  . 5)
                        (agenda    . 5)
                        (registers . 5)))
