;;; sanam-dired.el --- Dired configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for dired and dired-subtree

;;; Code:

(use-package dired
  :ensure nil  ;; built-in package
  :config
  ;; Use human-readable file sizes
  (setq dired-listing-switches "-alh")
  
  ;; Allow dired to delete or copy directory
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  
  ;; Auto refresh dired when file changes
  (setq dired-auto-revert-buffer t))


;; icons
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


(use-package dired-subtree
  :ensure t
  :after dired
  :config
  ;; Set a reasonable max depth to prevent performance issues
  (setq dired-subtree-max-depth 6)
  
  ;; Use different background colors for different subtree depths
  (setq dired-subtree-use-backgrounds t)
  
  :bind (:map dired-mode-map
         ;; Use C-, as prefix as suggested in the documentation
         ("C-," . nil) ;; Unset any existing binding
         ("C-, i" . dired-subtree-insert)
         ("C-, C-i" . dired-subtree-insert)
         ("C-, r" . dired-subtree-remove)
         ("C-, C-r" . dired-subtree-remove)
         ("C-, a" . dired-subtree-apply-filter)
         ("C-, C-a" . dired-subtree-apply-filter)
         ("C-, c" . dired-subtree-cycle)
         ("C-, C-c" . dired-subtree-cycle)
         ("C-, m" . dired-subtree-mark-subtree)
         ("C-, C-m" . dired-subtree-mark-subtree)
         ("C-, u" . dired-subtree-unmark-subtree)
         ("C-, C-u" . dired-subtree-unmark-subtree)
         ("C-, n" . dired-subtree-next-sibling)
         ("C-, C-n" . dired-subtree-next-sibling)
         ("C-, p" . dired-subtree-previous-sibling)
         ("C-, C-p" . dired-subtree-previous-sibling)))

;; Make sure we also load dired-subtree in sanam-init.el
(provide 'sanam-dired)
;;; sanam-dired.el ends here
