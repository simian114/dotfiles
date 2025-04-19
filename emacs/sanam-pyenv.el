;; Python virtualenv management for Django projects

;; Required for pyvenv's widget functionality
(require 'wid-edit)

(use-package pyvenv
  :ensure t
  :init
  ;; Display virtual environment in mode line
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  :config
  ;; Enable mode line indicator
  (pyvenv-mode 1)
  ;; Custom variable to store project-venv associations
  (defvar sanam-project-venv-alist '()
    "Alist mapping project root directories to virtualenv paths.")
  
  ;; Save custom venv paths between Emacs sessions
  (defun sanam-save-venv-config ()
    "Save project-venv associations to file."
    (with-temp-file (expand-file-name "venv-config.el" user-emacs-directory)
      (print `(setq sanam-project-venv-alist ',sanam-project-venv-alist) (current-buffer))))
  
  ;; Load saved config if exists
  (let ((venv-config (expand-file-name "venv-config.el" user-emacs-directory)))
    (when (file-exists-p venv-config)
      (load-file venv-config)))
  
  ;; Function to associate venv with current project
  (defun sanam-set-project-venv (venv-path)
    "Associate the current project with a virtualenv at VENV-PATH."
    (interactive "DVirtualenv directory: ")
    (let ((project (project-current nil)))
      (if project
          (let ((root (project-root project)))
            (setq sanam-project-venv-alist 
                  (cons (cons root venv-path)
                        (assoc-delete-all root sanam-project-venv-alist)))
            (sanam-save-venv-config)
            (pyvenv-activate venv-path)
            (message "Set virtualenv for project %s to %s" root venv-path))
        (message "No project detected. Open a project first."))))
  
  ;; Auto-activate venv when switching projects
  (defun sanam-auto-activate-venv ()
    "Activate virtualenv for current project if one is associated."
    (let ((project (project-current nil)))
      (when project
        (let* ((root (project-root project))
               (venv-path (cdr (assoc root sanam-project-venv-alist))))
          (when (and venv-path (file-directory-p venv-path))
            (pyvenv-activate venv-path)
            (message "Activated virtualenv: %s" venv-path))))))
  
  ;; Hook to auto-activate when switching projects
  (add-hook 'project-switch-project-hook #'sanam-auto-activate-venv)
  
  ;; Also activate when finding file in project
  (defun sanam-check-project-venv ()
    "Check if we entered a project with a virtualenv."
    (when (project-current nil)
      (sanam-auto-activate-venv)))
  
  ;; Add hooks to detect project changes
  (add-hook 'find-file-hook #'sanam-check-project-venv)
  (add-hook 'after-init-hook #'sanam-check-project-venv))

;; Django commands
(defun sanam-django-runserver ()
  "Run Django's development server in the current project."
  (interactive)
  (let ((project (project-current nil)))
    (if project
        (let ((root (project-root project)))
          (if (file-exists-p (expand-file-name "manage.py" root))
              (let ((default-directory root))
                (compile "python manage.py runserver"))
            (message "Not a Django project (manage.py not found)")))
      (message "No project detected"))))

(defun sanam-django-shell ()
  "Run Django's shell in the current project."
  (interactive)
  (let ((project (project-current nil)))
    (if project
        (let ((root (project-root project)))
          (if (file-exists-p (expand-file-name "manage.py" root))
              (let ((default-directory root))
                (term "python manage.py shell"))
            (message "Not a Django project (manage.py not found)")))
      (message "No project detected"))))

;; Key bindings for Django commands
(global-set-key (kbd "C-c d r") #'sanam-django-runserver) ; Django runserver
(global-set-key (kbd "C-c d s") #'sanam-django-shell)     ; Django shell
(global-set-key (kbd "C-c d v") #'sanam-set-project-venv) ; Set virtualenv for project

(provide 'sanam-pyenv)
