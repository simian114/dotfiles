;; project.el configuration (built-in project management)

;; Enable project.el (built-in since Emacs 26.1)
(require 'project)

;; Configure project.el
(setq project-switch-commands 'project-dired)

;; Create a project.el project list file
(setq project-list-file (locate-user-emacs-file "projects"))

;; To manually register folders as projects:
;; Use project-remember-project to add the current directory
;; or use this function to add specific directories:
(defun sanam-register-project (dir)
  "Register DIR as a project in project.el"
  (interactive "DDirectory: ")
  (let ((dir (expand-file-name dir)))
    (project-remember-project (cons 'transient dir))))

;; Register example projects (customize these with your actual folders)
;; Uncomment and modify these lines to add specific projects
;; (sanam-register-project "~/path/to/project-A")
;; (sanam-register-project "~/path/to/project-B")

;; Automatically discover projects in specific folders
(when (file-directory-p "~/Projects")
  (project-remember-projects-under "~/Projects" t))

;; Key bindings for project.el (C-x p prefix by default)
;; - To switch between projects: C-x p p
;; - To find file in project: C-x p f
;; - To search in project: C-x p g (grep)
;; - To run eshell in project: C-x p e
;; - To manage project buffers: C-x p b

;; Add some additional useful keybindings
(global-set-key (kbd "C-x p p") #'project-switch-project) ; Switch projects
(global-set-key (kbd "C-x p f") #'project-find-file)      ; Find file in project
(global-set-key (kbd "C-x p d") #'project-find-dir)       ; Find directory in project
(global-set-key (kbd "C-x p g") #'project-find-regexp)    ; Search in project (grep)
(global-set-key (kbd "C-x p b") #'project-switch-to-buffer) ; Switch to project buffer
(global-set-key (kbd "C-x p k") #'project-kill-buffers)   ; Kill project buffers
(global-set-key (kbd "C-x p e") #'project-eshell)         ; Open eshell in project