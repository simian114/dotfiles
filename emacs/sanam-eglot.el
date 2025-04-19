;; eglot - Built-in LSP client for Emacs (Django fullstack development)

(use-package eglot
  :ensure t
  :hook (;; Python/Django
         (python-mode . sanam-eglot-auto-enable)
         ;; Frontend
         (js-mode . sanam-eglot-auto-enable)
         (typescript-mode . sanam-eglot-auto-enable)
         (web-mode . sanam-eglot-auto-enable)
         (css-mode . sanam-eglot-auto-enable)
         (html-mode . sanam-eglot-auto-enable))
  :config
  ;; Define server lists for each mode - using only one preferred server per type
  (setq sanam-python-lsp-servers '(("pyright-langserver" "--stdio")))
  (setq sanam-js-lsp-servers '(("typescript-language-server" "--stdio")))
  (setq sanam-web-lsp-servers '(("vscode-css-language-server" "--stdio")))
  
  ;; Register servers with eglot
  ;; Python - using only pyright for Django
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  
  ;; JavaScript/TypeScript
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '((web-mode) . ("typescript-language-server" "--stdio")))
  
  ;; HTML/CSS
  (add-to-list 'eglot-server-programs '((css-mode html-mode web-mode) . ("vscode-css-language-server" "--stdio")))
  
  ;; Performance tweaks
  (setq eglot-events-buffer-size 0)
  (setq eglot-autoshutdown t)
  
  ;; Function to check if a command is available
  (defun sanam-command-exists-p (command)
    "Check if COMMAND exists in PATH."
    (executable-find (if (listp command) (car command) command)))
  
  ;; Simpler function to auto-enable eglot
  (defun sanam-eglot-auto-enable ()
    "Auto-enable eglot if the appropriate server is available."
    (let ((server-exists nil)
          (server-name ""))
      
      (cond
       ;; Python - using only pyright
       ((derived-mode-p 'python-mode)
        (setq server-name "pyright-langserver")
        (setq server-exists (sanam-command-exists-p "pyright-langserver")))
       
       ;; JS/TS
       ((or (derived-mode-p 'js-mode) (derived-mode-p 'typescript-mode))
        (setq server-name "typescript-language-server")
        (setq server-exists (sanam-command-exists-p "typescript-language-server")))
       
       ;; Web
       ((or (derived-mode-p 'web-mode) (derived-mode-p 'css-mode) (derived-mode-p 'html-mode))
        (setq server-name "vscode-css-language-server")
        (setq server-exists (sanam-command-exists-p "vscode-css-language-server"))))
      
      (if server-exists
          (eglot-ensure)
        (when server-name
          (message "LSP server '%s' not found. Please install it for %s support." 
                   server-name major-mode))))))

;; Web development modes
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  ;; Django template settings
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (setq web-mode-enable-engine-detection t))

;; TypeScript mode
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  ;; Fix for the flyspell-generic-progmode-verify warning
  (when (not (fboundp 'flyspell-generic-progmode-verify))
    (defun flyspell-generic-progmode-verify ()
      t)))

;; CSS mode enhancements
(use-package css-mode
  :ensure t
  :config
  (setq css-indent-offset 2))

;; Company mode for completions
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))