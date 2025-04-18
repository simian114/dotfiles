(meow-leader-define-key
   ;; window move
   '("w l" . windmove-right)
   '("w h" . windmove-left)
   '("w k" . windmove-up)
   '("w j" . windmove-down)
   ;; split
   '("w v" . split-window-right)       ;; Vertical split
   '("w b" . split-window-below)       ;; Horizontal split
   '("w d" . delete-window)            ;; Delete current window
   '("w o" . delete-other-windows)    ;; Maximize (delete others)
 )

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-window-resize (:hint nil)
    "
Window Resize: _j_ shrink height | _k_ enlarge height | _k_ shrink width | _l_ enlarge width | _q_ quit
"
    ("j" shrink-window)
    ("k" enlarge-window)
    ("h" shrink-window-horizontally)
    ("l" enlarge-window-horizontally)
    ("q" nil :exit t)))

;; Update Meow Leader keymap to include hydra
(meow-leader-define-key
 '("w L" . enlarge-window-horizontally)  ;; Keep for single press
 '("w H" . shrink-window-horizontally)
 '("w K" . enlarge-window)
 '("w J" . shrink-window)
 '("w r" . hydra-window-resize/body))  ;; SPC w r to start hydra
