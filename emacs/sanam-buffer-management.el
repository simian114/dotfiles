;; buffer management
(meow-leader-define-key
 '("b b" . counsel-ibuffer)
 '("b p" . previous-buffer)
 '("b n" . next-buffer)
 '("b d" . kill-current-buffer)
 '("b k" . kill-current-buffer)
 )
 
