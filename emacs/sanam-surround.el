;; Vim-surround style functionality for meow
;; Provides key bindings for surrounding text with pairs of characters

;; Explicitly load meow package
(require 'meow)
(eval-when-compile
  (require 'meow))

(defun my-meow-surround-region (char)
  "Surround the selected region or word with CHAR."
  (interactive "cSurround with: ")
  (message "my-meow-surround-region called with char: %s" char)
  (let ((surround (string char))
        (has-selection (region-active-p))
        start end)
    (if has-selection
        ;; 선택된 영역 감싸기
        (progn
          (setq start (region-beginning)
                end (region-end))
          (goto-char end)
          (insert surround)
          (goto-char start)
          (insert surround)
          (meow-cancel-selection))
      ;; 단어 감싸기
      (progn
        ;; meow-inner-of-thing으로 단어 선택
        (meow-inner-of-thing ?w) ; 단어 선택
        (setq start (region-beginning)
              end (region-end))
        (goto-char end)
        (insert surround)
        (goto-char start)
        (insert surround)
        (meow-cancel-selection)))))

(defun my-find-surrounding-pair (open close)
  "Find the closest surrounding pair of characters around point."
  (let ((p (point))
        (open-pos nil)
        (close-pos nil))
    ;; Search backward for opening character
    (save-excursion
      (when (search-backward open nil t)
        (setq open-pos (point))))
    
    ;; Search forward for closing character
    (save-excursion
      (when (search-forward close nil t)
        (setq close-pos (1- (point)))))
    
    (when (and open-pos close-pos)
      (cons open-pos close-pos))))

(defun my-meow-change-surround (old-open old-close new-open new-close)
  "Change surrounding characters OLD-OPEN, OLD-CLOSE to NEW-OPEN, NEW-CLOSE."
  (interactive "cOld opening char: \ncOld closing char: \ncNew opening char: \ncNew closing char: ")
  (message "my-meow-change-surround: %c%c → %c%c" old-open old-close new-open new-close)
  (let ((old-open-str (string old-open))
        (old-close-str (string old-close))
        (new-open-str (string new-open))
        (new-close-str (string new-close)))
    
    (let ((pair (my-find-surrounding-pair old-open-str old-close-str)))
      (when pair
        (save-excursion
          ;; First replace closing character (to avoid position changes)
          (goto-char (cdr pair))
          (delete-char 1)
          (insert new-close-str)
          
          ;; Then replace opening character
          (goto-char (car pair))
          (delete-char 1)
          (insert new-open-str))))))

(defun my-meow-delete-surround (open close)
  "Delete surrounding characters OPEN and CLOSE."
  (interactive "cOpening char to delete: \ncClosing char to delete: ")
  (message "my-meow-delete-surround: %c%c" open close)
  (let ((open-str (string open))
        (close-str (string close)))
    
    (let ((pair (my-find-surrounding-pair open-str close-str)))
      (when pair
        (save-excursion
          ;; First delete closing character (to avoid position changes)
          (goto-char (cdr pair))
          (delete-char 1)
          
          ;; Then delete opening character
          (goto-char (car pair))
          (delete-char 1))))))

;; For convenience, provide single-character versions for matching pairs
(defun my-meow-change-surround-single (old-char new-char)
  "Change surrounding characters when OLD-CHAR is used for both opening and closing."
  (interactive "cOld surround: \ncNew surround: ")
  (my-meow-change-surround old-char old-char new-char new-char))

(defun my-meow-delete-surround-single (char)
  "Delete surrounding characters when CHAR is used for both opening and closing."
  (interactive "cSurround to delete: ")
  (my-meow-delete-surround char char))

;; Common matched pairs helper function
(defun my-get-matching-pair (char)
  "Get the matching pair for a character."
  (pcase char
    (?\( ?\))
    (?\) ?\()
    (?\[ ?\])
    (?\] ?\[)
    (?\{ ?\})
    (?\} ?\{)
    (?\< ?\>)
    (?\> ?\<)
    (_ char)))  ; Default to same char for quotes etc.

(defun my-meow-change-surround-matched (old-char new-char)
  "Change matched surrounding pair characters where OLD-CHAR is the opening character."
  (interactive "cOld opening char: \ncNew opening char: ")
  (let ((old-close (my-get-matching-pair old-char))
        (new-close (my-get-matching-pair new-char)))
    (my-meow-change-surround old-char old-close new-char new-close)))

(defun my-meow-delete-surround-matched (char)
  "Delete matched surrounding pair characters where CHAR is the opening character."
  (interactive "cOpening char to delete: ")
  (let ((close-char (my-get-matching-pair char)))
    (my-meow-delete-surround char close-char)))

;; Set up leader key bindings with prefix 'y' for surround operations
(meow-leader-define-key
 '("y s" . my-meow-surround-region)
 '("y c" . my-meow-change-surround-single)
 '("y C" . my-meow-change-surround-matched)
 '("y d" . my-meow-delete-surround-single)
 '("y D" . my-meow-delete-surround-matched))

;; Separate binding for surround in beacon mode
(define-key meow-beacon-state-keymap (kbd "SPC y s") 'my-meow-surround-region)

;; Add surround functions to normal mode keybindings
(meow-normal-define-key
 '("S" . my-meow-surround-region)      ;; Surround region/word with character
 '("C" . my-meow-change-surround-single) ;; Change surrounding characters (same open/close)
 '("M-c" . my-meow-change-surround-matched) ;; Change surrounding characters (matched pairs)
 '("D" . my-meow-delete-surround-single) ;; Delete surrounding characters (same open/close)
 '("M-d" . my-meow-delete-surround-matched)) ;; Delete surrounding characters (matched pairs)