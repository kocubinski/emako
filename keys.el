(global-set-key (kbd "C-,") 'other-window)

;; beautiful vim bindings!

(defun vi-yank-line ()
  (let ((start (point)))
    (move-beginning-of-line 1)
    (let ((beg (point)))
      (move-end-of-line 1)
      (kill-ring-save beg (point)))
    (goto-char start)))

(defun vi-paste-line ()
  (interactive)
  (move-beginning-of-line 1)
  (yank)
  (newline))

(defun vi-kill-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (kill-line))

(defun vi-new-line ()
  (interactive)
  (move-beginning-of-line 1)
  (split-line))

(defun return (fn)
  (funcall fn)
  (hydra-vi/body))

(defhydra hydra-vi-delete (:color blue)
  ("d" (return 'vi-kill-line) "line")
  ("w" (return '(lambda () (kill-word 1))) "word"))

(defhydra hydra-vi-change (:color blue)
  ("d" vi-kill-line "line")
  ("w" (kill-word 1) "word"))

(defhydra hydra-vi-yank (:color blue)
  ("y" (return 'vi-yank-line) "line")
  )

(defhydra hydra-vi (global-map "C-j")
  "vi mode"
  ("l" forward-char "forward")
  ("f" forward-char "forward")
  ("h" backward-char "back")
  ("w" forward-word "forward word")
  ("b" backward-word "back word")
  ("j" next-line "down line")
  ("k" previous-line "up line")
  ("0" move-beginning-of-line "home")
  ("a" move-beginning-of-line "home")
  ("e" move-end-of-line "end")

  ("O" split-line "new line")
  ("p" vi-paste-line "paste-line")
  ("x" delete-char "delete")
  ("u" undo "undo")
  
  ("d" hydra-vi-delete/body "delete" :exit t)
  ("y" hydra-vi-yank/body "yank" :exit t)
  ("c" hydra-vi-change/body "change" :exit t)
  
  ("i" nil "quit"))


