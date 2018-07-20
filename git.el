(defun kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (cl-letf (((symbol-function 'kill-buffer-ask)
         (lambda (buffer) (kill-buffer buffer))))
    (kill-matching-buffers regexp)))

(defadvice magit-status (around magit-fullscreen activate)
    "Turn fullscreen on for magit-status."
     (window-configuration-to-register :magit-fullscreen)
     ad-do-it
     (delete-other-windows))

(defun magit-quit-session ()
    "Restore previous window configuration and cleanup buffers."
     (interactive)
     (kill-buffers "^\\*magit")
     (jump-to-register :magit-fullscreen))

(bind-key "q" #'magit-quit-session magit-status-mode-map)
