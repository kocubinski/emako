(when (display-graphic-p)
   (if (eq system-type 'windows-nt) 
      	 (set-face-attribute 'default nil :font "Consolas-11")
     ;(set-face-attribute 'default nil :font "Inconsolata-11")
     ;;(set-face-attribute 'default nil :font "Monaco-12")
     ))

(blink-cursor-mode -1)
(load-theme 'solarized-dark t)
(setq-default cursor-type 'bar)
(global-hl-line-mode 1)

(set-face-attribute 'default nil :font "Dejavu Sans Mono-9")
;(set-face-attribute 'default nil :font "Noto Mono-10")
;(set-face-attribute 'default nil :font "Inconsolata-12")
;(set-face-attribute 'default nil :font "Monaco-10")

;; trying to prevent emacs from splitting certain windows
;; https://stackoverflow.com/questions/5151620/how-do-i-make-this-emacs-frame-keep-its-buffer-and-not-get-resized
;; https://superuser.com/questions/582501/is-there-a-way-to-control-which-window-emacs-opens-new-buffers-in/632050#632050

;; I'm not sure why I orginally incorporated this, but the behavior is intolerable.
;;(defadvice pop-to-buffer (before cancel-other-window first)
;;  (ad-set-arg 1 nil))

;;(ad-activate 'pop-to-buffer)

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defun set-local-window-size-fixed ()
  (setq window-size-fixed t))

(defun make-window-sticky ()
  (interactive)
  (toggle-window-dedicated)
  (set-local-window-size-fixed))

(global-set-key (kbd "C-M-s-p") 'make-window-sticky)

;; default vertical split?
(setq split-height-threshold nil)

(setq split-width-threshold 90)
;;(setq split-width-threshold 88)

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_) 
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(global-set-key (kbd "<C-s-return>") 'toggle-maximize-buffer)
