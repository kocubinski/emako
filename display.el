(when (display-graphic-p)
   (if (eq system-type 'windows-nt) 
      	 (set-face-attribute 'default nil :font "Consolas-11")
     (set-face-attribute 'default nil :font "Inconsolata-11")
     ))

(blink-cursor-mode -1)
(load-theme 'monokai t)
(setq default-cursor-type 'bar)
(global-hl-line-mode 1)
