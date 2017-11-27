(use-package js2-mode
  :config
  ((lambda ()
     (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
     (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2))))))

(use-package js2-refactor
  :config
  ((lambda ()
     (add-hook 'js2-mode-hook #'js2-refactor-mode)
     (js2r-add-keybindings-with-prefix "C-c C-r")
     (define-key js2-mode-map (kbd "C-k") #'js2r-kill))))

(use-package xref-js2
  :config
  ((lambda ()
     ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
     ;; unbind it.
     (define-key js-mode-map (kbd "M-.") nil)
     (add-hook 'js2-mode-hook
	       (lambda ()
		 (add-hook 'xref-backend-functions
			   #'xref-js2-xref-backend
			   nil t))))))

(use-package company-tern
  :config
  ((lambda ()
     (add-to-list 'company-backends 'company-tern)
     (add-hook 'js2-mode-hook
	       (lambda ()
		 (tern-mode)
		 (company-mode)
		 ;; Disable completion keybindings, as we use xref-js2 instead
		 (define-key tern-mode-keymap (kbd "M-.") nil)
		 (define-key tern-mode-keymap (kbd "M-,") nil))))))