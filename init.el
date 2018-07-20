;; required:
;(package-initialize)

(defun load-el (f)
  (load-file (concat "~/.emacs.d/" f)))

(load-el "startup.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode xref-js2 js2-refactor js2-mode use-package smartparens simple-httpd rainbow-delimiters neotree monokai-theme magit helm-projectile ensime clj-refactor ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
