;;(package-initialize)

(defun load-el (f)
  (load-file (concat "~/.emacs.d/" f)))

(load-el "common.el")
(load-el "melpa-packages.el")
(load-el "display.el")
(load-el "helm.el")
(load-el "lisp.el")
(load-el "misc.el")
(load-el "keys.el")
(load-el "sql.el")
(load-el "scala.el")
(load-el "osx.el")
(load-el "javascript.el")

(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))
(setenv "PERL5LIB" (concat (getenv "HOME") "/perl5/lib/perl5"))
(setenv "PERL_LOCAL_LIB_ROOT" (concat (getenv "HOME") "/perl5"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (xref-js2 js2-refactor js2-mode use-package smartparens simple-httpd rainbow-delimiters neotree monokai-theme magit helm-projectile ensime clj-refactor ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
