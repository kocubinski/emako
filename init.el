(defun load-el (f)
  (load-file (concat "~/.emacs.d/" f)))

(load-el "common.el")
(load-el "packages.el")
(load-el "display.el")
(load-el "helm.el")
(load-el "lisp.el")
(load-el "misc.el")
(load-el "keys.el")

(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))
(setenv "PERL5LIB" (concat (getenv "HOME") "/perl5/lib/perl5"))
(setenv "PERL_LOCAL_LIB_ROOT" (concat (getenv "HOME") "/perl5"))
