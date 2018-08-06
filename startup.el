;; startup sequence

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
(load-el "git.el")

(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))
(setenv "PERL5LIB" (concat (getenv "HOME") "/perl5/lib/perl5"))
(setenv "PERL_LOCAL_LIB_ROOT" (concat (getenv "HOME") "/perl5"))
