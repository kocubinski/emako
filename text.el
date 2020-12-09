(defun the-org-mode-hook ()
  (auto-fill-mode 1)
  (set-fill-column 86))

(add-hook 'org-mode-hook #'the-org-mode-hook)

(defun the-markdown-mode-hook ()
  (auto-fill-mode 1)
  (set-fill-column 86))

(add-hook 'markdown-mode-hook #'the-markdown-mode-hook)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scheme . t)))

(use-package org-jira
  :init
  (setq jiralib-url "https://theladders.atlassian.net"))

(setq org-jira-working-dir "~/dev/notebook/ladders-log/org-jira")

(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path "~/.local/bin/plantuml.jar")

(use-package org-make-toc
  :init
  (require 'org-make-toc))

(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode)))
