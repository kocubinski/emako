;; backup files
(setq backup-directory-alist `(("." . "~/.backups")))

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; shell script indentation
(setq sh-basic-offset 2)
(setq sh-indentation 2)
