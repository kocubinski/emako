
;; MELPA sources

(require 'package)

(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

;; https://github.com/jwiegley/use-package
(require 'use-package)

(setq use-package-always-ensure t)

;; not idiomatic -- but I want to keep all my packages imports in one place.

(use-package solarized-theme)
(use-package monokai-theme)
(use-package async)
(use-package helm)
(use-package smartparens)
(use-package rainbow-delimiters)
(use-package company)
(use-package neotree)
(use-package clojure-mode)
(use-package cider
  :ensure t
  :pin melpa-stable)
(use-package clj-refactor)
(use-package hydra)
(use-package ag)
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(use-package projectile)
(use-package helm-projectile)
(use-package magit
  :init
  (global-magit-file-mode 1))
(use-package simple-httpd)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package dockerfile-mode)

;; scala
(use-package scala-mode)

;; use melpa-stable for scala 2.11.8
;; the unstable (dev) version is available on melpa
(use-package ensime
  :pin melpa)

(use-package terraform-mode)

(use-package exec-path-from-shell)

(defun the-groovy-mode-hook ()
  (custom-set-variables '(groovy-indent-offset 2))
  (rainbow-delimiters-mode-enable)
  (setq indent-tabs-mode nil))

(use-package groovy-mode
    :init
    (add-hook 'groovy-mode-hook #'the-groovy-mode-hook))

(use-package json-mode)
