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

;; not canonical -- but I want to keep all my packages imports in one place.

;;(use-package leuven-theme)
(use-package monokai-theme)
(use-package async)
(use-package helm)
(use-package smartparens)
(use-package rainbow-delimiters)
(use-package company)
(use-package neotree)
(use-package clojure-mode)
(use-package cider)

(use-package ag)

;; scala
(use-package scala-mode)
(use-package ensime)


