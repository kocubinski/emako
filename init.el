(defun load-el (f)
  (load-file (concat "~/.emacs.d/" f)))

;; --------------------------------------------------------------------------------
;; package.el

;; (setq package-check-signature nil)

;; don't write custom variables here in init.el, put them somewhere we'll never read
(setq custom-file "~/.emacs.d/custom.el")

;; setup MELPA
(require 'package)

(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)

;; when a local package cache is not present create one and install use-package
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; always download packages from MELPA which are not present locally
(setq use-package-always-ensure t)

;; --------------------------------------------------------------------------------
;; display.el
(use-package solarized-theme)

(blink-cursor-mode -1)
(load-theme 'solarized-dark t)
(setq-default cursor-type 'bar)
(global-hl-line-mode 1)

(set-face-attribute 'default nil :font "Dejavu Sans Mono-9")

;; default vertical split?
(setq split-height-threshold nil)

(setq split-width-threshold 90)
;;(setq split-width-threshold 88)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; --------------------------------------------------------------------------------
;; ux.el

;; We use ido
(require 'ido)
(ido-mode t)

;; ido in M-x for certain
(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

;; enable building of recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

;; --------------------------------------------------------------------------------
;; git.el

(use-package magit
  :init
  (global-magit-file-mode 1))

(defun kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (cl-letf (((symbol-function 'kill-buffer-ask)
         (lambda (buffer) (kill-buffer buffer))))
    (kill-matching-buffers regexp)))

(defadvice magit-status (around magit-fullscreen activate)
    "Turn fullscreen on for magit-status."
     (window-configuration-to-register :magit-fullscreen)
     ad-do-it
     (delete-other-windows))

(defun magit-quit-session ()
    "Restore previous window configuration and cleanup buffers."
     (interactive)
     (kill-buffers "^\\*magit")
     (jump-to-register :magit-fullscreen))

(bind-key "q" #'magit-quit-session magit-status-mode-map)


;; --------------------------------------------------------------------------------
;; lisp.el

(use-package smartparens)
(use-package rainbow-delimiters)
(use-package company)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(dolist (x '(scheme emacs-lisp lisp clojure))
  (let ((n (intern (concat (symbol-name x) "-mode-hook"))))
    (add-hook n 'smartparens-mode)
    (add-hook n 'rainbow-delimiters-mode)
    (add-hook n 'company-mode)))

(sp-use-paredit-bindings)
(sp-local-pair 'emacs-lisp-mode "`" "'")
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

(defun live-lisp-describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
   This checks in turn:
     -- for a function name where point is
     -- for a variable name where point is
     -- for a surrounding function call"
          (interactive)
          (let (sym)
            ;; sigh, function-at-point is too clever.  we want only the first half.
            (cond ((setq sym (ignore-errors
                               (with-syntax-table emacs-lisp-mode-syntax-table
                                 (save-excursion
                                   (or (not (zerop (skip-syntax-backward "_w")))
                                       (eq (char-syntax (char-after (point))) ?w)
                                       (eq (char-syntax (char-after (point))) ?_)
                                       (forward-sexp -1))
                                   (skip-chars-forward "`'")
                                   (let ((obj (read (current-buffer))))
                                     (and (symbolp obj) (fboundp obj) obj))))))
                   (describe-function sym))
                  ((setq sym (variable-at-point)) (describe-variable sym)))))

;; --------------------------------------------------------------------------------
;; Clojure

(use-package clojure-mode)
(use-package cider)
(use-package clj-refactor)

;;(setq cider-repl-use-pretty-printing 1)

(setq cider-clojure-cli-global-options
      "-A:test:deps:dev"
      )

(defun the-cider-repl-hook ()
  (smartparens-mode 1)
  (rainbow-delimiters-mode 1)
  (company-mode 1))

(add-hook 'cider-repl-mode-hook #'the-cider-repl-hook)

;; --------------------------------------------------------------------------------
;; keys.el

(global-set-key (kbd "C-,") 'other-window)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(defun mark-current-sexp ()
  (interactive)
  (sp-backward-up-sexp)
  (mark-sexp))

(global-set-key (kbd "C-s-SPC") 'mark-current-sexp)

;; --------------------------------------------------------------------------------
;; misc / should be elsewhere
;; don't create backup files in directory
(setq backup-directory-alist `(("." . "~/.backups")))

(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; kubernetes hacks for ladders
(setenv "KUROSAWA_S3_CONFIG_URI" "s3://ladders-config/local/")
(setenv "CLJ_CONFIG" "/home/mattk/dev/ladders/services")

