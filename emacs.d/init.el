(defun load-el (f)
  (load-file  (expand-file-name f user-emacs-directory)))

;; os x only
(add-to-list 'exec-path "/usr/local/opt/openjdk@8/bin")
(add-to-list 'exec-path	"/usr/local/bin")
(add-to-list 'exec-path (concat (getenv "HOME") "/.local/opt/node/bin"))

(setenv "PATH" (concat "/usr/local/opt/openjdk@8/bin:" (getenv "PATH")))
(setenv "JAVA_HOME" "/usr/local/opt/openjdk@8")

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

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
 package-archive-priorities '(("melpa"        . 5)
			      ("org"          . 4)
			      ("gnu"          . 3)
			      ("melpa-stable" . 1)
			      ))

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
(if (display-graphic-p)
    (load-theme 'solarized-dark t)
  (load-theme 'solarized-wombat-dark t))

(setq-default cursor-type 'bar)
(global-hl-line-mode 1)

(set-face-attribute 'default nil :font "Dejavu Sans Mono-9")
;;(set-face-attribute 'default nil :font "Menlo-9")

;; default vertical split?
(setq split-height-threshold nil)

;; 3 buffers
(setq split-width-threshold 121)

;; 4 buffers
;;(setq split-width-threshold 91)
;;(setq split-width-threshold 80)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; --------------------------------------------------------------------------------
;; ux.el

;;helm
(use-package helm)

(require 'helm-config)
(helm-mode 1)

(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key [(f10)] 'helm-buffers-list)

(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; enable building of recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

;; (use-package smex
;;   :init
;;   (global-set-key (kbd "M-x") 'smex))

;; Projectile
(use-package projectile
  :init
  (progn
    (projectile-mode 1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    ))

(use-package ag)

;; source:
;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-backup.el
;; Excess backup deletion will happen silently, without user confirmation, if
;; `delete-old-versions' is set to `t'.
(setq delete-old-versions t) ; default nil

;; this behavior of closing all windows annoys me to no end
(global-unset-key (kbd "ESC ESC ESC"))

;; --------------------------------------------------------------------------------
;; git.el

(use-package magit
;;  :init
;;  (global-magit-file-mode 1)
  )

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

;;(sp-use-paredit-bindings)
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
;; text.el

(use-package markdown-mode
  :init
  (progn
    (defun the-markdown-mode-hook ()
      (auto-fill-mode 1)
      (set-fill-column 86))
    (add-hook 'markdown-mode-hook #'the-markdown-mode-hook)))

(defun the-org-mode-hook ()
  (auto-fill-mode 1)
  (set-fill-column 86)
  (flyspell-mode)
  (define-key org-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-,") nil))

(add-hook 'org-mode-hook #'the-org-mode-hook)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scheme . t)
   (plantuml . t)))

(setq org-plantuml-jar-path "~/.local/bin/plantuml.jar")

(use-package dockerfile-mode)

;; --------------------------------------------------------------------------------
;; misc / should be elsewhere
;; don't create backup files in directory
(setq backup-directory-alist `(("." . "~/.backups")))

(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; kubernetes hacks for ladders
(setenv "KUROSAWA_S3_CONFIG_URI" "s3://ladders-config/local/")
(setenv "CLJ_CONFIG" (concat (getenv "HOME") "/dev/ladders/services"))

;; Groovy
(defun the-groovy-mode-hook ()
  (custom-set-variables '(groovy-indent-offset 2))
  (rainbow-delimiters-mode-enable)
  (setq indent-tabs-mode nil))

(use-package groovy-mode
    :init
    (add-hook 'groovy-mode-hook #'the-groovy-mode-hook))

(use-package yaml-mode)

(use-package lua-mode)
(use-package terraform-mode)

;;--------------------------------------------------------------------------------
;; javascript.el

(use-package jsonnet-mode)

(defun setup-jsonnet-mode ()
  (rainbow-delimiters-mode 1))

(add-hook 'jsonnet-mode-hook #'setup-jsonnet-mode)

(defun jsonnet-after-save ()
  "Before save hook to format the buffer before each save."
  (when (eq 'jsonnet-mode (symbol-value 'major-mode))
    (jsonnet-format-buffer)))

(defun jsonnet-format-buffer ()
  "Reformat entire buffer using the Jsonnet format utility."
  (interactive)
  (call-process "jsonnetfmt" nil nil nil "--in-place" (buffer-file-name)))

(add-hook 'after-save-hook 'jsonnet-after-save)

(defun my/json-mode-hook ()
   (setq tab-width 2))
(add-hook 'json-mode-hook 'my/js2-mode-hook)

(use-package tide)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  (setq-local company-minimum-prefix-length 1)
  (setq-local company-tooltip-align-annotations t))

;; aligns annotation to the right hand side


;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
