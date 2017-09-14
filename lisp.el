(require 'clj-refactor)

;; general lisp

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

;; clojure

(use-package clj-refactor
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode))

(defun the-cider-repl-hook ()
  (smartparens-mode 1)
  (company-mode 1))

(defun the-cider-mode-hook ()
  (company-mode 1))

(defun the-clojure-mode-hook ()
  (magit-file-mode 1))

(add-hook 'cider-repl-mode-hook #'the-cider-repl-hook)
(add-hook 'cider-mode-hook #'the-cider-mode-hook)
(add-hook 'clojure-mode-hook #'the-clojure-mode-hook)

(setq cider-repl-use-pretty-printing 1)
