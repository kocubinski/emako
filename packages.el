(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; el-get requires trailing /
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes/")

(setq el-packages
      (append '(monokai-emacs
		emacs-async
		helm
                smartparens
		rainbow-delimiters
		company-mode
		neotree
		emacs-leuven-theme
		clojure-mode
		cider
		)))

(el-get 'sync el-packages)
