(defun the-org-mode-hook ()
  (auto-fill-mode 1)
  (set-fill-column 86))

(add-hook 'org-mode-hook #'the-org-mode-hook)

(defun the-markdown-mode-hook ()
  (auto-fill-mode 1)
  (set-fill-column 86))

(add-hook 'markdown-mode-hook #'the-markdown-mode-hook)
