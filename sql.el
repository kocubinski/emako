(eval-after-load "sql"
  ;;'(load-library "sql-indent")
  '(set-variable 'tab-width 4))

(add-hook 'sql-mode-hook
		  (lambda () (set-variable 'tab-width 4)))
