(let ((home-bin (concat (getenv "HOME") "/bin"))
      (local-bin "/usr/local/bin"))
  (setenv "PATH" (concat (getenv "PATH") (concat ":" home-bin ":" local-bin)))
  (setq exec-path (append exec-path (list home-bin local-bin))))
