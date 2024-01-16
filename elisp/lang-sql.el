(use-package sql-indent
  :pin gnu
  :hook (sql-mode . sqlind-minor-mode)
  :config
  (setq sql-use-indent-support t))
(provide 'lang-sql)
