(use-package sql-indent
  :straight t
  :hook (sql-mode . sqlind-minor-mode)
  :config
  (setq sql-use-indent-support t))
(provide 'lang-sql)
