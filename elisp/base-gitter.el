(use-package circe
  :config
  (setq circe-network-options
      '(("Libera"
         :tls t
         :nick "jadex"
         :channels ("#emacs-circe")))))

(provide 'base-gitter)
