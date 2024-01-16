;;; package --- python configs
;;; Commentary:
;;; Contains my python configs


(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8)

(use-package pyvenv
  :config
  (add-hook 'python-mode-hook #'pyvenv-mode))
(setenv "WORKON_HOME" (expand-file-name "~/Documents/source/python/venvs"))

(defun update-python-variables ()
  (setq eglot--lsp-python (executable-find "python"))
  (message "Python path updated. Restart (eglot | lsp) if needed"))
(add-hook 'python-mode-hook #'flycheck-mode)
(when (or (fboundp 'eglot) (fboundp 'lsp))
  (add-hook 'pyvenv-post-activate-hooks #'update-python-variables))
(provide 'lang-python)
;;; base-python.el ends here
