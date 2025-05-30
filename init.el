
(defun load-config (file)
    (load (locate-user-emacs-file (concat "elisp/" file))))

(load-config "base.el")
(load-config "base-extensions.el")
(load-config "base-functions.el")
(load-config "base-theme.el")
(load-config "lang-python.el")
(load-config "lang-rust.el")
(load-config "lang-go.el")
(load-config "lang-java.el")
(load-config "lang-nix.el")
