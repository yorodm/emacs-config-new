(setq BASE_PATH (expand-file-name "~/.emacs.d/elisp/"))
(defun load-config (file)
  (let ((final (file-name-concat BASE_PATH file)))
    (load final)))

(load-config "base.el")
(load-config "base-extensions.el")
(load-config "base-functions.el")
(load-config "base-theme.el")
;;(load-config "ligature.el")
(load-config "lang-python.el")
(load-config "lang-rust.el")
(load-config "lang-go.el")
;(load-config "lang-ocaml.el")
;; (load-config "lang-elixir.el")
;;(load-config "lang-clojure.el")
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
