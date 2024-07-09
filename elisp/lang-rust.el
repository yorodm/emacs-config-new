;; rust-mode, racer, cargo

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rustic
  :straight t
  :config
  (setq lsp-inlay-hint-enable t))
(provide 'lang-rust)
