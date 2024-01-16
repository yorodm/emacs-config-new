(use-package go-mode
  :bind
  :config
  (use-package go-dlv)
  ;; Call Gofmt before saving
  )


(defun setup-go-mode-compile ()
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(provide 'lang-go)
