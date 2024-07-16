(use-package lsp-java
  :straight t
  :bind (("\C-\M-b" . lsp-find-implementation)
         ("M-RET" . lsp-execute-code-action))

  :config
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx1G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         (concat "-javaagent:" (expand-file-name "~/../../.m2/repository/org/projectlombok/lombok/1.18.30/lombok-1.18.30.jar"))
         )

        ;; Don't organise imports on save
        lsp-java-save-action-organize-imports nil

        ;; Currently (2019-04-24), dap-mode works best with Oracle
        ;; JDK, see https://github.com/emacs-lsp/dap-mode/issues/31
        lsp-java-java-path (executable-find "java")
        )
  )

(use-package dap-mode
  :straight t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-register-debug-template
   "um debug"
   (list :type "java"
         :request "attach"
         :hostName "172.18.0.200"
         :port 5005))
  )

(use-package dap-java
  :straight nil
  :ensure nil
  :after (lsp-java)
  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue)
  )

(defun maven-run(args)
  (let*((path (locate-dominating-file default-directory "pom.xml"))
        (compile-command (concat "mvn -f " path " " args)))
    (compile compile-command)))

(defun maven-compile ()
    (interactive)
  (maven-run "clean compile"))

(defun maven-format ()
  (interactive)
  (maven-run "formatter:format"))
