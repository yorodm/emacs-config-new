(use-package company
  :bind (:map company-mode-map
			  ("C-c TAB" . company-complete))
  :config
  (global-company-mode))

(use-package company-quickhelp          ; Documentation popups for Company
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(use-package fd-dired)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-log-io nil)
  (setq lsp-idle-delay 0.500)
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package dap-mode
  :defer t
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-flycheck-enable nil
        lsp-ui-imenu-enable nil
		lsp-ui-sideline-ignore-duplicate t))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))



(use-package helpful
  :bind
  ("C-h k" . helpful-key)
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h C" . helpful-command)
  ("C-h F" . helpful-function)
  (:map emacs-lisp-mode-map
        ("C-c C-d" . helpful-at-point))

  :custom
  (helm-describe-function-function 'helpful-function)
  (helm-describe-variable-function 'helpful-variable)

  :config
  ;; TODO: Follow up on this workaround for the removal of the
  ;; `read-symbol-positions-list' variable in Emacs 29.x. More details:
  ;; https://github.com/Wilfred/elisp-refs/issues/35
  (when (not (version< emacs-version "29.0"))
    (defvar read-symbol-positions-list nil))

  ;; TODO: Follow up this workaround required due to `help-fns--autoloaded-p'
  ;; being changed to only accept a single argument in Emacs 29.x in commit:
  ;; https://github.com/emacs-mirror/emacs/commit/1d1b664fbb9232aa40d8daa54a689cfd63d38aa9
  ;; (defun helpful--autoloaded-p (sym _buf)
  ;;   "Return non-nil if function SYM is autoloaded."
  ;;   (help-fns--autoloaded-p sym))
  )

(use-package exec-path-from-shell
  :config
  ;; Add GOPATH to shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  ;;	(exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck)

(use-package paradox
  :config
  (paradox-enable))

(defun bnb/buffer-backward-kill-dwim (&optional arg)
  (interactive "p")
  (cond ((looking-back "/") (backward-kill-sexp))
        (t (delete-backward-char 1))))

(use-package prescient
  :config
  (setq completion-styles '(prescient basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  :config
  (use-package vertico-prescient)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  ;; Configure directory extension.
  (use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; Persist history over Emacs restarts. Vertico sorts by history position.


(use-package company-prescient)

(use-package consult
  :config
  (use-package recentf)
  :bind (("C-x M-:" . consult-complex-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-keep-lines)
         ("C-c C-k" . consult-flush-lines)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
		 ("C-s" . consult-line)          ;; Alternative for swiper
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
         ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
         ("M-g r" . consult-git-grep)      ;; or consult-grep, consult-ripgrep
         ("M-g f" . consult-find)          ;; or consult-locate, my-fdfind
         ("M-g i" . consult-project-imenu) ;; or consult-imenu
         ;; ("M-g e" . consult-error)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)))

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :init (marginalia-mode)

  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :bind (:map minibuffer-local-completion-map
              ("C-i" . marginalia-cycle)))

(use-package display-line-numbers
  :ensure nil
  :config
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers 'relative))

(use-package magit
  :config
  (setq magit-log-margin '(t "%Y-%m-%dT%H:%M:%S" magit-log-margin-width t 18))
  ;; (setq magit-refresh-status-buffer nil)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (setq auto-revert-buffer-list-filter
		'magit-auto-revert-repository-buffer-p)
  (use-package magit-popup)
  (use-package magit-gitflow
	:after (magit)
	:config
	(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
	(add-hook 'git-commit-mode-hook 'turn-on-auto-fill))


  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))



(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("\s*#\\+BEGIN_SRC" . "\s*#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("\s*#\\+begin_src" . "\s*#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("\s*#\\+BEGIN_EXAMPLE" . "\s*#\\+END_EXAMPLE"))
  (add-to-list 'ispell-skip-region-alist '("\s*#\\+begin_example" . "\s*#\\+end_example")))


(use-package page-break-lines)

(use-package projectile
  :bind (:map projectile-mode-map
			  ("C-c p" . projectile-command-map))
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))
  (add-to-list 'projectile-project-root-files "package.json")
  (add-to-list 'projectile-project-root-files "go.mod")
  (add-to-list 'projectile-project-root-files "__manifest__.py")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories ".cargo")
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm install"
                                    :test "npm test"
                                    :run "npm start"
                                    :test-suffix ".spec")
  (setq projectile-indexing-method 'alien)
  (setq projectile-generic-command "fd -H --ignore-file .projectile -t f -0")
  ;;(setq projectile-completion-system 'ivy)
  (setq projectile-git-submodule-command nil)
  (projectile-global-mode))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package dired-single
  :config
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^" 'dired-single-up-directory)
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq dired-garbage-files-regexp "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyc\\)\\)\\'")
  (setq dired-dwim-target 't))

(use-package direnv
 :config
 (direnv-mode 1))

(use-package deadgrep
  :defer 15)

(use-package yasnippet
  :config
  (use-package yasnippet-snippets
	:defer 15)
  (yas-global-mode 1))

(use-package know-your-http-well
  :defer 15)

(use-package git
  :defer 15)

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package company-restclient))


(use-package yaml-mode
  :defer 15)

(use-package indent-guide
  :after (yaml-mode)
  :hook (yaml-mode . indent-guide-mode))


(use-package dockerfile-mode
  :defer 10)

(use-package tree-sitter
  :config
  (use-package tree-sitter-langs)
  (global-tree-sitter-mode))

(use-package pass)
(use-package format-all
  :pin melpa)
;(load-file "~/sources/hugo-blog-mode/hugo-blog-mode.el")
;(setq hugo-blog-project (expand-file-name "~/sources/yorodm-site/"))


(provide 'base-extensions)
